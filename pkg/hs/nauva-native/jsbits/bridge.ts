interface Empty {}

declare const React: {
    Component: {
        new(...args: any[]): Empty;
    };
    createElement: any;
    createClass: any;
};
declare const ReactDOM: {
    render(element, container: HTMLElement): void;
};


class Context {
    fn: { [id: string]: { [path: string]: any } } = {};
}

class ClientH {
    rafId: number = undefined;
    componentRegistry: Map<ComponentId, any> = new Map;
    rootContext = new Context;

    components: Map<string, any> = new Map;

    constructor
        ( public appE: HTMLElement
        , public dispatchComponentEvent: any
        , public dispatchNodeEvent: any
        , public attachRef: any
        , public detachRef: any
        , public componentDidMount: any
        , public componentWillUnmount: any
        ) {}

    renderSpine(spine: any): void {
        if (this.rafId !== undefined) {
            cancelAnimationFrame(this.rafId);
        }

        this.rafId = requestAnimationFrame(() => {
            // const spine = JSON.parse(json);
            // console.time('spineToReact');
            const rootElement = spineToReact(this, [], this.rootContext, spine, undefined);
            // console.timeEnd('spineToReact');

            // console.time('ReactDOM.render');
            ReactDOM.render(rootElement, this.appE);
            // console.timeEnd('ReactDOM.render');

            this.rafId = undefined;
        });
    }

    renderSpineAtPath(key: string, spine: any): void {
        const component = this.components.get(key);
        // console.log('renderSpineAtPath', key, spine, component);
        if (component) {
            component.setState({ spine });
        }
    }
}


type ComponentId = number;

function getComponent(clientH: ClientH, componentId: ComponentId) {
    let component = clientH.componentRegistry.get(componentId);
    if (component === undefined) {
        component = class extends React.Component {
            props: {
                clientH: ClientH;
                path: any[];
                hooks: any;
                spine: any;
                key: string;
            };

            state: { spine: any };
            setState: any;

            ctx = new Context;

            constructor(props) {
                super(props);
                this.state = { spine: props.spine};
            }

            componentDidMount() {
                const {clientH, path, spine: {eventListeners}} = this.props;
                clientH.components.set(path.join('.'), this);
                console.log('Registering component at key', path.join('.'));
                clientH.componentDidMount(path);

                eventListeners.forEach(([fid, name]) => {
                    window.addEventListener(name, getFn(this.ctx, path, fid, () => {
                        return ev => {
                            clientH.dispatchComponentEvent(path, fid, ev);
                        };
                    }));
                })
            }

            componentWillReceiveProps(nextProps) {
                this.setState({ spine: nextProps.spine });
            }

            componentWillUnmount() {
                const {clientH, path, spine: {eventListeners}} = this.props;
                clientH.componentWillUnmount(path);

                eventListeners.forEach(([fid, name]) => {
                    window.removeEventListener(name, getFn(this.ctx, path, fid, () => {
                        return () => undefined;
                    }));
                });

                clientH.components.delete(path.join('.'));
            }

            render() {
                const {clientH, path, key} = this.props;
                const {spine} = this.state;
                return spineToReact(clientH, path, this.ctx, spine.spine, key);
            }
        };

        clientH.componentRegistry.set(componentId, component);
    }

    return component;
}

function capitalizeFirstLetter(string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
}


class ControlledInput extends React.Component {
    props: any;
    state: any;
    setState: any;
    onChange: any;

    constructor(props) {
        super(props);

        this.state = { value: props.props.value || '' };
        this.onChange = ev => {
            this.setState({ value: ev.target.value });
            if (this.props.props.onChange) {
                this.props.props.onChange(ev);
            }
        };
    }
    componentWillReceiveProps(nextProps) {
        if (nextProps.props.value !== this.state.value) {
            this.setState({ value: nextProps.props.value });
        }
    }
    render() {
        return React.createElement(this.props.elementType, Object.assign({},
            this.props.props, { value: this.state.value, onChange: this.onChange }
        ), ...(this.props.children || []));
    }
}

function getFn(ctx: Context, path, fid, mkFn) {
    const pathCtx = ctx.fn[path] !== undefined
        ? ctx.fn[path]
        : (ctx.fn[path] = {});

    return pathCtx[fid] !== undefined
        ? pathCtx[fid]
        : (pathCtx[fid] = mkFn());
}

function spineToReact(clientH: ClientH, path, ctx: Context, spine, key) {
    if (typeof spine === 'string') {
        return spine;

    } else if (spine.type === 'Node') {
        const children = spine.children.map(([index, child]) =>
            spineToReact(clientH, [].concat(path, index), ctx, child, index)
        );

        const props: any = { key, style: spine.style };
        spine.eventListeners.forEach(([fid, name]) => {
            props[`on${capitalizeFirstLetter(name)}`] = getFn(ctx, path, fid, () => {
                console.log('getFn', fid, name);
                return ev => {
                    clientH.dispatchNodeEvent(path, fid, ev);
                };
            });
        });

        for (const [p,v] of spine.attributes) {
            props[p] = v;
        }

        if (spine.ref) {
            props.ref = getFn(ctx, path, 'ref', () => {
                return ref => {
                    if (ref === null) {
                        clientH.detachRef(path);
                    } else {
                        clientH.attachRef(path, ref);
                    }
                };
            });
        }

        if (spine.tag === 'input') {
            return React.createElement(ControlledInput, {
                elementType: 'input', props: props
            }, ...children);
        } else {
            return React.createElement(spine.tag, props, ...children);
        }

    } else if (spine.type === 'Component') {
        return React.createElement(getComponent(clientH, spine.id), {
            clientH, key, path, spine
        });

    } else {
        throw new Error('spineToReact: unexpected value: ' + spine);
    }
}


window['newBridge'] = function(appE, callbacks) {
    return new ClientH
        ( appE
        , callbacks.componentEvent
        , callbacks.nodeEvent
        , callbacks.attachRef
        , callbacks.detachRef
        , callbacks.componentDidMount
        , callbacks.componentWillUnmount
        );
}
