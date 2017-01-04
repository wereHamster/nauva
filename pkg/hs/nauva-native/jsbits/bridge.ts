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
        if (component) {
            component.setState({ spine });
        }
    }
}


type ComponentId = number;

function getComponent(clientH: ClientH, componentId: ComponentId, displayName: string) {
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

        component.displayName = displayName;

        clientH.componentRegistry.set(componentId, component);
    }

    return component;
}

function capitalizeFirstLetter(string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
}


const ControlledInput = (() => {
    let ci = undefined;

    function mkControlledInput() {
        ci = class ControlledInput extends React.Component {
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
    }

    return () => {
        if (ci === undefined) {
            mkControlledInput();
        }

        return ci;
    }
})();

function getFn(ctx: Context, path, fid, mkFn) {
    const pathCtx = ctx.fn[path] !== undefined
        ? ctx.fn[path]
        : (ctx.fn[path] = {});

    return pathCtx[fid] !== undefined
        ? pathCtx[fid]
        : (pathCtx[fid] = mkFn());
}

type CSSDeclarations = {
    [key: string]: number | string | string[];
}

const cssRules = new Set();
const styleSheet = (() => {
    let ss;
    function mk() {
        const style = <any> document.createElement("style");
        style.type = "text/css";

        document.head.appendChild(style);

        ss = <CSSStyleSheet> document.styleSheets[document.styleSheets.length - 1];
    }

    return () => {
        if (ss === undefined) {
            mk()
        };

        return ss;
    };
})();

const emitRule = (rule: any): string => {
    const {hash} = rule;

    if (!cssRules.has(hash)) {
        cssRules.add(hash);
        const text = cssRuleExText(rule);
        styleSheet().insertRule(text, styleSheet().cssRules.length);
    }

    return 's' + hash;
};

const renderCSSDeclarations = (() => {
    const hyphenate = (x: string): string => x
        .replace(/([A-Z])/g, "-$1")
        .replace(/^ms-/, "-ms-") // Internet Explorer vendor prefix.
        .toLowerCase();

    const append = (str: string, k: string, v: string | number): string =>
        str + (str.length === 0 ? "" : ";") + hyphenate(k) + ":" + v;

    return (x: CSSDeclarations): string => Object.keys(x).reduce((str, k) => {
        const v = x[k];
        return Array.isArray(v)
            ? v.reduce((a, v) => append(a, k, v), str)
            : append(str, k, v);
    }, "");
})();

const cssRuleExText = (() => {
    const renderCondition = c =>
        (c[0] == 1 ? `@media ` : `@supports `) + c[1] + ' ';

    const wrapWithCondition = (c: string[], text: string): string =>
        c.length === 0 ? text : wrapWithCondition(c.slice(1), renderCondition(c[0]) + "{" + text + "}");

    const cssStyleRuleExText = (rule: any): string =>
        wrapWithCondition(rule.conditions,
            [ "."
            , 's' + rule.hash
            , rule.suffixes.join("")
            , "{"
            , renderCSSDeclarations(rule.cssDeclarations)
            , "}"
            ].join(""));

    return (rule: any): string => {
        switch (rule.type) {
        case 1: return cssStyleRuleExText(rule);
        case 5: return `@font-face{${renderCSSDeclarations(rule.cssDeclarations)}}`;
        }
    };
})();

function spineToReact(clientH: ClientH, path, ctx: Context, spine, key) {
    if (typeof spine === 'string') {
        return spine;

    } else if (spine.type === 'Node') {
        const children = spine.children.map(([index, child]) =>
            spineToReact(clientH, [].concat(path, index), ctx, child, index)
        );

        const props: any = { key };

        const installEventListener = (fid, name) => {
            props[`on${capitalizeFirstLetter(name)}`] = getFn(ctx, path, fid, () => {
                return ev => {
                    clientH.dispatchNodeEvent(path, fid, ev);
                };
            });
        };

        for (const [k, a, b] of spine.attributes) {
            if (k === 'AVAL') {
                props[a] = b;
            } else if (k === 'AEVL') {
                installEventListener(a, b);
            } else if (k === 'ASTY') {
                props.className = a.map(v => {
                    switch (v[0]) {
                    case 1: return emitRule({ type: v[0], hash: v[1], conditions: v[2], suffixes: v[3], cssDeclarations: v[4] });
                    case 5: return emitRule({ type: v[0], hash: v[1], cssDeclarations: v[2] });
                    }
                }).filter(x => x !== undefined).join(" ");
            } else if (k === 'AREF') {
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
        }

        if (spine.tag === 'input') {
            return React.createElement(ControlledInput(), {
                elementType: 'input', props: props
            }, ...children);
        } else {
            return React.createElement(spine.tag, props, ...children);
        }

    } else if (spine.type === 'Component') {
        return React.createElement(getComponent(clientH, spine.id, spine.displayName), {
            clientH, key, path, spine
        });

    } else {
        throw new Error('spineToReact: unexpected value: ' + spine);
    }
}


function newBridge(appE, callbacks) {
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
