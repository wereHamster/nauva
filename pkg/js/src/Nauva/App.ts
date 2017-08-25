import * as React from 'react'
import * as ReactDOM from 'react-dom'

import {Input} from './React/Input'
import {Head} from './React/Head'

import {emitRule} from './CSS'


type ComponentId = number

class Context {
    refs: { [id: string]: any } = {}
    fn: { [id: string]: { [path: string]: any } } = {}
}


export class AppH {
    rafId: number = undefined
    componentRegistry: Map<ComponentId, any> = new Map
    rootContext = new Context

    components: Map<string, any> = new Map
    headFragment: any = document.createDocumentFragment()

    constructor
        ( public containerElement: HTMLElement
        , public sendLocation: any
        , public dispatchComponentEvent: any
        , public dispatchNodeEvent: any
        , public attachRef: any
        , public detachRef: any
        , public componentDidMount: any
        , public componentWillUnmount: any
        ) {
        window.addEventListener('popstate', () => {
            this.sendLocation(window.location.pathname);
        })
    }

    pushLocation(path: string): void {
        try {
            if (window.location.pathname !== path) {
                window.history.pushState({}, '', path);
            }
        } catch (e) {
            console.error('ClientH::pushLocation', e);
        }
    }

    renderHead(elements: any): void {
        ReactDOM.render(React.createElement('div', {}, ...elements
            .map(x => spineToReact(this, [], this.rootContext, x, undefined))
            .map(el => React.createElement(Head, { el }))), this.headFragment);
    }

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
            ReactDOM.render(rootElement as JSX.Element, this.containerElement);
            // console.timeEnd('ReactDOM.render');

            this.rafId = undefined;
        });
    }

    renderSpineAtPath(key: string, spine: any): void {
        const component = this.components.get(key)
        if (component) {
            component.setState({ spine })
        }
    }
}

const compileF = (o) => {
    const f = new Function('nv$ref', ...o.constructors.map(x => "nv$" + x), ...o.arguments, o.body)
    const constructorFunctions = o.constructors.map(x => (...args) => ([x, ...args]))
    return (refs, ...args) => f(k => refs[k], ...constructorFunctions, ...args)
}

function spineToReact(appH: AppH, path, ctx: Context, spine, key) {
    if (spine === null) {
        return null

    } else if (typeof spine === 'string') {
        return spine

    } else if (spine.type === 'Node') {
        const children = spine.children.map(([index, child]) =>
            spineToReact(appH, [].concat(path, index), ctx, child, index))

        const props: any = { key }

        const installEventListener = (name, o) => {
            props[`on${capitalizeFirstLetter(name)}`] = getFn(ctx, path, o.id, () => {
                const f = compileF(o)
                return ev => {
                    const v = f(ctx.refs, ev)
                    appH.dispatchNodeEvent(path, v);
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
                    case 1: return emitRule({ type: v[0], name: v[1], hash: v[2], conditions: v[3], suffixes: v[4], cssDeclarations: v[5] });
                    case 5: return emitRule({ type: v[0], hash: v[1], cssDeclarations: v[2] });
                    }
                }).filter(x => x !== undefined).join(" ");
            } else if (k === 'AREF') {
                props.ref = getFn(ctx, path, 'ref', () => {
                    const attachF = compileF(a.attach)
                    const detachF = compileF(a.detach)
                    return ref => {
                        ctx.refs[a.key] = ref
                        if (ref === null) {
                            appH.detachRef(path, detachF(ctx.refs));
                        } else {
                            appH.attachRef(path, attachF(ctx.refs, ref));
                        }
                    };
                });
            }
        }

        if (spine.tag === 'input') {
            return React.createElement(Input, {elementType: 'input', props}, ...children);
        } else {
            return React.createElement(spine.tag, props, ...children);
        }

    } else if (spine.type === 'Component') {
        return React.createElement(getComponent(appH, spine.id, spine.displayName), {
            appH, key, path, spine
        })

    } else {
        throw new Error('spineToReact: unexpected value: ' + spine)
    }
}

function getComponent(appH: AppH, componentId: ComponentId, displayName: string) {
    let component = appH.componentRegistry.get(componentId);
    if (component === undefined) {
        component = class extends React.Component {
            props: {
                appH: AppH;
                path: string[];
                hooks: any;
                spine: any;
                key: string;
            };

            state: { spine: any }

            ctx = new Context

            constructor(props) {
                super(props)
                this.state = { spine: props.spine }
            }

            componentDidMount() {
                const {appH, path, spine: {eventListeners, hooks: {componentDidMount}}} = this.props;
                appH.components.set(path.join('.'), this);

                const vals = componentDidMount.map(o => getFn(this.ctx, path, o.id, () => {
                    const f = compileF(o)
                    return () => f(this.ctx.refs)
                })())
                appH.componentDidMount(path, vals);

                eventListeners.forEach(([name, o]) => {
                    window.addEventListener(name, getFn(this.ctx, path, o.id, () => {
                        const f = compileF(o)
                        return ev => {
                            appH.dispatchComponentEvent(path, f(this.ctx.refs, ev));
                        };
                    }));
                })
            }

            componentWillReceiveProps(nextProps) {
                this.setState({ spine: nextProps.spine });
            }

            componentWillUnmount() {
                const {appH, path, spine: {eventListeners, hooks: {componentWillUnmount}}} = this.props;

                const vals = componentWillUnmount.map(o => getFn(this.ctx, path, o.id, () => {
                    const f = compileF(o)
                    return () => f(this.ctx.refs)
                })())
                appH.componentWillUnmount(path, vals);

                eventListeners.forEach(([name, f]) => {
                    window.removeEventListener(name, getFn(this.ctx, path, f.id, () => {
                        return () => undefined;
                    }));
                });

                appH.components.delete(path.join('.'));
            }

            render() {
                const {appH, path} = this.props
                const {spine} = this.state
                return spineToReact(appH, [].concat(path, 0), this.ctx, spine.spine, undefined) as JSX.Element
            }
        };

        component.displayName = displayName

        appH.componentRegistry.set(componentId, component)
    }

    return component;
}

function getFn(ctx: Context, path, fid, mkFn) {
    const pathCtx = ctx.fn[path] !== undefined
        ? ctx.fn[path]
        : (ctx.fn[path] = {});

    return pathCtx[fid] !== undefined
        ? pathCtx[fid]
        : (pathCtx[fid] = mkFn());
}


function capitalizeFirstLetter(string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
}
