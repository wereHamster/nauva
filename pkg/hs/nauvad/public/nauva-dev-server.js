// This is the URL to which we'll try to connect to.
const wsURL = 'ws://localhost:' + NAUVA_PORT + '/_nauva';
class Context {
    constructor() {
        this.fn = {};
        this.refs = new Map;
        this.nv$ref = (k) => {
            return this.refs.get(k);
        };
    }
}
const compileFunction = (ctx, { constructors, arguments, body }) => {
    const f = new Function('nv$ref', ...constructors.map(x => "nv$" + x), ...arguments, body);
    const constructorFunctions = constructors.map(x => (...args) => ([x, ...args]));
    return (...args) => {
        return f(ctx.nv$ref, ...constructorFunctions, ...args);
    };
};
// Initialize the nauvad runtime. Make sure everything is in place
// and we're ready to connect to the server.
const nauvadInit = () => {
    const appE = document.getElementById('root');
    if (appE === null) {
        throw new Error('nauvadInit: could not find container element (#root)');
    }
    return {
        appE,
        // ^ The HTMLElement into which we're rendering the application.
        rafId: undefined,
        // ^ Rendering is done asynchronously, scheduled through rAF.
        ctx: new Context,
        // ^ Top-level context.
        componentRegistry: new Map,
        // ^ Registry for (stateful) components.
        ws: undefined,
        // ^ WebSocket connection to the server.
        spine: undefined,
        // ^ The raw spine which was rendered into 'appE' or that's going
        // to be rendered into 'appE' in the next rAF tick.
        headFragment: document.createDocumentFragment(),
    };
};
// Think of it as a global 'IORef' with the nauvad client state.
const nauvad = nauvadInit();
// Actions
// ----------------------------------------------------------------------------
//
// These actions are 'IO ()', they have access to the global 'nauvad' ref and
// modify it in-place.
const sendValue = (v) => {
    if (typeof nauvad.ws !== 'undefined') {
        nauvad.ws.send(JSON.stringify(v));
    }
};
const sendLocation = () => {
    sendValue(['location', window.location.pathname]);
};
const sendHook = (path, value) => {
    sendValue(['hook', path, value]);
};
const sendAction = (path, name, action) => {
    sendValue(['action', path, name, action]);
};
const sendRef = (path, action) => {
    sendValue(['ref', path, action]);
};
class HeadElement extends React.Component {
    constructor() {
        super();
        this.elementClone = null;
        this.ref = null;
        this.refFn = ref => {
            this.ref = ref;
            this.update();
        };
        this.update = () => {
            if (this.elementClone !== null) {
                document.head.removeChild(this.elementClone);
            }
            const ref = this.ref;
            if (ref) {
                this.elementClone = ref.childNodes.item(0).cloneNode(true);
                document.head.appendChild(this.elementClone);
            }
        };
    }
    shouldComponentUpdate(nextProps) {
        const deepEqual = (a, b) => JSON.stringify(a) === JSON.stringify(b);
        return !deepEqual(this.props.el, nextProps.el);
    }
    componentDidUpdate() {
        this.update();
    }
    render() {
        return React.createElement('div', { ref: this.refFn }, this.props.el);
    }
}
const renderHead = (elements) => {
    ReactDOM.render(React.createElement('div', {}, ...elements.map(el => React.createElement(HeadElement, { el }))), nauvad.headFragment);
};
const render = () => {
    const { appE, ctx, ws, spine } = nauvad;
    if (typeof ws === 'undefined' || typeof spine === 'undefined') {
        console.warn('render: called without valid ws / spine');
        return;
    }
    // console.time('spineToReact');
    const rootElement = spineToReact(ws, [], ctx, spine, undefined);
    // console.timeEnd('spineToReact');
    // console.time('ReactDOM.render');
    ReactDOM.render(rootElement, appE);
    // console.timeEnd('ReactDOM.render');
};
const scheduleRender = (spine) => {
    nauvad.spine = spine;
    if (typeof nauvad.rafId === "undefined") {
        nauvad.rafId = requestAnimationFrame(() => {
            nauvad.rafId = undefined;
            render();
        });
    }
};
const loadingScreen = (heading, detail, reason) => {
    const style = {
        display: 'flex',
        flexDirection: 'column',
        justifyContent: 'center',
        alignItems: 'center',
        height: '100vh',
        width: '100vw',
        position: 'fixed',
        fontFamily: '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"',
        fontSize: '3rem',
        fontWeight: '200',
        letterSpacing: '-1px',
        lineHeight: 1.4,
        color: 'black',
    };
    const p = (t, style) => React.createElement('p', { style }, t);
    const detailEl = detail === undefined ? null : p(detail, { fontSize: '1.5rem', fontWeight: '200', margin: 0 });
    const reasonEl = reason === undefined ? null : p(reason, { fontSize: '0.825rem', color: '#999' });
    const el = React.createElement('div', { style }, heading, React.createElement('div', { style: { maxWidth: '520px', marginTop: '16px', letterSpacing: 0 } }, detailEl, reasonEl));
    ReactDOM.render(el, nauvad.appE);
};
const connectWebSocket = () => {
    renderHead([React.createElement('title', {}, 'Nauva: Connecting…')]);
    const ws = nauvad.ws = new WebSocket(wsURL);
    ws.addEventListener('open', () => {
        renderHead([React.createElement('title', {}, 'Nauva: Compiling…')]);
        sendLocation();
    });
    ws.addEventListener('message', msg => {
        const data = JSON.parse(msg.data);
        switch (data[0]) {
            case 1:
                break;
            case 2:
                sendLocation();
                setTimeout(() => { sendLocation(); }, 200);
                break;
            case 3:
                break;
            case 4:
                if (window.location.pathname !== data[1]) {
                    window.history.pushState({}, '', data[1]);
                }
                break;
            case 5:
                if (data[2]) {
                    renderHead(data[2].map(x => spineToReact(ws, [], nauvad.ctx, x, undefined)));
                }
                scheduleRender(data[1]);
                break;
        }
    });
    const codeToString = {
        [1000]: _ => "Normal closure, meaning that the purpose for which the connection was established has been fulfilled.",
        [1001]: _ => "An endpoint is \"going away\", such as a server going down or a browser having navigated away from a page.",
        [1002]: _ => "An endpoint is terminating the connection due to a protocol error",
        [1003]: _ => "An endpoint is terminating the connection because it has received a type of data it cannot accept (e.g., an endpoint that understands only text data MAY send this if it receives a binary message).",
        [1004]: _ => "Reserved. The specific meaning might be defined in the future.",
        [1005]: _ => "No status code was actually present.",
        [1006]: _ => "The connection was closed abnormally, e.g., without sending or receiving a Close control frame",
        [1007]: _ => "An endpoint is terminating the connection because it has received data within a message that was not consistent with the type of the message (e.g., non-UTF-8 [http://tools.ietf.org/html/rfc3629] data within a text message).",
        [1008]: _ => "An endpoint is terminating the connection because it has received a message that \"violates its policy\". This reason is given either if there is no other sutible reason, or if there is a need to hide specific details about the policy.",
        [1009]: _ => "An endpoint is terminating the connection because it has received a message that is too big for it to process.",
        [1010]: e => "An endpoint (client) is terminating the connection because it has expected the server to negotiate one or more extension, but the server didn't return them in the response message of the WebSocket handshake. <br /> Specifically, the extensions that are needed are: " + e.reason,
        [1011]: _ => "A server is terminating the connection because it encountered an unexpected condition that prevented it from fulfilling the request.",
        [1015]: _ => "The connection was closed due to a failure to perform a TLS handshake (e.g., the server certificate can't be verified).",
    };
    ws.addEventListener('close', ev => {
        const reason = codeToString[ev.code] || (_ => `Unknown WebSocket code: ${ev.code}`);
        loadingScreen('Goodbye', 'The session has ended — please restart the server.', reason(ev));
    });
    ws.addEventListener('error', ev => {
        loadingScreen('Error', "💀 WHAT'S HAPPENING ?!?? 👀 ");
    });
};
function getComponent(componentId, displayName) {
    let component = nauvad.componentRegistry.get(componentId);
    if (component === undefined) {
        component = class extends React.Component {
            constructor(props) {
                super(props);
                this.ctx = new Context;
            }
            componentDidMount() {
                const { path, spine: { eventListeners, hooks: { componentDidMount } } } = this.props;
                componentDidMount.forEach(exp => {
                    const f = compileFunction(this.ctx, exp);
                    const a = f();
                    if (a) {
                        sendHook(path, a);
                    }
                });
                eventListeners.forEach(([name, expr]) => {
                    const f = compileFunction(this.ctx, expr);
                    window.addEventListener(name, getFn(this.ctx, path, expr.id, () => {
                        return ev => {
                            const a = f(ev);
                            if (a) {
                                sendAction(path, name, a);
                            }
                        };
                    }));
                });
            }
            componentWillUnmount() {
                const { path, spine: { eventListeners, hooks: { componentWillUnmount } } } = this.props;
                componentWillUnmount.forEach(exp => {
                    const f = compileFunction(this.ctx, exp);
                    const a = f();
                    if (a) {
                        sendHook(path, a);
                    }
                });
                eventListeners.forEach(([name, expr]) => {
                    window.removeEventListener(name, getFn(this.ctx, path, expr.id, () => {
                        return () => undefined;
                    }));
                });
            }
            render() {
                const { ws, path, spine, key } = this.props;
                return spineToReact(ws, path, this.ctx, spine.spine, key);
            }
        };
        component.displayName = displayName;
        nauvad.componentRegistry.set(componentId, component);
    }
    return component;
}
function capitalizeFirstLetter(string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
}
class ControlledInput extends React.Component {
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
        return React.createElement(this.props.elementType, Object.assign({}, this.props.props, { value: this.state.value, onChange: this.onChange }), ...(this.props.children || []));
    }
}
function getFn(ctx, path, fid, mkFn) {
    const pathCtx = ctx.fn[path] !== undefined
        ? ctx.fn[path]
        : (ctx.fn[path] = {});
    return pathCtx[fid] !== undefined
        ? pathCtx[fid]
        : (pathCtx[fid] = mkFn());
}
const style = document.createElement("style");
style.type = "text/css";
document.head.appendChild(style);
const styleSheet = document.styleSheets[document.styleSheets.length - 1];
const cssRules = new Set();
const emitRule = (rule) => {
    const { hash } = rule;
    if (!cssRules.has(hash)) {
        cssRules.add(hash);
        const text = cssRuleExText(rule);
        styleSheet.insertRule(text, styleSheet.cssRules.length);
    }
    return 's' + hash;
};
const renderCSSDeclarations = (() => {
    const hyphenate = (x) => x
        .replace(/([A-Z])/g, "-$1")
        .replace(/^ms-/, "-ms-") // Internet Explorer vendor prefix.
        .toLowerCase();
    const append = (str, k, v) => str + (str.length === 0 ? "" : ";") + hyphenate(k) + ":" + v;
    return (x) => Object.keys(x).reduce((str, k) => {
        const v = x[k];
        return Array.isArray(v)
            ? v.reduce((a, v) => append(a, k, v), str)
            : append(str, k, v);
    }, "");
})();
const cssRuleExText = (() => {
    const renderCondition = c => (c[0] == 1 ? `@media ` : `@supports `) + c[1] + ' ';
    const wrapWithCondition = (c, text) => c.length === 0 ? text : wrapWithCondition(c.slice(1), renderCondition(c[0]) + "{" + text + "}");
    const cssStyleRuleExText = (rule) => wrapWithCondition(rule.conditions, [".",
        's' + rule.hash,
        rule.suffixes.join(""),
        "{",
        renderCSSDeclarations(rule.cssDeclarations),
        "}"
    ].join(""));
    return (rule) => {
        switch (rule.type) {
            case 1: return cssStyleRuleExText(rule);
            case 5: return `@font-face{${renderCSSDeclarations(rule.cssDeclarations)}}`;
            default: return '';
        }
    };
})();
const spineToReact = (ws, path, ctx, spine, key) => {
    if (spine === null) {
        return null;
    }
    else if (typeof spine === 'string') {
        return spine;
    }
    else if (spine.type === 'Node') {
        const children = spine.children.map(([index, child]) => spineToReact(ws, [].concat(path, index), ctx, child, index));
        const props = { key };
        const installEventListener = ([name, expr]) => {
            const f = compileFunction(ctx, expr);
            props[`on${capitalizeFirstLetter(name)}`] = getFn(ctx, path, expr.id, () => {
                return ev => {
                    const a = f(ev);
                    if (a) {
                        sendAction(path, name, a);
                    }
                };
            });
        };
        for (const [k, a, b] of spine.attributes) {
            if (k === 'AVAL') {
                props[a] = b;
            }
            else if (k === 'AEVL') {
                installEventListener(a);
            }
            else if (k === 'ASTY') {
                props.className = a.map(v => {
                    switch (v[0]) {
                        case 1: return emitRule({ type: v[0], name: v[1], hash: v[2], conditions: v[3], suffixes: v[4], cssDeclarations: v[5] });
                        case 5: return emitRule({ type: v[0], hash: v[1], cssDeclarations: v[2] });
                    }
                }).filter(x => x !== undefined).join(" ");
            }
            else if (k === 'AREF') {
                props.ref = getFn(ctx, path, 'ref', () => {
                    return ref => {
                        if (ref === null) {
                            // spine.ref.detach;
                            if (a.key) {
                                ctx.refs.delete(a.key);
                            }
                            const f = compileFunction(ctx, a.detach);
                            const r = f();
                            if (r) {
                                sendRef(path, r);
                            }
                        }
                        else {
                            if (a.key) {
                                ctx.refs.set(a.key, ref);
                            }
                            const f = compileFunction(ctx, a.attach);
                            const r = f(ref);
                            if (r) {
                                sendRef(path, r);
                            }
                        }
                    };
                });
            }
        }
        if (spine.tag === 'input') {
            return React.createElement(ControlledInput, {
                elementType: 'input', props: props
            }, ...children);
        }
        else {
            return React.createElement(spine.tag, props, ...children);
        }
    }
    else if (spine.type === 'Component') {
        return React.createElement(getComponent(spine.id, spine.displayName), {
            ws, key, path, spine
        });
    }
    else {
        throw new Error('spineToReact: unexpected value: ' + spine);
    }
};
renderHead([React.createElement('title', {}, 'Loading…')]);
connectWebSocket();
window.addEventListener('popstate', () => {
    sendLocation();
});
