var __extends = (this && this.__extends) || function (d, b) {
    for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
    function __() { this.constructor = d; }
    d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
};
var Context = (function () {
    function Context() {
        this.fn = {};
    }
    return Context;
}());
var ClientH = (function () {
    function ClientH(appE, dispatchComponentEvent, dispatchNodeEvent, attachRef, detachRef, componentDidMount, componentWillUnmount) {
        this.appE = appE;
        this.dispatchComponentEvent = dispatchComponentEvent;
        this.dispatchNodeEvent = dispatchNodeEvent;
        this.attachRef = attachRef;
        this.detachRef = detachRef;
        this.componentDidMount = componentDidMount;
        this.componentWillUnmount = componentWillUnmount;
        this.rafId = undefined;
        this.componentRegistry = new Map;
        this.rootContext = new Context;
        this.components = new Map;
    }
    ClientH.prototype.renderSpine = function (spine) {
        var _this = this;
        if (this.rafId !== undefined) {
            cancelAnimationFrame(this.rafId);
        }
        this.rafId = requestAnimationFrame(function () {
            // const spine = JSON.parse(json);
            // console.time('spineToReact');
            var rootElement = spineToReact(_this, [], _this.rootContext, spine, undefined);
            // console.timeEnd('spineToReact');
            // console.time('ReactDOM.render');
            ReactDOM.render(rootElement, _this.appE);
            // console.timeEnd('ReactDOM.render');
            _this.rafId = undefined;
        });
    };
    ClientH.prototype.renderSpineAtPath = function (key, spine) {
        var component = this.components.get(key);
        if (component) {
            component.setState({ spine: spine });
        }
    };
    return ClientH;
}());
function getComponent(clientH, componentId) {
    var component = clientH.componentRegistry.get(componentId);
    if (component === undefined) {
        component = (function (_super) {
            __extends(class_1, _super);
            function class_1(props) {
                var _this = _super.call(this, props) || this;
                _this.ctx = new Context;
                _this.state = { spine: props.spine };
                return _this;
            }
            class_1.prototype.componentDidMount = function () {
                var _this = this;
                var _a = this.props, clientH = _a.clientH, path = _a.path, eventListeners = _a.spine.eventListeners;
                clientH.components.set(path.join('.'), this);
                clientH.componentDidMount(path);
                eventListeners.forEach(function (_a) {
                    var fid = _a[0], name = _a[1];
                    window.addEventListener(name, getFn(_this.ctx, path, fid, function () {
                        return function (ev) {
                            clientH.dispatchComponentEvent(path, fid, ev);
                        };
                    }));
                });
            };
            class_1.prototype.componentWillReceiveProps = function (nextProps) {
                this.setState({ spine: nextProps.spine });
            };
            class_1.prototype.componentWillUnmount = function () {
                var _this = this;
                var _a = this.props, clientH = _a.clientH, path = _a.path, eventListeners = _a.spine.eventListeners;
                clientH.componentWillUnmount(path);
                eventListeners.forEach(function (_a) {
                    var fid = _a[0], name = _a[1];
                    window.removeEventListener(name, getFn(_this.ctx, path, fid, function () {
                        return function () { return undefined; };
                    }));
                });
                clientH.components.delete(path.join('.'));
            };
            class_1.prototype.render = function () {
                var _a = this.props, clientH = _a.clientH, path = _a.path, key = _a.key;
                var spine = this.state.spine;
                return spineToReact(clientH, path, this.ctx, spine.spine, key);
            };
            return class_1;
        }(React.Component));
        clientH.componentRegistry.set(componentId, component);
    }
    return component;
}
function capitalizeFirstLetter(string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
}
var ControlledInput = (function () {
    var ci = undefined;
    function mkControlledInput() {
        ci = (function (_super) {
            __extends(ControlledInput, _super);
            function ControlledInput(props) {
                var _this = _super.call(this, props) || this;
                _this.state = { value: props.props.value || '' };
                _this.onChange = function (ev) {
                    _this.setState({ value: ev.target.value });
                    if (_this.props.props.onChange) {
                        _this.props.props.onChange(ev);
                    }
                };
                return _this;
            }
            ControlledInput.prototype.componentWillReceiveProps = function (nextProps) {
                if (nextProps.props.value !== this.state.value) {
                    this.setState({ value: nextProps.props.value });
                }
            };
            ControlledInput.prototype.render = function () {
                return React.createElement.apply(React, [this.props.elementType, Object.assign({}, this.props.props, { value: this.state.value, onChange: this.onChange })].concat((this.props.children || [])));
            };
            return ControlledInput;
        }(React.Component));
    }
    return function () {
        if (ci === undefined) {
            mkControlledInput();
        }
        return ci;
    };
})();
function getFn(ctx, path, fid, mkFn) {
    var pathCtx = ctx.fn[path] !== undefined
        ? ctx.fn[path]
        : (ctx.fn[path] = {});
    return pathCtx[fid] !== undefined
        ? pathCtx[fid]
        : (pathCtx[fid] = mkFn());
}
var cssRules = new Set();
var styleSheet = (function () {
    var ss;
    function mk() {
        var style = document.createElement("style");
        style.type = "text/css";
        document.head.appendChild(style);
        ss = document.styleSheets[document.styleSheets.length - 1];
    }
    return function () {
        if (ss === undefined) {
            mk();
        }
        ;
        return ss;
    };
})();
var emitRule = function (rule) {
    var hash = rule.hash;
    if (!cssRules.has(hash)) {
        cssRules.add(hash);
        var text = cssRuleExText(rule);
        styleSheet().insertRule(text, styleSheet().cssRules.length);
    }
    return 's' + hash;
};
var renderCSSDeclarations = (function () {
    var hyphenate = function (x) { return x
        .replace(/([A-Z])/g, "-$1")
        .replace(/^ms-/, "-ms-") // Internet Explorer vendor prefix.
        .toLowerCase(); };
    var append = function (str, k, v) {
        return str + (str.length === 0 ? "" : ";") + hyphenate(k) + ":" + v;
    };
    return function (x) { return Object.keys(x).reduce(function (str, k) {
        var v = x[k];
        return Array.isArray(v)
            ? v.reduce(function (a, v) { return append(a, k, v); }, str)
            : append(str, k, v);
    }, ""); };
})();
var cssRuleExText = (function () {
    var renderCondition = function (c) {
        return (c[0] == 1 ? "@media " : "@supports ") + c[1] + ' ';
    };
    var wrapWithCondition = function (c, text) {
        return c.length === 0 ? text : wrapWithCondition(c.slice(1), renderCondition(c[0]) + "{" + text + "}");
    };
    var cssStyleRuleExText = function (rule) {
        return wrapWithCondition(rule.conditions, [".",
            's' + rule.hash,
            rule.suffixes.join(""),
            "{",
            renderCSSDeclarations(rule.cssDeclarations),
            "}"
        ].join(""));
    };
    return function (rule) {
        switch (rule.type) {
            case 1: return cssStyleRuleExText(rule);
            case 5: return "@font-face{" + renderCSSDeclarations(rule.cssDeclarations) + "}";
        }
    };
})();
function spineToReact(clientH, path, ctx, spine, key) {
    if (typeof spine === 'string') {
        return spine;
    }
    else if (spine.type === 'Node') {
        var children = spine.children.map(function (_a) {
            var index = _a[0], child = _a[1];
            return spineToReact(clientH, [].concat(path, index), ctx, child, index);
        });
        var props_1 = { key: key };
        var installEventListener = function (fid, name) {
            props_1["on" + capitalizeFirstLetter(name)] = getFn(ctx, path, fid, function () {
                return function (ev) {
                    clientH.dispatchNodeEvent(path, fid, ev);
                };
            });
        };
        for (var _i = 0, _a = spine.attributes; _i < _a.length; _i++) {
            var _b = _a[_i], k = _b[0], a = _b[1], b = _b[2];
            if (k === 'AVAL') {
                props_1[a] = b;
            }
            else if (k === 'AEVL') {
                installEventListener(a, b);
            }
            else if (k === 'ASTY') {
                props_1.className = a.map(function (v) {
                    switch (v[0]) {
                        case 1: return emitRule({ type: v[0], hash: v[1], conditions: v[2], suffixes: v[3], cssDeclarations: v[4] });
                        case 5: return emitRule({ type: v[0], hash: v[1], cssDeclarations: v[2] });
                    }
                }).filter(function (x) { return x !== undefined; }).join(" ");
            }
            else if (k === 'AREF') {
                props_1.ref = getFn(ctx, path, 'ref', function () {
                    return function (ref) {
                        if (ref === null) {
                            clientH.detachRef(path);
                        }
                        else {
                            clientH.attachRef(path, ref);
                        }
                    };
                });
            }
        }
        if (spine.tag === 'input') {
            return React.createElement.apply(React, [ControlledInput(), {
                    elementType: 'input', props: props_1
                }].concat(children));
        }
        else {
            return React.createElement.apply(React, [spine.tag, props_1].concat(children));
        }
    }
    else if (spine.type === 'Component') {
        return React.createElement(getComponent(clientH, spine.id), {
            clientH: clientH, key: key, path: path, spine: spine
        });
    }
    else {
        throw new Error('spineToReact: unexpected value: ' + spine);
    }
}
if (this != null && typeof this.window !== 'undefined') {
    window['newBridge'] = function (appE, callbacks) {
        return new ClientH(appE, callbacks.componentEvent, callbacks.nodeEvent, callbacks.attachRef, callbacks.detachRef, callbacks.componentDidMount, callbacks.componentWillUnmount);
    };
}
