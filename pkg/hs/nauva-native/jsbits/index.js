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
        // console.log('renderSpineAtPath', key, spine, component);
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
                _super.call(this, props);
                this.ctx = new Context;
                this.state = { spine: props.spine };
            }
            class_1.prototype.componentDidMount = function () {
                var _this = this;
                var _a = this.props, clientH = _a.clientH, path = _a.path, eventListeners = _a.spine.eventListeners;
                clientH.components.set(path.join('.'), this);
                console.log('Registering component at key', path.join('.'));
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
var ControlledInput = (function (_super) {
    __extends(ControlledInput, _super);
    function ControlledInput(props) {
        var _this = this;
        _super.call(this, props);
        this.state = { value: props.props.value || '' };
        this.onChange = function (ev) {
            _this.setState({ value: ev.target.value });
            if (_this.props.props.onChange) {
                _this.props.props.onChange(ev);
            }
        };
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
function getFn(ctx, path, fid, mkFn) {
    var pathCtx = ctx.fn[path] !== undefined
        ? ctx.fn[path]
        : (ctx.fn[path] = {});
    return pathCtx[fid] !== undefined
        ? pathCtx[fid]
        : (pathCtx[fid] = mkFn());
}
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
            props_1[("on" + capitalizeFirstLetter(name))] = getFn(ctx, path, fid, function () {
                console.log('getFn', fid, name);
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
                props_1.style = a;
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
            return React.createElement.apply(React, [ControlledInput, {
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
window['newBridge'] = function (appE, callbacks) {
    return new ClientH(appE, callbacks.componentEvent, callbacks.nodeEvent, callbacks.attachRef, callbacks.detachRef, callbacks.componentDidMount, callbacks.componentWillUnmount);
};
