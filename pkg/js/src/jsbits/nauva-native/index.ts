import {AppH} from '../../Nauva/App'

export const newBridge = (containerElement, callbacks) => new AppH
    ( containerElement
    , callbacks.sendLocation
    , callbacks.componentEvent
    , callbacks.nodeEvent
    , callbacks.attachRef
    , callbacks.detachRef
    , callbacks.componentDidMount
    , callbacks.componentWillUnmount
    )

export const evalF = (refs, args, o) => {
    const f = new Function('nv$ref', ...o.constructors.map(x => "nv$" + x), ...o.arguments, o.body)
    const constructorFunctions = o.constructors.map(x => (...args) => ([x, ...args]))
    const nv$ref = (k) => refs[k]
    return f(nv$ref, ...constructorFunctions, ...args)
}
