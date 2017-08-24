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
