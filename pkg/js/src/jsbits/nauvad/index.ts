import * as React from 'react'
import * as ReactDOM from 'react-dom'

import {AppH} from '../../Nauva/App'



// NAUVA_PORT is the TCP/websocket port where we can connect to.
// It's set in the index.html file in a generated <script> tag.
declare const NAUVA_PORT: number;

// This is the URL to which we'll try to connect to.
const wsURL = 'ws://localhost:' + NAUVA_PORT + '/_nauva';


const localAppH = containerElement =>
    new AppH(
        containerElement,
        () => {},
        () => {},
        () => {},
        () => {},
        () => {},
        () => {},
        () => {},
    )
}


// Initialize the nauvad runtime. Make sure everything is in place
// and we're ready to connect to the server.
const nauvadInit = () => {
    const containerElement = document.getElementById('root')
    if (containerElement === null) {
        throw new Error('nauvadInit: could not find container element (#root)');
    }

    const appH = localAppH(containerElement)

    return {
        containerElement,
        appH,
        ws: <void | WebSocket> undefined,
    }
}

// Think of it as a global 'IORef' with the nauvad client state.
const nauvad = nauvadInit()


// Actions
// ----------------------------------------------------------------------------
//
// These actions are 'IO ()', they have access to the global 'nauvad' ref and
// modify it in-place.

const loadingScreen = (heading: string, detail?: string, reason?: string): void => {
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
    }

    const p = (t, style) => React.createElement('p', {style}, t)

    const detailEl = detail === undefined ? null : p(detail, {fontSize: '1.5rem', fontWeight: '200', margin: 0})
    const reasonEl = reason === undefined ? null : p(reason, {fontSize: '0.825rem', color: '#999'})

    const el = React.createElement('div', { style }, heading,
        React.createElement('div', {style: {maxWidth: '520px', marginTop: '16px', letterSpacing: 0}}, detailEl, reasonEl))

    ReactDOM.render(el, nauvad.containerElement);
}

const connectWebSocket = (): void => {
    nauvad.appH.renderHead([{type: 'Node', tag: 'title', attributes: [], children: [[0, 'Nauva: Connecting']]}]);
    const ws = nauvad.ws = new WebSocket(wsURL)

    ws.addEventListener('open', () => {
        const sendValue = (v): void => {
            if (ws.readyState === 1) {
                ws.send(JSON.stringify(v));
            }
        }

        const sendHook = (path, value): void => {
            sendValue(['hook', path, value])
        }

        const sendRef = (path, action): void => {
            sendValue(['ref', path, action])
        }

        nauvad.appH = new AppH(
            nauvad.containerElement,
            x => { if (ws.readyState === 1) { sendValue(['location', x]) } },
            sendRef,
            sendRef,
            sendRef,
            sendRef,
            (path, vals) => vals.forEach(v => sendHook(path, v)),
            (path, vals) => vals.forEach(v => sendHook(path, v)),
        )

        nauvad.appH.renderHead([{type: 'Node', tag: 'title', attributes: [], children: [[0, 'Nauva: Compiling…']]}])
        nauvad.appH.sendLocation(window.location.pathname)
    })

    ws.addEventListener('message', msg => {
        const data = JSON.parse(msg.data);
        switch (data[0]) {
        case 1:
            break;
        case 2:
            sendLocation()
            setTimeout(() => { sendLocation() }, 200);
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
                nauvad.appH.renderHead(data[2])
            }
            nauvad.appH.renderSpine(data[1])
            break;
        }
    })

    const codeToString: {[code: number]: (ev: {reason: string}) => string} = {
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
    }

    ws.addEventListener('close', ev => {
        nauvad.appH = localAppH(nauvad.containerElement)
        nauvad.ws = undefined

        const reason = codeToString[ev.code] || (_ => `Unknown WebSocket code: ${ev.code}`)
        loadingScreen('Goodbye', 'The session has ended — please restart the server.', reason(ev))
    })

    ws.addEventListener('error', ev => {
        nauvad.appH = localAppH(nauvad.containerElement)
        nauvad.ws = undefined

        loadingScreen('Error', "💀 WHAT'S HAPPENING ?!?? 👀 ")
    })
}

nauvad.appH.renderHead([{type: 'Node', tag: 'title', attributes: [], children: [[0, 'Loading…']]}]);
connectWebSocket();
