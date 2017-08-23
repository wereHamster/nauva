import * as React from 'react'
import isEqual from 'lodash/isEqual'

export class Head extends React.Component<{el: any}> {
    elementClone: null | Node = null

    ref: null | Node = null
    refFn = (ref: null | Node) => {
        this.ref = ref
        this.update()
    };

    update() {
        if (this.elementClone !== null) {
            document.head.removeChild(this.elementClone)
        }

        const ref = this.ref
        if (ref) {
            this.elementClone = ref.childNodes.item(0).cloneNode(true)
            document.head.appendChild(this.elementClone)
        }
    }

    shouldComponentUpdate(nextProps) {
        return !isEqual(this.props.el, nextProps.el)
    }

    componentDidUpdate() {
        this.update()
    }

    render() {
        return React.createElement('div', {ref: this.refFn}, this.props.el)
    }
}
