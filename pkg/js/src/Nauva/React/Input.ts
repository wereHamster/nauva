import * as React from 'react'

export class Input extends React.Component<any, any> {
    constructor(props) {
        super(props)
        this.state = { value: props.props.value || '' }
    }

    onChange = (ev: any): void => {
        this.setState({ value: ev.target.value })
        if (this.props.props.onChange) {
            this.props.props.onChange(ev)
        }
    }

    componentWillReceiveProps(nextProps) {
        if (nextProps.props.value !== this.state.value) {
            this.setState({ value: nextProps.props.value })
        }
    }

    render() {
        return React.createElement(this.props.elementType, Object.assign({},
            this.props.props, { value: this.state.value, onChange: this.onChange }
        ), ...(this.props.children || []))
    }
}
