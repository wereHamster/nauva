type CSSDeclarations = {
    [key: string]: number | string | string[];
}

const cssRules = new Set()

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

export const emitRule = (rule: any): string => {
    const {hash} = rule;

    if (!cssRules.has(hash)) {
        cssRules.add(hash);
        const text = cssRuleExText(rule);
        styleSheet().insertRule(text, styleSheet().cssRules.length);
    }

    return rule.name === '' ? `s-${rule.hash}` : `${rule.name}-${rule.hash}`;
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
            , rule.name === '' ? `s-${rule.hash}` : `${rule.name}-${rule.hash}`
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
