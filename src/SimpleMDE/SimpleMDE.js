const marked = require("marked");
const SimpleMDE = require("simplemde")

exports.marked = function (elname, toRender) {
    let d = document.getElementById(elname);
    if (d !== null && typeof (d) !== 'undefined') {
        d.innerHTML = marked(toRender, { smartypants: true, silent: true })
    };
};

exports.setHTML = function (el) {
    return function (html) {
        return function () {
            el.innerHTML = html;
        };
    };
};

exports.simpleMDE = function(elementId) { 
    return function() {
        let smde = new SimpleMDE({element: document.getElementById(elementId)});
        return smde;
    };
};

exports.onSimpleMDEChange = function (simpleMDE) {
    return function(onChange) {
        return function() {
            simpleMDE.codemirror.on("change", function() { 
                onChange(simpleMDE.value())();
            });
        };
    };
};

exports.setSimpleMDEValue = function(value, simpleMDE) {
    return function() { 
        simpleMDE.value = value;
    };
};

exports.getSimpleMDEValue = function(simpleMDE) {
    return function() { 
        return simpleMDE.value();
    };
};

exports.debug = function(string){
    return function(value) {
        return function() {
            console.log(string, value);
            return value;
        };
    };
};