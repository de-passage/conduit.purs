const SimpleMDE = require("simplemde")


exports.simpleMDE = function (elementId) {
    return function () {
        let smde = new SimpleMDE({ element: document.getElementById(elementId) });
        return smde;
    };
};

exports.createSimpleMDE = function (element) {
    return function () {
        let smde = new SimpleMDE({ element: element });
        return smde;
    };
};

exports.onSimpleMDEChange = function (simpleMDE) {
    return function (onChange) {
        return function () {
            simpleMDE.codemirror.on("change", function () {
                onChange(simpleMDE.value())();
            });
        };
    };
};

exports.setSimpleMDEValue = function (value) {
    return function (simpleMDE) {
        return function () {
            simpleMDE.value(value);
        };
    };
};

exports.getSimpleMDEValue = function (simpleMDE) {
    return simpleMDE.value();
};

// exports.debug = function (string) {
//     return function (value) {
//         return function () {
//             console.log(string, value);
//             return value;
//         };
//     };
// };