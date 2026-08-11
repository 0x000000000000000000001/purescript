import * as Data_Symbol from "../Data.Symbol/index.js";
import * as Record_Unsafe from "../Record.Unsafe/index.js";
import * as Type_Proxy from "../Type.Proxy/index.js";
var fooIsSymbol = {
    reflectSymbol: function () {
        return "foo";
    }
};
var barIsSymbol = {
    reflectSymbol: function () {
        return "bar";
    }
};
var set = function (dictIsSymbol) {
    return function () {
        return function (l) {
            return function (a) {
                return function (r) {
                    return Record_Unsafe.unsafeSet(Data_Symbol.reflectSymbol(dictIsSymbol)(l))(a)(r);
                };
            };
        };
    };
};
var get = function (dictIsSymbol) {
    return function () {
        return function (l) {
            return function (r) {
                return Record_Unsafe.unsafeGet(Data_Symbol.reflectSymbol(dictIsSymbol)(l))(r);
            };
        };
    };
};
var foo = /* #__PURE__ */ (function () {
    return Type_Proxy["Proxy"].value;
})();
var h = function (n) {
    return set(fooIsSymbol)()(foo)(n)({
        foo: 0
    });
};
var f = function (n) {
    return get(fooIsSymbol)()(foo)({
        foo: n
    });
};
var bar = /* #__PURE__ */ (function () {
    return Type_Proxy["Proxy"].value;
})();
var g = function (n) {
    return get(barIsSymbol)()(bar)({
        foo: 0,
        bar: n
    });
};
export {
    get,
    set,
    foo,
    bar,
    f,
    g,
    h
};
