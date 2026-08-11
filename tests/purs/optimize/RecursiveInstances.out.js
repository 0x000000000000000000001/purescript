import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Symbol from "../Data.Symbol/index.js";
import * as Type_Proxy from "../Type.Proxy/index.js";
var findKeysAuxNil = {
    findKeysAux: function (v) {
        return [  ];
    }
};
var findKeysAux = function (dict) {
    return dict.findKeysAux;
};
var findKeysAuxCons = function (dictIsSymbol) {
    return function (dictFindKeysAux) {
        return {
            findKeysAux: function (v) {
                return Data_Semigroup.append(Data_Semigroup.semigroupArray)([ Data_Symbol.reflectSymbol(dictIsSymbol)(Type_Proxy["Proxy"].value) ])(findKeysAux(dictFindKeysAux)(Type_Proxy["Proxy"].value));
            }
        };
    };
};
var findKeysAuxCons1 = /* #__PURE__ */ findKeysAuxCons({
    reflectSymbol: function () {
        return "a";
    }
});
var findKeysAuxCons2 = /* #__PURE__ */ findKeysAuxCons1(findKeysAuxNil);
var findKeysAuxCons3 = /* #__PURE__ */ findKeysAuxCons({
    reflectSymbol: function () {
        return "b";
    }
});
var findKeysAuxCons4 = /* #__PURE__ */ findKeysAuxCons({
    reflectSymbol: function () {
        return "c";
    }
});
var findKeysAuxCons5 = /* #__PURE__ */ findKeysAuxCons({
    reflectSymbol: function () {
        return "d";
    }
});
var findKeysAuxCons6 = /* #__PURE__ */ findKeysAuxCons1(/* #__PURE__ */ findKeysAuxCons3(/* #__PURE__ */ findKeysAuxCons4(/* #__PURE__ */ findKeysAuxCons5(/* #__PURE__ */ findKeysAuxCons({
    reflectSymbol: function () {
        return "e";
    }
})(findKeysAuxNil)))));
var findKeysAuxCons7 = /* #__PURE__ */ findKeysAuxCons1(/* #__PURE__ */ findKeysAuxCons3(findKeysAuxNil));
var findKeysAuxCons8 = /* #__PURE__ */ findKeysAuxCons1(/* #__PURE__ */ findKeysAuxCons3(/* #__PURE__ */ findKeysAuxCons4(findKeysAuxNil)));
var findKeysAuxCons9 = /* #__PURE__ */ findKeysAuxCons1(/* #__PURE__ */ findKeysAuxCons3(/* #__PURE__ */ findKeysAuxCons4(/* #__PURE__ */ findKeysAuxCons5(findKeysAuxNil))));
var findKeys = function () {
    return function (dictFindKeysAux) {
        return function (v) {
            return findKeysAux(dictFindKeysAux)(Type_Proxy["Proxy"].value);
        };
    };
};
var findKeys1 = /* #__PURE__ */ (function () {
    return findKeys()(findKeysAuxCons2)(Type_Proxy["Proxy"].value);
})();
var findKeys10 = /* #__PURE__ */ (function () {
    return findKeys()(findKeysAuxCons6)(Type_Proxy["Proxy"].value);
})();
var findKeys2 = /* #__PURE__ */ (function () {
    return findKeys()(findKeysAuxCons7)(Type_Proxy["Proxy"].value);
})();
var findKeys3 = /* #__PURE__ */ (function () {
    return findKeys()(findKeysAuxCons8)(Type_Proxy["Proxy"].value);
})();
var findKeys4 = /* #__PURE__ */ (function () {
    return findKeys()(findKeysAuxCons9)(Type_Proxy["Proxy"].value);
})();
var findKeys5 = /* #__PURE__ */ (function () {
    return findKeys()(findKeysAuxCons6)(Type_Proxy["Proxy"].value);
})();
var findKeys6 = /* #__PURE__ */ (function () {
    return findKeys()(findKeysAuxCons2)(Type_Proxy["Proxy"].value);
})();
var findKeys7 = /* #__PURE__ */ (function () {
    return findKeys()(findKeysAuxCons7)(Type_Proxy["Proxy"].value);
})();
var findKeys8 = /* #__PURE__ */ (function () {
    return findKeys()(findKeysAuxCons8)(Type_Proxy["Proxy"].value);
})();
var findKeys9 = /* #__PURE__ */ (function () {
    return findKeys()(findKeysAuxCons9)(Type_Proxy["Proxy"].value);
})();
export {
    findKeysAux,
    findKeys,
    findKeys1,
    findKeys2,
    findKeys3,
    findKeys4,
    findKeys5,
    findKeys6,
    findKeys7,
    findKeys8,
    findKeys9,
    findKeys10,
    findKeysAuxNil,
    findKeysAuxCons
};
