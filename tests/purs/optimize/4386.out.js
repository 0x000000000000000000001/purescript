import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Monad_ST_Internal from "../Control.Monad.ST.Internal/index.js";
import * as Data_Function from "../Data.Function/index.js";
var pure = /* #__PURE__ */ Control_Applicative.pure(Control_Monad_ST_Internal.applicativeST);
var mySTFn2 = function (a, b) {
    return a + b | 0;
};
var mySTFn1 = function (a) {
    return a + 1 | 0;
};
var myInt2 = function () {
    return mySTFn2(0, 1);
};
var myInt1 = function () {
    return mySTFn1(0);
};
var otherTest = function __do() {
    var a = mySTFn2(0, 1);
    var b = mySTFn1(2);
    var c = myInt1();
    var d = myInt2();
    return Data_Function.apply(pure)(((a + b | 0) + c | 0) + d | 0)();
};
export {
    mySTFn1,
    mySTFn2,
    myInt1,
    myInt2,
    otherTest
};
