import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
var liftM1 = function (dictMonad) {
    var bind = Control_Bind.bind(dictMonad.Bind1());
    var pure = Control_Applicative.pure(dictMonad.Applicative0());
    return function (f) {
        return function (a) {
            return bind(a)(function (a$prime) {
                return pure(f(a$prime));
            });
        };
    };
};
var ap = function (dictMonad) {
    var Bind1 = dictMonad.Bind1();
    var bind = Control_Bind.bind(Bind1);
    var bind1 = Control_Bind.bind(Bind1);
    var pure = Control_Applicative.pure(dictMonad.Applicative0());
    return function (f) {
        return function (a) {
            return bind(f)(function (f$prime) {
                return bind1(a)(function (a$prime) {
                    return pure(f$prime(a$prime));
                });
            });
        };
    };
};
export {
    liftM1,
    ap
};
