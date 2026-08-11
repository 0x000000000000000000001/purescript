import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
var liftM1 = function (dictMonad) {
    var Bind1 = dictMonad.Bind1();
    var Applicative0 = dictMonad.Applicative0();
    return function (f) {
        return function (a) {
            return Control_Bind.bind(Bind1)(a)(function (a$prime) {
                return Control_Applicative.pure(Applicative0)(f(a$prime));
            });
        };
    };
};
var ap = function (dictMonad) {
    var Bind1 = dictMonad.Bind1();
    var Applicative0 = dictMonad.Applicative0();
    return function (f) {
        return function (a) {
            return Control_Bind.bind(Bind1)(f)(function (f$prime) {
                return Control_Bind.bind(Bind1)(a)(function (a$prime) {
                    return Control_Applicative.pure(Applicative0)(f$prime(a$prime));
                });
            });
        };
    };
};
export {
    liftM1,
    ap
};
