// Canonical test for #2866. This doesn't need to test whether `apply`s
// defined from modules other than `Data.Function` are incorrectly
// optimized since the rest of the test suite seemingly catches it.
import * as Data_Function from "../Data.Function/index.js";
var Area = function (x) {
    return x;
};
var areaFlipped = /* #__PURE__ */ Data_Function.applyFlipped(42)(Area);
var area = /* #__PURE__ */ Data_Function.apply(Area)(42);
export {
    Area,
    area,
    areaFlipped
};
