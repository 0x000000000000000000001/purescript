export const log = function (str) {
  return function () {
    console.log(str);
  };
};

export const magic = {};
export const foreignValue = function (x) { return x; };
