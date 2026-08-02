export const logRecord = function (r) {
  return function () {
    console.log(r);
  };
};

export const getRecord = function () {
  return { a: 1 };
};
