export const logInt = function (i) {
  return function () {
    console.log(i);
  };
};
