"use strict";

exports.getLocalStorage = function(key) {
  return function() {
    return window.localStorage.getItem(key);
  };
}

exports.setLocalStorage = function(key) {
  return function (val) {
    return function () {
      window.localStorage.setItem(key, val);
    };
  };
};
