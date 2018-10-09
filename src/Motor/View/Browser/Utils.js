"use strict";

exports.getOffsetHeight = function(key) {
    return function() {
        return document.getElementById(key).offsetHeight;
    }
};
