// -*- coding: utf-8-unix -*-
"use strict";

function include(path, charset) {
    var script  = document.createElement('script');
    script.src  = path;
    script.type = 'text/javascript';
    if (!charset) {
        script.charset = charset;
    }
    var head = document.getElementsByTagName('head').item(0);
    head.appendChild(script);
}

function onload() {
}

function debugPrint(str) {
    var out = document.getElementById('debug');
    if (!out) return;
    out.value += str + '\n';
}

function say(str) {
    document.write(str + '<br>');
}
