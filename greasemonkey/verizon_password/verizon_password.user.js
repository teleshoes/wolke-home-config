// ==UserScript==
// @name           verizon password
// @namespace      http://teleshoes.org
// @require        file:///home/wolke/greasemonkey/verizon_password/secret.js
// @include        http://192.168.1.1/*
e = document.getElementById('pass1');
e.value = secret['password'];
e = document.getElementsByName('user_name')[0];
e.value = secret['admin'];
// ==/UserScript==
