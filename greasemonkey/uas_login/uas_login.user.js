// ==UserScript==
// @name           uas login
// @namespace      www.teleshoes.org
// @require        file:///home/wolke/greasemonkey/uas_login/secret.js
// @include        https://www.uasecho.com/

e = document.getElementById('txtUserName');
e.value = secret['userName'];

e = document.getElementById('txtUserPassword');
e.value = secret['password'];

document.getElementById('Login_Login').click()
// ==/UserScript==
