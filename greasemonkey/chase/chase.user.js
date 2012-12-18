// ==UserScript==
// @name           chase
// @namespace      chase
// @description    chase logon
// @require        file:///home/wolke/greasemonkey/chase/secret.js
// @include        https://chaseonline.chase.com/Logon*
userId = document.getElementById('UserID');
password = document.getElementById('Password');

userId.value = secret['userId'];
password.value = secret['password'];

document.getElementById('logon').click()
// ==/UserScript==
