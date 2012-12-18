// ==UserScript==
// @name           mohela login step 2
// @namespace      mohela
// @require        file:///home/wolke/greasemonkey/mohela_login_step_2/secret.js
// @include        https://www.mohela.com/DL/secure/account/loginStep2.aspx?loginID=*
document.getElementById('ctl00_cphMainForm_txtPassword').value = secret['password'];
document.getElementById('ctl00_cphMainForm_btnSubmit').click();
// ==/UserScript==
