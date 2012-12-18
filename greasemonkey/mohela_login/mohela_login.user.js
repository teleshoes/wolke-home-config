// ==UserScript==
// @name           mohela login
// @namespace      www.teleshoes.org
// @require        file:///home/wolke/greasemonkey/mohela_login/secret.js
// @include        https://www.mohela.com/DL/secure/account/loginStep1.aspx

try{
  //first time page loads
  e = document.getElementById('ctl00_cphContent_txtLoginID');
  e.value = secret['loginId']
  document.getElementById('ctl00_cphContent_btnLogin').click()
}finally{
  //second time page loads
  document.getElementById('ctl00_cphContent_btnContinue').click()
}

// ==/UserScript==
