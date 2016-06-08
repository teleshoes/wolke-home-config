// ==UserScript==
// @name        gobus
// @namespace   www.teleshoes.org
// @require     file:///home/wolke/greasemonkey/gobus/secret.js
// @description boston<=>nyc
// @include     https://www.gobuses.com/*?gm-*
// @version     1
// @grant       metadata
// ==/UserScript==

baseUrl = 'https://www.gobuses.com/'
loginUrl = baseUrl + 'account-home/?gm-login'

function main(){
  q = window.location.search
  if(q == "?gm-login"){
    login();
    return;
  }

  if(!isLoggedIn()){
    loginPopupRefresh();
    return;
  }
}

function isLoggedIn(){
  loggedInRegex = new RegExp(".*" + "You are currently logged in as" + ".*");

  divs=document.getElementsByClassName('header-top-right');
  if(divs.length > 0 && loggedInRegex.test(divs[0].innerHTML)){
    return true;
  }else{
    return false;
  }
}

function loginPopupRefresh(){
  setTimeout(function(){
    window.open(loginUrl, '_blank', true);
    setTimeout(function(){ location.reload(true) }, 5000);
  }, 300);
}

function login(){
  un = document.getElementById('fld_loginEmailAddress');
  pw = document.getElementById('fld_loginPass');
  remember = document.getElementById('fld_loginRemember');
  btn = document.getElementById('loginBttn');

  if(un != null && pw != null && btn != null){
    un.value = secret['username'];
    pw.value = secret['password'];
    remember.checked = true;
    btn.click();
  }
}

main();
