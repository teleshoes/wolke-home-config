// ==UserScript==
// @name        github-username
// @namespace   teleshoes
// @require     file:///home/wolke/greasemonkey/github-username/secret.js
// @include     https://github.com/*
// @version     1
// @grant       none
// ==/UserScript==

baseUrl = "https://github.com"
okUserNames = ["teleshoes", "ewolk"]

function getTargetUserName(url){
  if(/[?\/]teleshoes/.exec(url)){
    return "teleshoes";
  }else if(/[?\/]ewolk/.exec(url)){
    return "ewolk";
  }else{
    return null;
  }
}

function main(){
  url = document.URL;

  landingPage = isLandingPage();
  targetUserName = getTargetUserName(url);
  userName = getUserName();
  isLogoutUrl = isLogout(url);

  if(userName != null && targetUserName != null && userName != targetUserName){
    logout();
    navToLoginDelay(targetUserName);
  }else if(targetUserName != null && /github.com\/login/.exec(url)){
    login(targetUserName);
  }else if(userName == null && targetUserName != null){
    navToLoginDelay(targetUserName);
  }else if(landingPage){
    if(targetUserName == null){
      targetUserName = getTargetUserName(document.referrer);
    }
    if(targetUserName != null){
      navToLoginDelay(targetUserName);
    }
  }else if(is404()){
    if(/[?\/]lillegroup/.exec(url)){
      targetUserName = "ewolk";
      newUrl = baseUrl + "/" + targetUserName;
      window.open(newUrl, '_blank', true);
      setTimeout(function(){ location.reload(true) }, 5000);
    }
  }else if(isLogoutUrl){
    logout()
  }
}

function isLandingPage(){
  btns = document.getElementsByClassName('signin');
  if(btns.length == 1 && /\/login$/.exec(btns[0].href)){
    return true;
  }
  return false;
}

function is404(){
  return /Page not found/.exec(document.title) ? true : false;
}

function isLogout(url){
  if(/github.com\/logout/.exec(url)){
    return true;
  }else{
    return false;
  }
}

function navToLoginDelay(userName){
  setTimeout(function(){
    newUrl = baseUrl + "/login?" + userName;
    window.open(newUrl, '_self', false);
  }, 300);
}

function login(userName){
  un = document.getElementById('login_field');
  pw = document.getElementById('password');

  btn = null;
  login = document.getElementById('login');
  if(login != null){
    btns = login.getElementsByClassName('btn');
    if(btns.length == 1){
      btn = btns[0];
    }
  }

  if(un != null && pw != null && btn != null){
    un.value = userName;
    pw.value = secret[userName];
    btn.click();
  }
}

function logout(){
  document.getElementsByClassName('logout-form')[0].submit();
}

function getUserName(){
  ul = document.getElementById('user-links');
  if(ul != null){
    as = ul.getElementsByTagName("a");
    for(i=0; i<as.length; i++){
      a = as[i];
      href = a.href;
      arr = /\/([a-zA-Z0-9_]+)$/.exec(href);
      if(arr != null && arr.length == 2){
        userName = arr[1]
        if(okUserNames.indexOf(userName) >= 0){
          return userName
        }
      }
    }
  }
  return null;
}

main();
