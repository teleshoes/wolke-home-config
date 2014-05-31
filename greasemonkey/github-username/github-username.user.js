// ==UserScript==
// @name        github-username
// @namespace   teleshoes
// @require     file:///home/wolke/greasemonkey/github-username/secret.js
// @include     https://github.com/*
// @version     1
// @grant       none
// ==/UserScript==

function main(){
  q = window.location.search
  url = document.URL
  if(q == "?teleshoes" || /teleshoes/.exec(url)){
    targetUserName = "teleshoes"
  }else if(q == "?ewolk" || /ewolk/.exec(url)){
    targetUserName = "ewolk"
  }else{
    return
  }

  userName = getUserName()
  if(userName != null && userName != targetUserName){
    logout();
  }else if(/github.com\/login/.exec(url)){
    login(targetUserName)
  }
}

function login(userName){
  un = document.getElementById('login_field')
  pw = document.getElementById('password')

  btn = null
  login = document.getElementById('login')
  if(login != null){
    btns = login.getElementsByClassName('button')
    if(btns.length == 1){
      btn = btns[0]
    }
  }

  if(un != null && pw != null && btn != null){
    un.value = userName
    pw.value = secret[userName]
    btn.click()
  }
}

function logout(){
  setTimeout(function(){
    btns = document.getElementsByClassName("sign-out-button")
    if(btns.length == 1){
      btns[0].click()
    }
  }, 500);
}

function getUserName(){
  ul = document.getElementById('user-links')
  if(ul != null){
    as = ul.getElementsByTagName("a")
    if(as.length > 0){
      a = as[0]
      href = a.href
      arr = /\/([a-zA-Z0-9_]+)$/.exec(href)
      if(arr != null && arr.length == 2){
        return arr[1]
      }
    }
  }
  return null
}

main()
