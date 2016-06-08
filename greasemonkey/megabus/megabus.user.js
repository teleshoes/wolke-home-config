// ==UserScript==
// @name        megabus
// @namespace   www.teleshoes.org
// @require     file:///home/wolke/greasemonkey/megabus/secret.js
// @description boston<=>nyc
// @include     https://us.megabus.com/*?gm-*
// @version     1
// @grant       metadata
// ==/UserScript==

baseUrl = 'https://us.megabus.com/'
loginUrl = baseUrl + '/?gm-login'

boston = 94
nyc = 123

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

  if(q == "?gm-bos-ny"){
    from = boston
    to = nyc
  }else if(q == "?gm-ny-bos"){
    from = nyc
    to = boston
  }
  fromField = document.getElementById('JourneyPlanner_ddlOrigin')
  toField = document.getElementById('JourneyPlanner_ddlDest')
  dateField = document.getElementById('JourneyPlanner_txtOutboundDate')
  btn = document.getElementById('JourneyPlanner_btnSearch')

  if(fromField != null && toField != null && btn != null){
    setTimeoutInv(1000, function(){
      fromField.value = from;
      fireOnChange(fromField);

      setTimeoutInv(1000, function(){
        toField.value = to;
        fireOnChange(toField);

        setTimeoutInv(1000, function(){
          dateField.value = new Date().toLocaleDateString();
          fireOnChange(dateField)

          setTimeoutInv(1000, function(){
            btn.click();
          })
        })
      })
    });
  }
}

function setTimeoutInv(delay, fun){
  setTimeout(fun, delay);
}

function fireOnChange(elem){
  if ("createEvent" in document) {
    var evt = document.createEvent("HTMLEvents");
    evt.initEvent("change", false, true);
    elem.dispatchEvent(evt);
  }else{
    elem.fireEvent("onchange");
  }
}

function getOptElem(select, val){
  opts = select.children;
  for (var i=0; i<opts.length; i++){
    if(opts[i].value == val){
      return opts[i];
    }
  }
  return null;
}

function isLoggedIn(){
  unField = document.getElementById('UserStatus_txtEmail')
  if(unField != null){
    return false;
  }else{
    return true;
  }
}

function loginPopupRefresh(){
  setTimeout(function(){
    window.open(loginUrl, '_blank', true);
    setTimeout(function(){ location.reload(true) }, 5000);
  }, 300);
}

function login(){
  un = document.getElementById('UserStatus_txtEmail');
  pw = document.getElementById('UserStatus_txtPassword');
  btn = document.getElementById('UserStatus_btnLogin');

  if(un != null && pw != null && btn != null){
    un.value = secret['username'];
    pw.value = secret['password'];
    btn.click();
  }
}

main();
