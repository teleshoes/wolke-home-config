// ==UserScript==
// @name        boltbus
// @namespace   www.teleshoes.org
// @description sets up boston=>ny
// @include     https://www.boltbus.com/*?*
// @version     1
// @grant       metadata
// ==/UserScript==

signInSelector = 'ctl00_cphM_signIn'
signInButton = 'login_LoginButton'
rememberMe = 'login_Remember'

fwd = 'ctl00_cphM_forwardRouteUC_'

regionSelector = fwd + 'lstRegion_textBox'
origSelector = fwd + 'lstOrigin_textBox'
destSelector = fwd + 'lstDestination_textBox'
dateSelector = fwd + 'imageE'

textRegexNortheast = /.*Northeast.*/i
textRegexBoston = /.*Boston.*South.*Station.*/i
textRegexNY = /.*New York.*33rd.*/i

regionIdRegex = new RegExp(".*" + fwd + "lstRegion_repeater" + ".*", "i")
origIdRegex = new RegExp(".*" + fwd + "lstOrigin_repeater" + ".*", "i")
destIdRegex = new RegExp(".*" + fwd + "lstDestination_repeater" + ".*", "i")

function main(){
  if(maybeSignIn()){
    return
  }
  q = window.location.search
  if(q == '?bos-ny'){
    regionTextRegex = textRegexNortheast
    origTextRegex = textRegexBoston
    destTextRegex = textRegexNY
  }else if(q == '?ny-bos'){
    regionTextRegex = textRegexNortheast
    origTextRegex = textRegexNY
    destTextRegex = textRegexBoston
  }
  clickRoute(regionTextRegex, origTextRegex, destTextRegex)
}

function maybeSignIn(){
  if(document.getElementById(signInSelector)){
    clickDelay(0, signInSelector)
    setTimeout(function(){
      iframe = document.getElementsByName('ShowModal')[0]
      iframe = iframe.contentDocument || iframe.contentWindow.document
      iframe.getElementById(rememberMe).click()
      iframe.getElementById(signInButton).click()
    }, 1000)
    return true
  }
}

function clickRoute(regionTextRegex, origTextRegex, destTextRegex){
  clickDelay(0,    function(){return regionSelector})
  clickDelay(100,  function(){return getId(regionTextRegex, regionIdRegex)})
  clickDelay(1500, function(){return origSelector})
  clickDelay(1600, function(){return getId(origTextRegex, origIdRegex)})
  clickDelay(3000, function(){return destSelector})
  clickDelay(3100, function(){return getId(destTextRegex, destIdRegex)})
  clickDelay(4500, function(){return dateSelector})
}

function getId(textRegex, idRegex){
  var as = document.getElementsByTagName("a")
  for(var i=0; i<as.length; i++){
    if(idRegex.test(as[i].id) && textRegex.test(as[i].textContent)){
      return as[i].id
    }
  }
  window.alert('could not find element '
    + 'id=~' + idRegex + ' and text=~' + textRegex)
}

function clickDelay(delay, idFct){
  setTimeout(function(){
    document.getElementById(idFct()).click()
  }, delay)
}

main()
