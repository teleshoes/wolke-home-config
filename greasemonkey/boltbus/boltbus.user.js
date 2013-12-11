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

regionNortheast = fwd + 'lstRegion_repeater_ctl00_link'
origBoston = fwd + 'lstOrigin_repeater_ctl01_link'
origNY = fwd + 'lstOrigin_repeater_ctl03_link'
destBoston = fwd + 'lstDestination_repeater_ctl01_link'
destNY = fwd + 'lstDestination_repeater_ctl00_link'

function main(){
  if(maybeSignIn()){
    return
  }
  q = window.location.search
  if(q == '?bos-ny'){
    orig = origBoston
    dest = destNY
  }else if(q == '?ny-bos'){
    orig = origNY
    dest = destBoston
  }
  clickRoute(orig, dest)
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

function clickRoute(orig, dest){
  clickDelay(0,    function(){return regionSelector})
  clickDelay(100,  function(){return regionNortheast})
  clickDelay(1500, function(){return origSelector})
  clickDelay(1600, function(){return orig})
  clickDelay(3000, function(){return destSelector})
  clickDelay(3100, function(){return dest})
  clickDelay(4500, function(){return dateSelector})
}

function clickDelay(delay, idFct){
  setTimeout(function(){
    document.getElementById(idFct()).click()
  }, delay)
}

main()
