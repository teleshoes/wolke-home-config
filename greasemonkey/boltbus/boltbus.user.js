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

regionNortheast = fwd + 'lstRegion_repeater_ctl01_link'
origBoston = fwd + 'lstOrigin_repeater_ctl01_link'
origNY = fwd + 'lstOrigin_repeater_ctl04_link'
destBoston = fwd + 'lstDestination_repeater_ctl00_link'
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
  clickDelay(0, regionSelector)
  clickDelay(100, regionNortheast)
  clickDelay(1500, origSelector)
  clickDelay(1600, orig)
  clickDelay(3000, destSelector)
  clickDelay(3100, dest)
  clickDelay(4500, dateSelector)
}

function clickDelay(delay, id){
  setTimeout(function(){
    document.getElementById(id).click()
  }, delay)
}

main()
