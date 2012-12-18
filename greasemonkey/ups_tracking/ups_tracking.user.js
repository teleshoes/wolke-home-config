// ==UserScript==
// @name           ups tracking
// @namespace      ups
// @include        *ups.com*track*

regex = new RegExp('.*\\?gm(.*)');
results = regex.exec(window.location.href)
if(results){
  track = results[1]
  e = document.getElementById('trackNums')
  e.value = track
  e = document.getElementsByName('track.x')[0]
  e.click();
}
// ==/UserScript==
