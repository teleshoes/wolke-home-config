// ==UserScript==
// @name           usps_track
// @namespace      usps.com
// @include        https://tools.usps.com/go/TrackConfirmAction.action?*

var url = window.location.href;
var groups = url.match(/\?(.+)$/);
if(groups != null){
  var labelNumber = groups[1];
  var input = document.getElementById('tLabels');
  input.value = labelNumber;
  
  var txt = document.createTextNode(
    "Grease monkey is submitting the above");
  input.parentNode.appendChild(txt);

  document.getElementById('trackNumFindBtn').click()
}
// ==/UserScript==
