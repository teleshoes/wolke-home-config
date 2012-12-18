// ==UserScript==
// @name           usps_track
// @namespace      usps.com
// @include        http://www.usps.com/shipping/trackandconfirm.htm
var url = window.location.href;
var groups = url.match(
  /^http:\/\/www.usps.com\/shipping\/trackandconfirm.htm\?(.+)$/
);

if(groups != null){
  var labelNumber = groups[1];
  var input = document.getElementById('label_number');
  input.value = labelNumber;
  
  var txt = document.createTextNode(
    "Grease monkey is submitting the above");
  input.parentNode.appendChild(txt);
  
  document.forms[0].submit();
}
// ==/UserScript==
