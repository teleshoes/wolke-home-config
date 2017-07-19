// ==UserScript==
// @name        facebook
// @namespace   www.teleshoes.org
// @include     https://www.facebook.com/*
// @version     1
// @grant       none

var chatDockCoverDiv = document.createElement('div');
chatDockCoverDiv.style.cssText = (''
  + 'position:   fixed;'
  + 'left:       calc(100vw - 300px);'
  + 'top:        calc(100vh - 30px);'
  + 'width:      300px;'
  + 'height:     30px;'
  + 'z-index:    10000000;'
  + 'opacity:    0.3;'
  + 'background: #00f;'
);
chatDockCoverDiv.title = "(dbl-click to remove)"
chatDockCoverDiv.ondblclick = function(e){
  document.body.removeChild(chatDockCoverDiv);
}

document.body.appendChild(chatDockCoverDiv);

// ==/UserScript==
