// ==UserScript==
// @name     github-pr-by-commit
// @version  1
// @grant    none
// ==/UserScript==

var onclickFct = ""

var commitAnchors = document.getElementsByClassName("BtnGroup-item");
for(var i = commitAnchors.length - 1; i>=0; i--){
  var link = commitAnchors[i].href;
  if(link != null && link.includes("/pull")){
    onclickFct += "window.open('" + link + "', '_blank');\n"
  }
}

commits_bucket = document.getElementById("commits_bucket");
commits_bucket.innerHTML = "<button id='openAllCommitsButton'>OPEN ALL COMMITS</button>" + commits_bucket.innerHTML;

btn = document.getElementById("openAllCommitsButton");
btn.onclick = new Function (onclickFct);
