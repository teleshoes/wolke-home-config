// ==UserScript==
// @name     github-pr-by-commit
// @version  1
// @grant    none
// ==/UserScript==

var onclickFct = ""

var allAnchors = document.getElementsByTagName("a");
commitsSeen = {};
for(var i = allAnchors.length - 1; i>=0; i--){
  var link = allAnchors[i].href;
  m = link ? link.match("/pull/[0-9]+/commits/([0-9a-f]+)") : null;
  commit = m ? m[1] : null;
  if(commit && !(commit in commitsSeen)){
    onclickFct += "window.open('" + link + "', '_blank');\n";
    commitsSeen[commit] = 1;
  }
}

commits_bucket = document.getElementById("commits_bucket");
commits_bucket.innerHTML = "<button id='openAllCommitsButton'>OPEN ALL COMMITS</button>" + commits_bucket.innerHTML;

btn = document.getElementById("openAllCommitsButton");
btn.onclick = new Function (onclickFct);
