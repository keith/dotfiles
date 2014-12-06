// Collapse comments on HN
// Mostly copied from https://github.com/Igglyboo/hn_collapse
var defaults = document.getElementsByClassName("default");

for(var i = 0; i < defaults.length; i++) {
  var collapse = document.createElement("a");
  collapse.href = "#";
  collapse.innerHTML = " [-] ";
  collapse.onclick = (function(collapse, comment, indent) {
    return function() {
      if (comment.style.display == "none") {
        comment.style.display = "block";
        collapse.innerHTML = " [-] ";
      } else{
        comment.style.display = "none";
        collapse.innerHTML = " [+] ";
      }

      var currentRow = comment.parentNode.parentNode.parentNode.parentNode.parentNode.parentNode.nextSibling;
      while (currentRow) {
        var currentRowIndent = currentRow.getElementsByTagName("img")[0].width;
        if (currentRowIndent > indent) {
          if (currentRow.style.display == "none") {
            currentRow.style.display = "block";
          } else {
            currentRow.style.display = "none";
          }

          currentRow = currentRow.nextSibling;
        } else {
          break;
        }
      }
    };
  })(collapse, defaults[i].getElementsByClassName("comment")[0],
    defaults[i].parentNode.getElementsByTagName("img")[0].width);

  var div = defaults[i].getElementsByTagName("div")[0];
  div.insertBefore(collapse, div.firstChild);
}
