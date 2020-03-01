// Alternate between h/m Objective-C files with 'a'
function alternate(URL) {
  if (URL.indexOf("#") !== -1) {
    var URL = URL.substr(0, URL.indexOf('#'));
  }

  var newURL = null;
  if (URL.endsWith(".h")) {
    newURL = URL.replaceLast("m");
  } else if (URL.endsWith(".m")) {
    newURL = URL.replaceLast("h");
  } else if (URL.endsWith(".c")) {
    newURL = URL.replaceLast("h");
  }

  if (newURL) {
    window.location.href = newURL;
  }
}

function commits(oldURL) {
  window.location.href = oldURL + "/commits/master";
}

String.prototype.replaceLast = function(character) {
  return this.substr(0, this.length - 1) + character;
};

$(document).keypress(function(e) {
  // Exit early if we're in an input field
  // This wasn't needed in Chrome but is in Safari
  if (document.activeElement.nodeName === "INPUT" || document.activeElement.nodeName === "TEXTAREA") {
    return;
  }

  // If you're in a file editing box it's not technically an input field.
  if (document.activeElement.isContentEditable) {
    return;
  }

  switch (e.which) {
    case 97: // 'a'
      alternate(window.location.href);
      break;
    case 99: // 'c'
      commits(window.location.href);
      break;
    default:
      break;
  }
});

// Actions on pageload

$(document).ready(function() {
  removeDislikedElements();
  addHideDiffButton();
});

// Remove some buttons GitHub adds that I don't want to click
function removeDislikedElements() {
  $(".js-new-blob-submit").remove();
  $(".signup-prompt-bg").remove();
  $(".toolbar-help").remove();
  $("a[href^=github-mac]").remove();
}

// Add a button to 'fold' a file in diff mode (super nice for storyboards!)
function addHideDiffButton() {
  $(".file-actions").each(function() {
    var diff = $(this).parent().parent();
    var link = $("<a href=\"#\" class=\"btn btn-sm\">Hide</a>");
    $(this).prepend(link);
    link.click(function() {
      var codeArea = $(diff).find('[class$="wrapper"]');
      $(this).html() == 'Hide' ? codeArea.hide() : codeArea.show()
      $(this).html($(this).html() == 'Show' ? 'Hide' : 'Show');
      return false;
    });
  });
}
