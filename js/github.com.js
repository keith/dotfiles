function alternate(URL) {
  var newURL = null;
  if (URL.endsWith(".h")) {
    newURL = URL.replaceLast("m");
  } else if (URL.endsWith(".m")) {
    newURL = URL.replaceLast("h");
  }

  if (newURL) {
    window.location.href = newURL;
  }
}

String.prototype.replaceLast = function(character) {
  return this.substr(0, this.length - 1) + character;
};

$(document).ready(function() {
  $(".file > .meta > .actions").each(function() {
    var diff = $(this).parent().parent();
    var link = $("<a href=\"#\" class=\"minibutton\">Hide</a>");
    $(this).prepend(link);
    link.click(function() {
      var codeArea = $(diff).find('[class$="wrapper"]');
      $(this).html() == 'Hide' ? codeArea.hide() : codeArea.show()
      $(this).html($(this).html() == 'Show' ? 'Hide' : 'Show');
      return false;
    });
  });
});

$(document).keypress(function(e) {
  // Exit early if we're in an input field
  // This wasn't needed in Chrome but is in Safari
  if (document.activeElement.nodeName === "INPUT") {
    return;
  }

  // The letter 'a'
  var aPressed = (e.which == 97 ? true : false);
  if (aPressed) {
    alternate(window.location.href);
  }
});

function selectElements(_this) {
  _this.actions = commentForm().querySelector('.form-actions');
  _this.bubblesContent = document.querySelectorAll('.timeline-new-comment.js-comment-container');
  _this.bubble = _this.bubblesContent[_this.bubblesContent.length - 1];
  _this.close = _this.actions.querySelector('.js-comment-and-button');
  _this.comment = _this.actions.querySelector('.primary');
  return _this.textarea = commentForm().querySelector('textarea');
}

function commentForm() {
  return document.querySelector('.js-new-comment-form');
}

function button(text, innerHTML, closable) {
  if (closable === null) {
    closable = true;
  }

  var btn;
  btn = document.createElement('button');
  btn.innerHTML = text;
  btn.className = 'btn';
  btn.setAttribute('tabindex', '1');
  btn.setAttribute('type', 'submit');
  btn.setAttribute('title', innerHTML);
  btn.addEventListener('click', function(event) {
    event.preventDefault();
    selectElements(this);
    this.textarea.value += " " + innerHTML;
    if (closable) {
      this.close.click();
    } else {
      this.comment.click();
    }
    return this.textarea.value = '';
  });

  return btn;
}

function insertButtons(elem) {
  var btn, buttonGroup;
  buttonGroup = document.createElement('div');
  buttonGroup.className = 'button-group';
  buttonGroup.setAttribute('style', 'margin: 10px 0 0 0;');
  btn = button("Unver", "We cannot update unversioned specs (specs that point to a commit). If you'd like to add a new version the repository would have to be tagged. If you would like to use a different commit in your own project you can use some flags in your `Podfile` documented [here](http://guides.cocoapods.org/syntax/podfile.html#pod)");
  buttonGroup.appendChild(btn);
  btn = button("Trunk", "We have just launched 'Trunk', which is the new way for you to submit your Pods. For more information on this have a read through the post at http://blog.cocoapods.org/CocoaPods-Trunk/ If you have previously had your Pod merged into the master repository, please ensure you claim your Pod http://blog.cocoapods.org/Claim-Your-Pods/");
  buttonGroup.appendChild(btn);
  btn = button("Structure", "The folder structure of this spec should be `name/version/name.podspec` where `name` is the same as `s.name` in the spec file.", false);
  buttonGroup.appendChild(btn);
  btn = button("Tags", "`git push --tags`", false);
  buttonGroup.appendChild(btn);
  btn = button("<img src='https://a248.e.akamai.net/assets.github.com/images/icons/emoji/beers.png' width='14' height='14'>", ":beers:", false);
  buttonGroup.appendChild(btn);
  btn = button("<img src='https://a248.e.akamai.net/assets.github.com/images/icons/emoji/+1.png' width='14' height='14'>", ":+1:", false);
  buttonGroup.appendChild(btn);
  elem.appendChild(buttonGroup);
}

(function() {
  if (!commentForm()) {
    return;
  }

  var mutationObserver, observer;
  selectElements(this);
  mutationObserver = typeof WebKitMutationObserver !== "undefined" && WebKitMutationObserver !== null ? WebKitMutationObserver : MutationObserver;
  observer = new mutationObserver(function(mutations) {
    return mutations.forEach(function(mutation) {
      return selectElements(this);
    });
  });

  observer.observe(this.actions, {
    childList: true
  });

  if (this.close) {
    insertButtons(this.bubble);
  }
}).call();
