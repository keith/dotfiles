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
};

String.prototype.replaceLast = function(character) {
  return this.substr(0, this.length - 1) + character;
};

$(document).keypress(function(e) {
  var aPressed = (e.which == 97 ? true : false);
  if (aPressed) {
    alternate(window.location.href);
  }
});

function selectElements(commentForm) {
  this.actions = commentForm.querySelector('.form-actions');
  this.bubblesContent = document.querySelectorAll('.timeline-new-comment.js-comment-container');
  this.bubble = this.bubblesContent[this.bubblesContent.length - 1];
  this.close = this.actions.querySelector('.js-comment-and-button');
  this.comment = this.actions.querySelector('.primary');
  this.textarea = commentForm.querySelector('textarea');
};

function button(text, innerHTML, closable) {
  if (closable == null) {
    closable = true;
  }

  var btn;
  btn = document.createElement('button');
  btn.innerHTML = text;
  btn.className = 'button';
  btn.setAttribute('tabindex', '1');
  btn.setAttribute('type', 'submit');
  btn.setAttribute('title', innerHTML);
  btn.addEventListener('click', function(event) {
    event.preventDefault();
    this.textarea.value += " " + innerHtml;
    if (closable) {
      this.close.click();
    } else {
      this.comment.click();
    }
    return this.textarea.value = '';
  });

  return btn;
};

function insertButtons(elem) {
  var btn, closeButtonGroup, openButtonGroup;
  closeButtonGroup = document.createElement('div');
  closeButtonGroup.className = 'button-group';
  closeButtonGroup.setAttribute('style', 'margin: 10px 0 0 0;');
  openButtonGroup = document.createElement('div');
  openButtonGroup.className = 'button-group';
  openButtonGroup.setAttribute('style', 'margin: 10px 0 0 25px;');
  btn = button("Unver", "We cannot update unversioned specs (specs that point to a commit). If you'd like to add a new version the repository would have to be tagged. If you would like to use a different commit in your own project you can use some flags in your `Podfile` documented [here](http://guides.cocoapods.org/syntax/podfile.html#pod)");
  closeButtonGroup.appendChild(btn);
  btn = button("Trunk", "We have just launched 'Trunk', which is the new way for you to submit your Pods. For more information on this have a read through the post at http://blog.cocoapods.org/CocoaPods-Trunk/ If you have previously had your Pod merged into the master repository, please ensure you claim your Pod http://blog.cocoapods.org/Claim-Your-Pods/");
  closeButtonGroup.appendChild(btn);
  btn = button("Structure", "The folder structure of this spec should be `name/version/name.podspec` where `name` is the same as `s.name` in the spec file.", false);
  openButtonGroup.appendChild(btn);
  btn = button("Tags", "`git push --tags`", false);
  openButtonGroup.appendChild(btn);
  btn = button("<img src='https://a248.e.akamai.net/assets.github.com/images/icons/emoji/beers.png' width='14' height='14'>", ":beers:", false);
  openButtonGroup.appendChild(btn);
  btn = button("<img src='https://a248.e.akamai.net/assets.github.com/images/icons/emoji/+1.png' width='14' height='14'>", ":+1:", false);
  openButtonGroup.appendChild(btn);
  elem.appendChild(closeButtonGroup);
  elem.appendChild(openButtonGroup);
};

function keithVSOSS() {
  var commentForm = document.querySelector('.js-new-comment-form');
  if (!commentForm) {
    return;
  }

  var mutationObserver, observer;
  selectElements(commentForm);
  mutationObserver = typeof WebKitMutationObserver !== "undefined" && WebKitMutationObserver !== null ? WebKitMutationObserver : MutationObserver;
  observer = new mutationObserver(function(mutations) {
    return mutations.forEach(function(mutation) {
      return selectElements(commentForm);
    });
  });

  observer.observe(this.actions, {
    childList: true
  });

  if (commentForm && this.close) {
    insertButtons(this.bubble);
  }
};

keithVSOSS();
