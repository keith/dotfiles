// Collapse comments on HN
// Mostly copied from https://github.com/Igglyboo/hn_collapse
(function () {
    'use strict';

    // This makes sure we're on a comment page. Otherwise dotjs in firefox
    // will not load the css because the JS fails to load
    var commentTree = document.getElementsByClassName("comment-tree");
    if (commentTree.length === 0) {
        return;
    }

    var comments = document.getElementsByClassName('athing');
    if(comments.length === 0){
        return;
    }

    var lastCommentIndex = 0;
    // Some pages have a submission title that is also a .athing that we do not need to process
    if (comments[0].getElementsByClassName('title').length !== 0) {
        lastCommentIndex = 1;
    }

    for (var i = comments.length - 1; i >= lastCommentIndex; i--) {
        var currentComment = comments[i];
        var currentCommentHeader = currentComment.getElementsByClassName('comhead')[0];
        var currentCommentBody = currentComment.getElementsByClassName('default')[0];
        var collapseTag = document.createElement('a');

        // Need to use indentation to figure out how far down in the tree it is since the comments are actually flat
        currentComment.dataset.indentation = currentComment.querySelector('.ind img').width;

        // Use hidden by to figure out what parent collapsed a certain comment so we don't end up with trees
        // that go expanded -> collapsed -> expanded
        currentComment.dataset.hiddenBy = -1;

        collapseTag.textContent = ' [-] ';
        collapseTag.className = 'collapse';
        collapseTag.onclick = getCollapseFunction(currentComment, currentCommentBody);

        currentCommentHeader.insertBefore(collapseTag, currentCommentHeader.firstChild);
    }

    function getCollapseFunction(comment, commentBody) {
        return function () {
            var alreadyHidden = commentBody.classList.contains('hidden');
            if (alreadyHidden) {
                commentBody.classList.remove('hidden');
                this.textContent = ' [-] ';
            } else {
                commentBody.classList.add('hidden');
                this.textContent = ' [+] '
            }

            var parentIndentation = parseInt(comment.dataset.indentation);
            var nextElementSibling = comment.nextElementSibling;
            while (nextElementSibling !== null) {
                var nextIndentation = parseInt(nextElementSibling.dataset.indentation);
                if (nextIndentation > parentIndentation) {
                    var nextHiddenBy = parseInt(nextElementSibling.dataset.hiddenBy);
                    if(nextHiddenBy === parentIndentation || nextHiddenBy === -1) {
                        if (alreadyHidden) {
                            nextElementSibling.classList.remove('hidden');
                            nextElementSibling.dataset.hiddenBy = -1;
                        } else {
                            nextElementSibling.classList.add('hidden');
                            nextElementSibling.dataset.hiddenBy = parentIndentation;
                        }
                    }
                    nextElementSibling = nextElementSibling.nextElementSibling;
                } else {
                    break;
                }
            }
        }
    }
})();
