#!/usr/bin/env bash

run_segment() {
    dirs=$MAILDIRS
    if [[ -z $maildirs ]];then
        dirs="$MAIL/*/INBOX/new/"
    fi

    echo $(find $dirs -type f | wc -l | xargs)
}
