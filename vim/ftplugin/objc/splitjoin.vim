if !exists('b:splitjoin_split_callbacks')
  let b:splitjoin_split_callbacks = [
        \ 'sj#objc#SplitIfClause',
        \ 'sj#objc#SplitSetProperty',
        \ ]
endif

if !exists('b:splitjoin_join_callbacks')
  let b:splitjoin_join_callbacks = [
        \ 'sj#objc#JoinIfClause',
        \ 'sj#objc#JoinSetProperty',
        \ 'sj#objc#JoinNSNumber',
        \ ]
endif
