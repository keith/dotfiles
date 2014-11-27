" File:        objc_matchbracket.vim
" Author:      Michael Sanders (msanders42 [at] gmail [dot] com)
" Version:     0.8
" Description: TextMate's "Insert Matching Start Bracket" feature implemented
"              in vim script. Makes it a lot more pleasant to write Objective-C.
" Usage:       Just type "]" in insert mode after an object or method you want to
"              wrap in brackets and this will wrap it appropriately; to escape out
"              of it once you're finished, simply time "]" again.

if exists("g:objc_matchbracket") && g:objc_matchbracket
  finish
endif
let g:objc_matchbracket = 1

inoremap <buffer> <silent> ] <C-r>=<SID>MatchBracket()<CR>

function! s:Count(haystack, needle)
  let counter = 0
  let index = stridx(a:haystack, a:needle)
  while index != -1
    let counter += 1
    let index = stridx(a:haystack, a:needle, index + 1)
  endw
  return counter
endfunction

" Automatically inserts matching bracket, TextMate style!
function! s:MatchBracket()
  if pumvisible() " Close popup menu if it's visible.
    call feedkeys("\<esc>a", 'n')
    call feedkeys(']')
    return ''
  endif

  let line = getline('.')
  let lnum = line('.')
  let col  = col('.') - 1
  let before_cursor = strpart(line, 0, col)

  " Only wrap past delimeters such as ";", "*", "return", etc.
  " But ignore delimeters in function calls.
  let functionPos = match(before_cursor, '\v(if|for|while)@!<\w+>\s*\(.{-}\)([;,|{}!])@!') + 1
  if functionPos
    let before_cursor = strpart(line, 0, functionPos)
  endif

  let delimPos = matchend(before_cursor, '\v.*(^|[;,|{}()!*&^%~=]|\s*return)\s*') + 1
  let wrap_text = strpart(before_cursor, delimPos - 1)

  " These are used to tell whether the bracket is still open:
  let left_brack_count = s:Count(before_cursor, '[') " Note the before_cursor!
  let right_brack_count = s:Count(before_cursor, ']')

  " Don't autocomplete if line is blank, if inside or directly outside
  " string, or if inserting a matching bracket.
  if wrap_text == '' || wrap_text =~'@\=["'']\S*\s*\%'.col.'c'
           \ || s:Count(line, '[') > s:Count(line, ']')
    return ']'
  " Escape out of string when bracket is the next character, unless
  " wrapping past a colon or equals sign, or inserting a closing bracket.
  elseif line[col] == ']' && wrap_text !~ '\v\k+:\s*\k+(\s+\k+)+$'
              \ && (before_cursor !~ '\[.*\(=\)]'
                \ || left_brack_count != right_brack_count + 1)
    " "]" has to be returned here or the "." command breaks.
    call setline(lnum, substitute(line, '\%'.(col + 1).'c.', '', ''))
    return ']'
  else
    " Only wrap past a colon, except for special keywords such as "@selector:".
    " E.g., "foo: bar|" becomes "foo: [bar |]", and "[foo bar: baz bar|]"
    " becomes "[foo bar: [baz bar]|]" but "[foo bar: baz bar]|" becomes
    " "[[foo bar: baz bar] |]" (where | is the cursor).
    let colonPos = matchend(wrap_text, '^\v(\[\k+\s+)=\k+:\s*') + 1
    if colonPos && colonPos > matchend(wrap_text,
          \ '\v.*\<\@(selector|operator|ope|control):')
          \ && left_brack_count != right_brack_count
      let delimPos += colonPos - 1
    endif

    let col -= 1
    " If a space or tab is already added, don't add another.
    if line[col] =~ '\s'
      let col -= 1
      let space =  ''
    " Automatically append space if there is only 1 word.
    " E.g., "foo" becomes "[foo ]", and "foo bar" becomes "[foo bar]"
    else
      let space = line[col] == ']' || wrap_text !~ '^\s*\S\+\s\+' ? ' ' : ''
    endif

    exe 'norm! i'.space.']'
    call cursor(lnum, delimPos)
    normal! i[
    call cursor(lnum, col + 4)

    return ''
  endif
endfunction
