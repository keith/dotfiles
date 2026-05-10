" Netrw improvement, custom gx because fugitive:// breaks the default gx
" The key here is that the second argument is a 0 which means !remote
" nnoremap gx :call netrw#BrowseX(expand('<cfile>'), 0)<CR>
function! s:Gx()
  " Don't stop at parens for URLs
  let save_isf = &isfname
  let &isfname .= ',(,)'
  let s:gx_text = expand('<cfile>')
  let &isfname = save_isf

python3 << EOF
from urllib.parse import urlparse, urlunparse
import re
import signal
import subprocess
import vim

text = vim.eval("s:gx_text")

def _timeout(signum, frame):
    raise TimeoutError("gx regex timed out")

signal.signal(signal.SIGALRM, _timeout)
signal.alarm(2)

# https://daringfireball.net/2010/07/improved_regex_for_matching_urls
# Daring fireball regex
try:
    match = re.search(
        r"(?i)\b((?:[a-z][\w-]+:(?:/{1,3}|[a-z0-9%])|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'\".,<>?«»“”‘’]))",
        text)
finally:
    signal.alarm(0)

if match:
    url = match.group(0)
else:
    # NOTE: This actually opens virtually anything, but it enables just 'google.com' to work
    url = text.strip("\"'")

if "://" not in url:
    url = "https://" + url

subprocess.check_call(["open", url])
EOF
endfunction

" Tests:
"
" https://google.com
" google.com
" "google.com"
" "https://google.com"
" [nixos.org/download](https://nixos.org/download.html#nixos-pi)
" wikipedia.org/#some_(weird)_section
" https://wikipedia.org/#some_(weird)_section
nnoremap <silent> gx :call <SID>Gx()<CR>
