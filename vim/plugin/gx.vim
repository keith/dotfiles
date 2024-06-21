" Netrw improvement, custom gx because fugitive:// breaks the default gx
" The key here is that the second argument is a 0 which means !remote
" nnoremap gx :call netrw#BrowseX(expand('<cfile>'), 0)<CR>
function! s:Gx()
python3 << EOF
from urllib.parse import urlparse, urlunparse
import re
import subprocess
import vim

text = vim.eval("expand('<cWORD>')")

# https://daringfireball.net/2010/07/improved_regex_for_matching_urls
# Daring fireball regex
match = re.search(
    r"(?i)\b((?:[a-z][\w-]+:(?:/{1,3}|[a-z0-9%])|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'\".,<>?«»“”‘’]))",
    text)
if match:
    url = urlunparse(urlparse(match.group(0), scheme="https"))
    subprocess.check_call(["open", url])
else:
    # TODO: This actually opens virtually anything, but it enables just 'google.com' to work
    text = text.strip("\"'")
    url = urlunparse(urlparse(text, scheme="https"))
    subprocess.check_call(["open", url])
EOF
endfunction

" Tests:
"
" https://google.com
" google.com
" "google.com"
" "https://google.com"
nnoremap <silent> gx :call <SID>Gx()<CR>
