" Stop myself from typing print instead of printf
match ErrorMsg 'print('

let b:format_on_save = filereadable('.keithclangformat')

command! A ClangdSwitchSourceHeader
