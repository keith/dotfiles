let g:projectionist_heuristics = {
      \ "Podfile": {
        \ "Podfile": {"alternate": "Podfile.lock", "type": "podfile"},
        \ "Podfile.lock": {"alternate": "Podfile"}
      \ },
      \ "*.m": {
        \ "*.h": {"alternate": "{}.m"},
        \ "*.m": {"alternate": "{}.h"}
      \ },
      \ "*.c": {
        \ "*.h": {"alternate": "{}.c"},
        \ "*.c": {"alternate": "{}.h"}
      \ },
      \ "*.cpp": {
        \ "*.h": {"alternate": "{}.cpp"},
        \ "*.cpp": {"alternate": "{}.h"}
      \ }
  \ }
