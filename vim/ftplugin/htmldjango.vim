setlocal commentstring={#\ %s\ #}

let b:surround_{char2nr("%")} = "{% \r %}"
let b:surround_{char2nr("v")} = "{{ \r }}"
