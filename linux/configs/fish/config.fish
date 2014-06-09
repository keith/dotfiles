set fish_greeting
set -gx PATH /usr/local/bin $PATH

set fish_key_bindings fish_vi_key_bindings

function fish_right_prompt
  if [ "$fish_bind_mode" = "default" ]
    printf "[NORMAL]"
  end
end

# TODO: Readline style CTRL-a/e/w
