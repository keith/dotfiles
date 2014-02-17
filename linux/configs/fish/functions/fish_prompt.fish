function fish_prompt
  set __pwd (pwd)
  set __parent (basename (dirname $__pwd))
  set __current (basename $__pwd)
  set __full (printf "%s/%s" $__parent $__current)
  set __home (echo $HOME | awk '{ print substr($1, 2, length($1) - 1); }')
  set __prompt (echo "$__full" | sed -e "s|$__home|~|" -e "s|$USER|~|" -e "s|^///|/|" -e "s|^//|/|")

  printf "(%s) " $__prompt
end
