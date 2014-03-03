exec_python () {
  name=$1
  shift
  if [[ $name == "django-admin.py" ]];then
    $name "$@"
    return
  fi

  python $name
}
alias -s py=exec_python
