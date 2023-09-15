export ENV_AUTH=$HOME/.env_auth

_dotenv_hash_pair() {
  env_file=$1
  env_shasum=$(shasum $env_file | cut -d' ' -f1)
  echo "$env_file:$env_shasum"
}

_dotenv_authorized_env_file() {
  env_file=$1
  pair=$(_dotenv_hash_pair $env_file)
  touch $ENV_AUTH
  grep -Gq $pair $ENV_AUTH
}

_dotenv_authorize() {
  env_file=$1
  _dotenv_deauthorize $env_file
  _dotenv_hash_pair $env_file >> $ENV_AUTH
}

_dotenv_deauthorize() {
  env_file=$1
  echo $(grep -Gv $env_file $ENV_AUTH) > $ENV_AUTH
}

_dotenv_print_unauthorized_message() {
  echo "Attempting to load unauthorized env: $1"
  echo ""
  echo "**********************************************"
  echo ""
  cat $1
  echo ""
  echo "**********************************************"
  echo ""
  echo "Would you like to authorize it? (y/n)"
}

_dotenv_source_env() {
  _dotenv_source_file "$PWD/.env"
  _dotenv_source_file "$PWD/.keithenv"
}

_dotenv_source_file() {
  local env_file="$1"

  if [[ ! -f $env_file ]];then
    return
  fi

  if _dotenv_authorized_env_file $env_file; then
    source $env_file
    return 0
  fi

  echo "Unauthorized env files found: $env_file, run dotenv_authorize to authorize"
}

dotenv_authorize() {
  _dotenv_authorize "$PWD/.env"
  _dotenv_authorize "$PWD/.keithenv"
  source "$PWD/.env"
  source "$PWD/.keithenv"
}

chpwd_functions=($chpwd_functions _dotenv_source_env)
_dotenv_source_env
