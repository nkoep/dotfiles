autoload -U add-zsh-hook

typeset -g VENV_DIR_NAME=.venv
typeset -g PYTHON_VERSION_FILE=.python-version

typeset -g current_project_dir=""

function find_project_dir() {
  local dir="$PWD"
  while [[ -n $dir && $dir != "/" ]]; do
    if [[ -f "$dir/$PYTHON_VERSION_FILE" ]]; then
      echo "$dir"
      return 0
    fi
    dir=$(dirname "$dir")
  done
  return 1
}

function is_cached_dir_valid() {
  [[ $PWD == "$current_project_dir" ]]
}

function validate_and_update_cache() {
  local project_dir
  if ! project_dir=$(find_project_dir); then
    current_project_dir=""
    return 1
  fi

  current_project_dir="${project_dir:A}"
  return 0
}

function activate_venv() {
  local venv_dir="$1"
  local activate_script="$venv_dir/bin/activate"

  if [[ ! -f $activate_script ]]; then
    echo "Error: Virtual environment activate script not found"
    return 1
  fi

  if [[ -z $VIRTUAL_ENV || $VIRTUAL_ENV != "$venv_dir" ]]; then
    VIRTUAL_ENV_DISABLE_PROMPT=1 source "$activate_script"
  fi
}

function deactivate_venv() {
  if [[ -n $VIRTUAL_ENV ]]; then
    if ! type deactivate >/dev/null 2>&1; then
      echo "Error: deactivate function not found"
      return 1
    fi
    deactivate
  fi
}

function activate_or_create_venv() {
 local project_dir="$1"
 local venv_dir="${project_dir}/${VENV_DIR_NAME}"
 local py_version_file="${project_dir}/${PYTHON_VERSION_FILE}"

 [[ ! -f $py_version_file ]] && return 1

 local required_version=$(cut -d. -f1,2 "$py_version_file")

 if [[ -d $venv_dir ]]; then
   local venv_python_version
   local venv_python="$venv_dir/bin/python"
   if ! venv_python_version=$("$venv_python" --version 2>&1 |
       awk '{print $2}' | cut -d. -f1,2); then
     echo "Error: Failed to determine Python version in virtual environment"
     return 1
   fi

   if [[ $required_version == "$venv_python_version" ]]; then
     activate_venv "$venv_dir"
   else
     echo "Warning: .venv Python version ($venv_python_version) does not" \
          "match .python-version ($required_version)"
     return 1
   fi
 else
   if ! command -v uv >/dev/null; then
     echo "Error: 'uv' command not found. Please install uv to create" \
          "virtual environments."
     return 1
   fi

   echo "Creating new virtual environment with Python $required_version..."
   if ! uv venv --python "$required_version" "$venv_dir"; then
     echo "Error: Failed to create virtual environment"
     return 1
   fi

   activate_venv "$venv_dir"
 fi
}

function auto_activate_venv() {
  if [[ -n $VIRTUAL_ENV && ! -d $VIRTUAL_ENV ]]; then
    deactivate_venv
  fi

  if is_cached_dir_valid && [[ -n $VIRTUAL_ENV ]]; then
    return
  fi

  if ! validate_and_update_cache; then
    deactivate_venv
    return
  fi

  activate_or_create_venv "$current_project_dir"
}

add-zsh-hook precmd auto_activate_venv
