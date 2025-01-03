autoload -U add-zsh-hook

LAST_PROJECT_DIR=""

function find_python_version_file() {
  local dir="$PWD"
  while [[ $dir != "/" ]]; do
    if [[ -f "$dir/.python-version" ]]; then
      echo "$dir"
      return
    fi
    dir=$(dirname "$dir")
  done
  return 1
}

function is_cached_dir_valid() {
  [[ $PWD == "$LAST_PROJECT_DIR" ]]
}

function validate_and_update_cache() {
  local project_dir=$(find_python_version_file)
  if [[ $project_dir != "$LAST_PROJECT_DIR" ]]; then
    LAST_PROJECT_DIR="$project_dir"
  fi
  echo "$LAST_PROJECT_DIR"
}

function activate_or_create_venv() {
  local project_dir="$1"
  local venv_dir="${project_dir}/.venv"
  local py_version_file="${project_dir}/.python-version"

  if [[ -f $py_version_file ]]; then
    local required_version=$(cut -d. -f1,2 "$py_version_file")
    if [[ -d $venv_dir ]]; then
      local venv_python_version=$("$venv_dir/bin/python" --version 2>&1 | awk '{print $2}' | cut -d. -f1,2)
      if [[ $required_version == "$venv_python_version" ]]; then
        if [[ -z $VIRTUAL_ENV || $VIRTUAL_ENV != "$venv_dir" ]]; then
          VIRTUAL_ENV_DISABLE_PROMPT=1 source "$venv_dir/bin/activate"
        fi
      else
        echo "Warning: .venv Python version ($venv_python_version) does not match .python-version ($required_version)"
      fi
    else
      uv venv --python "$required_version" "$venv_dir"
      VIRTUAL_ENV_DISABLE_PROMPT=1 source "$venv_dir/bin/activate"
    fi
  fi
}

function auto_activate_venv() {
  if ! command -v uv >/dev/null; then
    return
  fi

  if is_cached_dir_valid; then
    return
  fi

  local project_dir=$(validate_and_update_cache)
  if [[ -n $project_dir ]]; then
    activate_or_create_venv "$project_dir"
  elif [[ -n $VIRTUAL_ENV ]]; then
    deactivate
  fi
}

add-zsh-hook precmd auto_activate_venv
auto_activate_venv
