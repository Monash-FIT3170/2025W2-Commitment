#!/usr/bin/env bash

# The directory to use data from
# PROCESS_UUID="$(uuidgen)"
WORKDIR="/home/python"

# Directory to make available to the container at ~
# Currently gives access to the directory the container was run in
OVERLAY_ROSRC="$(pwd)"

bwrap_opts=()
while [[ $# -gt 0 ]]; do
    case $1 in
        --store-paths)
            shift
            STORE_FILE="$1"
            shift

            echo "Reading store paths file $STORE_FILE"

            STORE_PATHS=$(<"$STORE_FILE")
            for p in $STORE_PATHS; do
                bwrap_opts+=(--ro-bind "$p" "$p")
            done
            ;;
        *)
            break
            ;;
    esac
done


# paths shared read only by default
paths_general=(
  /bin
  /lib
  /lib64
)
for p in "${paths_general[@]}"; do
  if [ -d "$p" ]; then
    bwrap_opts+=(--ro-bind "$p" "$p")
  fi
done

# Get the executable to run, so we can symlink it to the container filesystem

PROGRAM="$1"
PROGRAM_PARENT_DIR="${PROGRAM%/*}"

if ! [ -f "$PROGRAM" ]; then
  PROGRAM="$(which "$1")"
  PROGRAM_PARENT_DIR="${PROGRAM%/*}"

  if ! [ -f "$PROGRAM" ]; then
    echo "$1 is not a directory, nor an executable"
    exit 1
  fi
fi

# Invent a fake user
FAKE_PASSWD_FILE="/tmp/fake_wrap_passwd"
if ! [ -f "$FAKE_PASSWD_FILE" ]; then
  echo "python:x:1001:1001:Fake User:/home/python:/bin/bash" > "$FAKE_PASSWD_FILE"
fi

# Function: scan /bin (or any directory) for symlinks and add --ro-bind for linked directories
bind_symlink_dirs() {
    local dir="$1"
    local -n opts_ref="$2"  # pass array by reference

    echo "bind_symlink_dirs $dir"

    for f in "$dir"/*; do
        [ -e "$f" ] || continue  # skip if file doesn't exist

        if [ -L "$f" ]; then
            # Resolve symlink to the real file
            local real_path
            real_path=$(readlink -f "$f")

            if [[ "$real_path" == /nix/store/* ]]; then
                # extract store root
                # Strip "/nix/store/" prefix
                store_tail="${real_path#/nix/store/}"
                # Extract first component (hash-name folder)
                store_root="${store_tail%%/*}"
                # Prepend "/nix/store/"
                store_root="/nix/store/$store_root"

                # Avoid duplicates
                local already_added=false
                for opt in "${opts_ref[@]}"; do
                    if [[ "$opt" == "--ro-bind $store_root $store_root" ]]; then
                        already_added=true
                        break
                    fi
                done

                if ! $already_added; then
#                    echo "Binding symlink -> $store_root"
                    opts_ref+=(--ro-bind "$store_root" "$store_root")
                fi
            fi

        fi
    done
}

# Function: scan executables for shared libraries and add --ro-bind for needed /nix/store paths
bind_needed_libraries() {
    local dir="$1"
    local -n opts_ref="$2"  # pass array by reference

    echo "bind_needed_libraries $dir"

    # Find all executable files in the directory
    find "$dir" -type f -executable | while read -r bin; do
        # Skip non-ldd-compatible files
        [ -x "$bin" ] || continue

        # Use ldd to list libraries
        while IFS= read -r lib; do
            # Only process /nix/store libraries
            if [[ "$lib" == /nix/store/* ]]; then
                # Extract store root
                store_tail="${lib#/nix/store/}"
                store_root="${store_tail%%/*}"
                store_root="/nix/store/$store_root"

                # Avoid duplicates
                already_added=false
                for opt in "${opts_ref[@]}"; do
                    if [[ "$opt" == "--ro-bind $store_root $store_root" ]]; then
                        already_added=true
                        break
                    fi
                done

                if ! $already_added; then
#                    echo "Binding library -> $store_root"
                    opts_ref+=(--ro-bind "$store_root" "$store_root")
                fi
            fi
        done < <(ldd "$bin" 2>/dev/null | awk '/\/nix\/store/ {print $3}')
    done
}

# TODO: Find a way to get any store paths found in this step as part of the nix build process instead, to speed up container start times
bind_symlink_dirs "/bin" bwrap_opts
#bind_symlink_dirs "/usr/bin" bwrap_opts
#bind_symlink_dirs "/lib" bwrap_opts
#bind_symlink_dirs "/lib64" bwrap_opts

#bind_needed_libraries "/bin" bwrap_opts
#bind_needed_libraries "/usr/bin" bwrap_opts
#bind_needed_libraries "/lib" bwrap_opts
#bind_needed_libraries "/lib64" bwrap_opts



echo ""
echo "Found $PROGRAM at $PROGRAM_PARENT_DIR"
echo "Running container:" "$@"
echo ""

exec bwrap \
  --unshare-all \
  --clearenv \
  --dev /dev \
  --proc /proc \
  --tmpfs /tmp \
  --dir /etc \
  --dir /usr \
  --overlay-src "$OVERLAY_ROSRC" \
  --tmp-overlay "$WORKDIR" \
  --hostname python \
  --uid 1001 \
  --gid 1001 \
  --ro-bind "$FAKE_PASSWD_FILE" /etc/passwd \
  --setenv USERNAME python \
  --die-with-parent \
  --dir /container \
  --chmod 0755 /container \
  --setenv PATH "/bin:/usr/bin:/container/bin" \
  --setenv HOME /home/python \
  --ro-bind "$PROGRAM_PARENT_DIR" /container/bin \
  --chdir "$WORKDIR" \
  "${bwrap_opts[@]}" \
  "${@}"
