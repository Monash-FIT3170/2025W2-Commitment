#!/usr/bin/env bash

# The directory to use data from
# PROCESS_UUID="$(uuidgen)"
WORKDIR="/home/python"

# Directory to make available to the container at ~
# Currently gives access to the directory the container was run in
OVERLAY_ROSRC="$(pwd)"

bwrap_opts=()
# ignored=()

ROOT_FS=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --store-paths)
            shift
            STORE_FILE="$1"
            shift

            echo "Reading store paths file $STORE_FILE"

            STORE_PATHS=$(<"$STORE_FILE")
            for p in $STORE_PATHS; do
                bwrap_opts+=(--ro-bind-try "$p" "$p")
            done
            ;;
        --root-fs)
            shift
            ROOT_FS="$1"
            shift

            echo "Using $ROOT_FS as root filesystem"

            for i in "${ROOT_FS}"/*; do
                path="${i##*/}"
                if [[ $path == '/etc' ]]; then
                    :
                elif [[ -L $i ]]; then
                    bwrap_opts+=(--symlink "$(readlink "$i")" "$path")
                    # ignored+=("$path")
                else
                    bwrap_opts+=(--ro-bind "$i" "$path")
                    # ignored+=("$path")
                fi
            done
            ;;
        --)
            shift
            while [[ $# -gt 0 ]] && [[ "$1" != "--" ]]; do
                EXTRA_COMMAND="$1"
                shift
                echo "  -- $EXTRA_COMMAND"
                bwrap_opts+=("$EXTRA_COMMAND")
            done

            if [[ "$1" == "--" ]]; then
              shift
            fi
            ;;
        *)
            break
            ;;
    esac
done

# Get the executable to run, so we can symlink it to the container filesystem
CONTAINER_PATH="/bin:/usr/bin"

echo ""
echo "Running container:" "$@"
echo ""

# Run bwrap to create/enter a sandbox environment
# Notes:
#   - Docker doesn't support:
#       --proc /proc
#       --dev /dev

exec bwrap \
  --unshare-all \
  --cap-drop all \
  --uid 1000 \
  --gid 1000 \
  --clearenv \
  --tmpfs /tmp \
  --dir /etc \
  --dir /usr \
  --overlay-src "$OVERLAY_ROSRC" \
  --tmp-overlay "$WORKDIR" \
  --ro-bind-try "$FAKE_PASSWD_FILE" /etc/passwd \
  --setenv USERNAME python \
  --die-with-parent \
  --dir /container \
  --chmod 0755 /container \
  --setenv PATH "$CONTAINER_PATH" \
  --setenv HOME /home/python \
  --chdir "$WORKDIR" \
  "${bwrap_opts[@]}" \
  "${@}"