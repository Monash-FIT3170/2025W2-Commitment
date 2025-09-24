#!/usr/bin/env bash

# The directory to use data from
USERNAME="python"

PROCESS_UUID="$(uuidgen)"
WORKDIR="/home/python"

OVERLAY_ROSRC="$(pwd)"

bwrap_opts=()

# paths shared read only by default
paths_general=(
  /bin
  /usr
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
PROGRAM_NAME="${PROGRAM#$PROGRAM_PARENT_DIR/}"
echo "Program dir $PROGRAM_PARENT_DIR"
echo "Program name $PROGRAM_NAME"

if [ -f "$PROGRAM" ]; then
  # The given program is at some path
#  bwrap_opts+=(--ro-bind "$PROGRAM_PARENT_DIR" "$PROGRAM_PARENT_DIR")
  echo ""
else
  PROGRAM="$(which "$1")"
  PROGRAM_PARENT_DIR="${PROGRAM%/*}"

  if [ -f "$PROGRAM" ]; then
    echo "$1 is at $PROGRAM"
    # The given program is at some path
#    bwrap_opts+=(--ro-bind "$PROGRAM_PARENT_DIR" "$PROGRAM_PARENT_DIR")
  else
    echo "$1 Not a directory, nor an executable"
  fi
fi


# --tmpfs /hidden/overlay/workdir \
#  --ro-bind "$OVERLAY_ROSRC" /hidden/overlay/rosrc \
# --setenv PATH /bin:/usr/bin \
# --ro-bind /nix/store/ih779chzzag1nm91fgnrndml4mghm3la-coreutils-9.7 /nix/store/ih779chzzag1nm91fgnrndml4mghm3la-coreutils-9.7 \

exec bwrap \
  --chdir "$WORKDIR" \
  --unshare-all \
  --clearenv \
  --dev /dev \
  --proc /proc \
  --tmpfs /tmp \
  --overlay-src "$OVERLAY_ROSRC" \
  --tmp-overlay "$WORKDIR" \
  --hostname python \
  --setenv USERNAME python \
  --die-with-parent \
  --dir /container \
  --chmod 0755 /container \
  --setenv PATH /container/bin \
  --setenv HOME /home/python \
  --ro-bind "$PROGRAM_PARENT_DIR" /container/bin \
  --ro-bind /nix /nix \
  "${bwrap_opts[@]}" \
  "${@}"
