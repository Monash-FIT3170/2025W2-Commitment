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

echo "Found $PROGRAM at $PROGRAM_PARENT_DIR"
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
  --ro-bind /nix /nix \
  --chdir "$WORKDIR" \
  "${bwrap_opts[@]}" \
  "${@}"
