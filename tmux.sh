#!/usr/bin/env bash


dir="$HOME/.myprojs/programming-how-to"
ses='proghowto'

if [ ! -d "$dir" ] ; then
    echo "Directory \`$dir\` not found."
    echo "The project “$ses” doesn't seem to be exist this machine."
    echo "Bailing out..."
    exit 1
fi

cd "$dir"

tmux new-session -d -s "$ses" -c "$dir" \; rename-window 'vim' \; \
    send-keys 'vim -c NERDTree' C-m

tmux new-window -t "${ses}:2" -n git -c "$dir" \; \
    send-keys 'git status' C-m

tmux new-window -t "${ses}:3" -n shell -c "$dir"

tmux select-window -t "${ses}:1"
tmux -2 attach-session -t "$ses"


