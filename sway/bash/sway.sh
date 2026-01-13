kitty-os-windows() {
    local num=${1:-1}

    for ((i=0; i<num; i++)); do
        kitty @ launch --type=os-window --cwd=current
    done
}

focus-num-tabs-left() {
    local num=${1:-1}
    local i
    for i in $(seq $num); do
        swaymsg focus left
    done
}

kitty-os-windows-to-right() {
    local num=${1:-1}

    kitty-os-windows ${num}
    focus-num-tabs-left ${num}
}

# Assume a kitty window alone on its display.
# Set up so that we have two side-by-side stacks of ${num} windows.
kitty-tab-then-move-right() {
    local num=${1:-1}

    # 1) Ensure we're in tabbed layout so the new kitty window becomes a tab
    swaymsg -q layout tabbed

    # 2) Open new kitty OS windows (same instance, same cwd)
    kitty-os-windows ${num}

    # 3) Move one of the newly-created (focused) window to the right,
    #    creating a split.  The sleep gives sway a moment to map/focus
    #    the new window, which will work most of the time if the
    #    machine is not heavily burdened.
    sleep 0.1
    swaymsg -q move right
    kitty-os-windows $((${num} - 1));
    sleep 0.1
    focus-num-tabs-left $((2 * ${num} - 1))
}

emf() {
    local num=${1:-1}

    local one_less=$((${num} - 1))
    emacs --eval="(progn
      (shell-command \"swaymsg -q move left\")
      (dotimes (_ ${one_less})
        (make-frame-command)))" &
}
# Shortcut for opening some extra windows in the current stack.
k-os() { kitty-os-windows $*; }
k-osr() { kitty-os-windows-to-right $*; }
# Shortcut for opening two stacks of n kitty windows.
k-n() { kitty-tab-then-move-right $*; }
# Shortcut for opening two stacks of n kitty windows and four emacs frames.
k-e() { kitty-tab-then-move-right $*; emf 4; }
