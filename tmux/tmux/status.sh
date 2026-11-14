#!/bin/sh
# status.sh — tmux status-right helpers (kept out of tmux.conf to avoid
# strftime/% escaping). Usage: status.sh cpu | mem
case "$1" in
  cpu)
    # Sum of per-process %CPU (user + sys) normalised to all cores.
    ps -A -o %cpu= | awk -v n="$(sysctl -n hw.ncpu)" '{s+=$1} END {printf "%d%%", s/n + 0.5}'
    ;;
  mem)
    # Available = free + inactive + speculative pages (what macOS can hand out
    # without swapping). "Pages free" alone reads near zero on a healthy Mac.
    vm_stat | awk '
      /page size of/      {ps=$8}
      /Pages free/        {a+=$3}
      /Pages inactive/    {a+=$3}
      /Pages speculative/ {a+=$3}
      END {printf "%.1fG", a*ps/1073741824}'
    ;;
esac
