#!/usr/bin/env python3
"""Grok lifecycle notify hook. Always exit 0. No extra packages."""

from __future__ import annotations

import json
import os
import subprocess
import sys
from datetime import datetime
from pathlib import Path

LOG = Path.home() / ".grok" / "logs" / "notify.log"
SOUND = os.environ.get("GROK_NOTIFY_SOUND", "Glass")


def log(msg: str) -> None:
    LOG.parent.mkdir(parents=True, exist_ok=True)
    with LOG.open("a") as f:
        f.write(f"{datetime.now():%Y-%m-%dT%H:%M:%S} {msg}\n")


def read_payload() -> dict:
    raw = sys.stdin.read()
    if not raw.strip():
        return {}
    try:
        data = json.loads(raw)
        return data if isinstance(data, dict) else {}
    except json.JSONDecodeError:
        return {}


def event_from(payload: dict) -> tuple[str, str, str]:
    """Return (event, message, skip_reason). skip_reason empty means notify."""
    sub = payload.get("subagentType") or payload.get("subagent_type") or ""
    reason = payload.get("reason") or ""
    hook = (
        payload.get("hookEventName")
        or os.environ.get("GROK_HOOK_EVENT")
        or os.environ.get("GROK_EVENT")
        or ""
    ).lower()
    ntype = (
        payload.get("notificationType") or payload.get("notification_type") or ""
    ).lower()
    message = payload.get("message") or os.environ.get("GROK_MESSAGE") or ""

    if sub:
        return "turn_complete", message, "subagent"
    if hook in {"stop"} and reason and reason != "end_turn":
        return "turn_complete", message, f"reason:{reason}"

    if ntype == "permission_prompt" or hook in {"approval_required"}:
        event = "approval_required"
    elif ntype == "task_complete" or hook == "task_complete":
        event = "task_complete"
    elif hook in {"stop_failure", "stopfailure", "agent_error"}:
        event = "agent_error"
    else:
        event = "turn_complete"
    return event, message, ""


def tmux_window_active() -> bool:
    if not os.environ.get("TMUX"):
        return True
    target = os.environ.get("TMUX_PANE")
    cmd = ["tmux", "display-message", "-p"]
    if target:
        cmd.extend(["-t", target])
    cmd.append("#{window_active}")
    try:
        out = subprocess.run(cmd, capture_output=True, text=True, timeout=2)
        return out.stdout.strip() == "1"
    except (OSError, subprocess.TimeoutExpired):
        return True


def ghostty_frontmost() -> bool:
    try:
        out = subprocess.run(
            [
                "osascript",
                "-e",
                'tell application "System Events" to get name of first process whose frontmost is true',
            ],
            capture_output=True,
            text=True,
            timeout=3,
        )
        name = out.stdout.strip()
        return name.lower() == "ghostty"
    except (OSError, subprocess.TimeoutExpired):
        return False


def looking_at_grok() -> bool:
    return tmux_window_active() and ghostty_frontmost()


def title_and_default(event: str) -> tuple[str, str]:
    return {
        "approval_required": ("Grok needs you", "Waiting for approval."),
        "agent_error": ("Grok error", "The last turn hit an error."),
        "task_complete": ("Grok task done", "A background task finished."),
        "session_ready": ("Grok ready", "Session is ready."),
    }.get(event, ("Grok done", "Turn finished."))


def banner(title: str, message: str) -> None:
    script = """
on run argv
  set theTitle to item 1 of argv
  set theMessage to item 2 of argv
  set theSound to item 3 of argv
  display notification theMessage with title theTitle sound name theSound
end run
"""
    subprocess.run(
        ["osascript", "-", title, message, SOUND],
        input=script,
        text=True,
        capture_output=True,
        timeout=5,
    )


def main() -> int:
    payload = read_payload()
    event, message, skip = event_from(payload)
    hook = os.environ.get("GROK_HOOK_EVENT", "")
    pane = os.environ.get("TMUX_PANE", "")

    if skip:
        log(f"skip event={event} ({skip}) hook={hook}")
        return 0

    if not os.environ.get("GROK_NOTIFY_FORCE") and looking_at_grok():
        log(f"skip event={event} (looking at grok) hook={hook} pane={pane}")
        return 0

    title, default = title_and_default(event)
    text = " ".join((message or default).replace("\r", " ").split())
    if len(text) > 220:
        text = text[:217] + "..."

    log(f"notify event={event} title={title} hook={hook} pane={pane}")
    try:
        banner(title, text)
    except (OSError, subprocess.TimeoutExpired) as exc:
        log(f"osascript failed: {exc}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
