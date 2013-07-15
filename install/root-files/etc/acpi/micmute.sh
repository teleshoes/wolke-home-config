#!/bin/bash
pulse_user=$(ps -o user `pgrep -f pulseaudio` 2>/dev/null | tail -1)
if [ -n "$pulse_user" ]; then
  su $pulse_user -c "pulse-vol mic toggle"
fi
