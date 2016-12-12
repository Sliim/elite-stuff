#!/bin/bash
export TERM=xterm
/usr/bin/script -q -c "/usr/bin/tmux new-session -c /root/.msf4 '/msf/msfconsole -r /msf/startup.rc'"
