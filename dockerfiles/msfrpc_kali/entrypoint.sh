#!/bin/bash
export TERM=xterm
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
/usr/bin/script -q -c "/usr/bin/tmux new-session './msfconsole -r startup.rc'"
