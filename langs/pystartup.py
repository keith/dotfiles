# Add auto-completion and a stored history file of commands to your Python
# interactive interpreter. Requires Python 2.0+, readline. Autocomplete is
# bound to the Esc key by default (you can change it - see readline docs).
#
# Set an environment variable to point to it:  "export PYTHONSTARTUP=foo"
#
# Note that PYTHONSTARTUP does *not* expand "~", so you have to put in the full
# path to your home directory.

import atexit
import os
import readline
import rlcompleter

historyPath = os.path.expanduser("~/.pyhistory")


def save_history(path=historyPath):
    import readline
    readline.write_history_file(path)


if os.path.exists(historyPath):
    readline.read_history_file(historyPath)

readline.parse_and_bind("tab: complete")
readline.parse_and_bind(r"\C-a: beginning-of-line")
readline.parse_and_bind(r"\C-e: end-of-line")
atexit.register(save_history)
del os, atexit, readline, rlcompleter, save_history, historyPath

# vim: ft=python
