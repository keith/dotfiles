# Add auto-completion and a stored history file of commands to your Python
# interactive interpreter. Requires Python 2.0+, readline. Autocomplete is
# bound to the Esc key by default (you can change it - see readline docs).
#
# Set an environment variable to point to it:  "export PYTHONSTARTUP=foo"
#
# Note that PYTHONSTARTUP does *not* expand "~", so you have to put in the full
# path to your home directory.

import os.path
from pathlib import Path as _Path

Path = _Path

_JSON_BUILTINS = {
    "true": True,
    "false": False,
    "null": None,
}


# https://mamot.fr/@mdk/115342681772436494
class _BuiltinsWrapper(dict):
    __slots__ = ()

    def __missing__(self, key):
        if value := __import__("builtins").__dict__.get(key):
            return value

        try:
            return __import__(key)
        except ImportError:
            raise NameError(f"name {key!r} is not defined")


__builtins__ = _BuiltinsWrapper(_JSON_BUILTINS)


def _main():
    import atexit, readline

    path = os.path.expanduser("~/.pyhistory")
    if os.path.exists(path):
        readline.read_history_file(path)

    def save_history():
        import readline

        readline.write_history_file(path)

    readline.parse_and_bind("tab: complete")
    readline.parse_and_bind(r"\C-a: beginning-of-line")
    readline.parse_and_bind(r"\C-e: end-of-line")
    atexit.register(save_history)


if __name__ == "__main__":
    _main()
