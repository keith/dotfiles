# Add auto-completion and a stored history file of commands to your Python
# interactive interpreter. Requires Python 2.0+, readline. Autocomplete is
# bound to the Esc key by default (you can change it - see readline docs).
#
# Set an environment variable to point to it:  "export PYTHONSTARTUP=foo"
#
# Note that PYTHONSTARTUP does *not* expand "~", so you have to put in the full
# path to your home directory.

# Default imports for easier repl usage
import glob, fnmatch, re, os.path, pathlib, sys, subprocess  # noqa


def _main():
    import atexit, readline

    path = os.path.expanduser("~/.pyhistory")
    if os.path.exists(path):
        readline.read_history_file(path)

    try:
        import __builtin__
    except ImportError:
        import builtins as __builtin__

    setattr(__builtin__, "true", True)
    setattr(__builtin__, "false", False)
    setattr(__builtin__, "null", None)

    def save_history():
        import readline

        readline.write_history_file(path)

    readline.parse_and_bind("tab: complete")
    readline.parse_and_bind(r"\C-a: beginning-of-line")
    readline.parse_and_bind(r"\C-e: end-of-line")
    atexit.register(save_history)


if __name__ == "__main__":
    _main()
