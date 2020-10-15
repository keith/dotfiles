import os


def Settings(language, filename, **kwargs):
    if language != "python":
        return None

    if os.path.isdir("venv"):
        return {
            "interpreter_path": os.path.abspath("venv/bin/python"),
        }

    return None
