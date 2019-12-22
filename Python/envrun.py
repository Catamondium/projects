#!/usr/bin/env python3
"""
Python virtualenv dispatcher
runs the given program as if source bin/activate were active
"""
from os import environ
from pathlib import Path
from copy import deepcopy
import subprocess


def searchPaths(prog, path=environ['PATH']) -> Path:
    """
    Get path to prog from PATH
    """
    paths = path.split(':')
    for p in map(Path, paths):
        for f in p.rglob('*'):
            if f.name == prog:
                return f


def execute(prog: Path, args, env, interpereter: str = 'python') -> int:
    """
    execute prog with args and env
    simply delegates to subprocess.call
    """
    return subprocess.call([interpereter, str(prog)] + args,
                           shell=False, env=env)


def envmod(prog: Path, env):
    """
    Create the virtualenv environment
    """
    newenv = deepcopy(environ)
    venv = prog.parent
    newenv["VIRTUAL_ENV"] = str(venv)
    newenv["PATH"] = f"{venv / Path('bin')}:{environ['PATH']}"
    return newenv


if __name__ == "__main__":
    from argparse import ArgumentParser, REMAINDER
    parser = ArgumentParser(description="virtualenv resolver")
    parser.add_argument(
        "prog", help="python program to run. NOTE: if searched in PATH, PATH must be a symlink to the venv")
    parser.add_argument("--path", default=environ['PATH'])
    parser.add_argument("--py", default="python",
                        help="python version to run")
    parser.add_argument("args", nargs=REMAINDER,
                        help="args to pass to prog")

    argv = parser.parse_args()
    if Path(argv.prog).resolve().is_absolute():
        pyprog = Path(argv.prog)
    else:
        pyprog = searchPaths(argv.prog, path=argv.path)
    if not pyprog:
        print(f"Couldn't find {argv.prog}")
        exit(1)
    pyprog = pyprog.resolve()
    newenv = envmod(pyprog, environ)
    exit(execute(pyprog, argv.args, newenv, interpereter=argv.py))
