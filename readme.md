# Projects readme
This repo is intended for collected small-medium projects across various languages.

Addtime files are simple utility programs I've done for learning syntax, all they do is convert hh:mm +mins to an end time, initially this was used to adapt cinema viewings in that format for putting into my calendar without the fuss of mental arithmetic.

# Notable projects
## Notably large projects
The largest projects inhabit /Java/Processing, most notably Tetris and Snake; many of these projects are common artistic simulations or clones of relatively simplistic games. Some of these sims use significant data structures to optimise complexity, hence, two versions of the Autoagents sym exist; One uses a basic linear search when evaluating current state.

The other uses spatial partitioning to evaluate an agent's local environment in O(nLog(n)) time on average, one caveat being that the tree is rebuilt on every update, which by itself takes linear time guaranteed. This is to isolate the preceding system from the system with forced applied as agents evaluate their environment such that it isn't skewed by the state proceeding it.
# Common installation patterns
##  Building executables
All buildable sources should be assumed to be non-portable with exclusive support for for Unix environments. Exceptions to this rule are minimal particularly within C/C++ projects, calling directly into `unistd.h` for a variety of system calls.

Presuming a standard Unix environment is established, all single-file programs should build with minimal additional preparation, and my be compiled with the relevant compiler, `gcc` or `cc` for C/C++ programs, `rustc` for Rust programs.

C/C++ programs spread across several sources have a file `Makefile` alongside them, this is sufficient to build such programs.
## Python virtual environments
Any python programs not headed with the shebang `#/bin/envrun` will run as-is with no additional prep, unless extremely small or little used by myself, a help printout should be provided by the `-h` flag.

If a program does have the envrun header, the envrun program is included in the general python directory an can run as is, symlinking it in /usr/bin/envrun with executable permissions will be enough to run venv programs.

As a matter of disclosure, the repo is solely operated by myself with public domain licensing, the envrun prog modifies the prog running environment as if `. ./bin/activate` were run for the shebang'd prog, beyond that it chooses the interpreting Python binary by a flag in the shebang; defaulting to Python3 as my system-wide default.

## Calling into OAuth apis
Naturally, for security reasons, development and client tokens are excluded from repos requiring authorisation for rate management and user security. The common format in my programs is to decouple these tokens to an on-system plaintext file pulled in by the relevant program.

Development token filenames can be found with a simple string search in the program's main file, OAuth is always one of the functions at the very top of my main files, so the filename should be very easy to determine.

Where programs run standard flow to authorise clients automatically, a client token ought not be provided, programs using nonstandard authorisation will simply fail if one is not provided. Tokens are always searched for at the scope of the programs execution directory, usually where it or the symlink its running through is located; where programs are capable, they resolve their own symlink to the repo directory, so it does vary between programs.
