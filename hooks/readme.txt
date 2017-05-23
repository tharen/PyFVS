This folder contains Git hooks and supporting files used by PyFVS developers. 
To enable them simply create a symbolic link to the .git/hooks folder for each 
hook you wish to enable. As with *nix, symbolic links can be created on Windows
using a Git bash console.

Hooks:

pre-commit

    Executes clean_version.py to wipe any Git metadata from _version.py prior
    to committing local changes.
    
    To install:  `ln -s ./hooks/pre-commit .git/hooks/pre-commit`
    