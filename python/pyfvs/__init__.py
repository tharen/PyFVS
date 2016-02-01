"""
PyFVS

Modules and objects for executing and interacting with FVS variants.

@author: tod.haren@gmail.com
"""

import os
import sys
import re
import logging
import logging.config

# TODO: Look in local path as well as user home path
# Use a config file written as a Python dictionary.
# The config file is used to initialize logging and FVS library paths.
config_path = os.path.join(os.path.split(__file__)[0], 'pyfvs.cfg')

# TODO: Get the version from a package file, git tag, etc.
__version__ = '0.0.1a0'

def version():
    """Return the current PyFVS API version number."""
    return __version__

def get_config():
    """
    Return the configuration dict.
    """
    try:
        cfg = eval(open(config_path).read())

    except:
        cfg = {
            'logging':{
                'version':1
                , 'disable_existing_loggers':True
                , 'incremental':False
                }
            }

    return cfg

def init_logging():
    """
    Initialize package wide logging from the configuration file.
    """
    logging.config.dictConfig(get_config()['logging'])

def list_variants():
    """
    Return a list of available FVS variants.
    """
    import glob
    root = os.path.dirname(__file__)
    if os.name == 'nt':
        so = '.pyd'
    else:
        so = '.so'
    _vars = glob.glob(os.path.join(root, '*{}'.format(so,)))
    vars = []
    for var in _vars:
        libname = os.path.splitext(os.path.basename(var))[0]
        if not (libname.startswith('py') or libname.startswith('_py')):
            continue

        vars.append(libname[5:7].upper())

    return vars

def platform_ext():
    """
    Return the platform specific extension filename suffix.
    """
    if sys.version_info[:2] >= (3, 2):
        from importlib import machinery as m
        abi = m.EXTENSION_SUFFIXES[0]
        if re.match('.*\.(so|pyd)', abi):
          ext = abi
        else:
            if sys.platform == 'win32':
                ext = ext = '{}{}'.format(abi, 'pyd')
            else:
                ext = ext = '{}{}'.format(abi, 'so')

        return ext

    else:
        if sys.platform == 'win32':
            ext = 'pyd'
        else:
            ext = 'so'

        return ext
