"""
PyFVS

Modules and objects for executing and interacting with FVS variants.

@author: Tod Haren, tod.haren at gmail
"""

__author__ = 'Tod Haren, tod.haren at gmail'

import os
import sys
import logging
import logging.config

# # Bring standard modules into the package namespace
# from .keywords.keywords import *
# from .keywords.eventmonitor import *
# from .fvs import FVS, FVSTrees

# If the __version__ file is present, use it.
from ._version import __version__, __status__, __git_tag__

# TODO: Look in local path as well as user home path
# Use a config file written as a Python dictionary.
# The config file is used to initialize logging and FVS library paths.
config_path = os.path.join(os.path.split(__file__)[0], 'pyfvs.cfg')

def version():
    """Return the current PyFVS API version number."""
    return __version__

treelist_format = {
    'template':(
        '{plot:04d}{tree:04d}{tpa:>7.1f}{status:1d}{species:2s}'
        '{diam:>5.1f}000{height:>3.0f}{cr_code:>7d}{age:>27d}')
    , 'fvs_format':(
        'I4,T1,I8,F7.1,I1,A2,F5.1,F3.1,F3.0,F3.0,F3.1'
        ',I1,6I2,2I1,I2,2I3,2I1,F3.0'
        )
    }

def get_config():
    """
    Return the configuration dict.
    """
    try:
        with open(config_path) as foo:
            cfg = eval(foo.read())

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
