"""
Determine the platform specific file name for a compiled extension and print it.
"""

import os
import sys
import re

if sys.version_info[:2]>=(3,2):
    from importlib import machinery as m
    abi = m.EXTENSION_SUFFIXES[0]
    if re.match('.*\.(so|pyd)',abi):
      ext = abi
    else:
        if sys.platform=='win32':
            ext = ext = '{}{}'.format(abi,'pyd')
        else:
            ext = ext = '{}{}'.format(abi,'so')
    
    fn = '{}{}'.format(sys.argv[1],ext)
    print(fn)

else:
    if sys.platform=='win32':
        ext = 'pyd'
    else:
        ext = 'so'
    fn = '{}.{}'.format(sys.argv[1],ext)
    print(fn)
    