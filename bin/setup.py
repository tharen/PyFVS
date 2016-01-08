from __future__ import division, absolute_import, print_function

import os
from numpy.distutils.core import Extension

fvs_variant = 'pnc'
mod_path ='./pnc'
ext_name = 'pyfvspnc'
ext_pyf = './pyfvspnc.pyf'
fvs_lib = 'FVSpnc_static'
lib_dir = '.'

if os.name=='nt':
    odbc = 'odbc32'
else:
    odbc = 'odbc'
    
ext1 = Extension(
        name = ext_name,
        sources = [ext_pyf,],
        libraries = [fvs_lib,odbc],
        library_dirs = [lib_dir,],
        #extra_compile_args = ['-J={}'.format(mod_path),]
)

if __name__ == "__main__":
    from numpy.distutils.core import setup
    setup(name = ext_name,
          description       = "F2PY wrappers for the Open-FVS library ({})".format(fvs_variant.upper()),
          author            = "Tod Haren",
          author_email      = "tod.haren<at>gm....com",
          ext_modules = [ext1,],
          build_ext = ['--compiler=mingw32','--fcompiler=gnu',]
          )
