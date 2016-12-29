from distutils.core import setup
from distutils.extension import Extension
from Cython.Build import cythonize

extensions = [
    Extension(
        'pyfvspnc'
        , ['pyfvspnc.pyx',]
        , libraries=['fvspnc','odbc','ltdl']
        , library_dirs=['../build',]
        )
    ]

setup(
    name = 'PyFVS'
    , ext_modules = cythonize(extensions)
)

