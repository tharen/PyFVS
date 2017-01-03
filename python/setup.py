import shutil

from setuptools import setup
from distutils.extension import Extension
from Cython.Build import cythonize
import numpy

extensions = []

variants = ['pnc',] #'wcc','cac','soc']
for variant in variants:
    cython_ext = 'pyfvs{}.pyx'.format(variant)
    shutil.copy('pyfvsvar.pyx.in',cython_ext)
    ext = Extension(
        'pyfvs{}'.format(variant)
        , [cython_ext,]
        , libraries=['fvs{}'.format(variant),]
        , library_dirs=['../build',]
        , include_dirs=[numpy.get_include(),]
        )
    extensions.append(ext)

setup(
    name = 'PyFVS'
    , ext_modules = cythonize(extensions)
    , setup_requires=['pytest-runner',]
    , tests_require=['pytest',]
)

