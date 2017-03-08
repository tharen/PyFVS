import os
import sys
import shutil

from setuptools import setup, Extension
from Cython.Build import cythonize
from Cython.Distutils import build_ext
import numpy

# NOTE: Python 2.7 C compiler for Windows
#       https://www.microsoft.com/en-us/download/details.aspx?id=44266
#       Python 3.4 requires VS 2010
#       Python 3.5 requires VS 2015

# TODO: Get the version from the git tag and or revision.
version = open('./pyfvs/version').readline().strip()

description = open('./pyfvs/README.txt').readline().strip()
long_desc = open('./pyfvs/README.txt').read().strip()

_is_64bit = (getattr(sys, 'maxsize', None) or getattr(sys, 'maxint')) > 2**32
_is_windows = sys.platform=='win32'

class build_ext_subclass( build_ext ):
    """
    Subclass build_ext to get the configured compiler vendor
    
    ref: http://stackoverflow.com/a/5192738/673590
    """
    def build_extensions(self):
        comp = self.compiler.compiler_type
        if comp=='mingw32' and _is_windows and _is_64bit:
            # MinGW w64 has problems link to MSVC compiled Python
            # A customized mingw spec file helps
            spec_file = './mingw-gcc.specs'
            v = sys.version_info[:2]
            
            if v >= (3, 3) and v <= (3, 4):
                d = {'msvcrt':'msvcr100', 'msvcrt_version':'0x1000'
                        , 'moldname':'moldname'}
                with open(spec_file + '.in') as infile:
                    open(spec_file, 'w').write(infile.read().format(**d))

            elif v >= (2, 6) and v <= (3, 2):
                d = {'msvcrt':'msvcr90', 'msvcrt_version':'0x0900'
                        , 'moldname':'moldname'}
                with open(spec_file + '.in') as infile:
                    open(spec_file, 'w').write(infile.read().format(**d))
            
            args = [
                    '-static-libgcc', '-static-libstdc++'
                    ,'-specs={}'.format(spec_file)
                    ,'-Wl,--allow-multiple-definition'
                    ]
            
            # Make sure all libraries know this is a 64 bit windows library
            define_macros = [('MS_WIN64', None), ]
            
            for e in self.extensions:
                e.extra_compile_args = args
                e.extra_link_args = args
                e.define_macros = define_macros
        
        build_ext.build_extensions(self)
        
# Collect all Cython source files as a list of extensions
extensions = cythonize([
        Extension("pyfvs.*"
            , sources=["pyfvs/*.pyx"]
            , include_dirs=[numpy.get_include()]
            )])

setup(
    name='pyfvs'
    , version=version
    , description=description
    , long_description=long_desc
    , url='https://github.com/tharen/PyFVS'
    , author="Tod Haren"
    , author_email="tod.haren@gmail.com"
    , setup_requires=['cython','numpy>=1.11','pytest-runner']
    , tests_require=['pytest',]
    , install_requires=['numpy>=1.11',] #'numexpr']
    , ext_modules=extensions
    , packages=['pyfvs','pyfvs.keywords']
    , package_data={
            '':['*.pyd','*.cfg','*.so','README.*','version']
            ,'pyfvs':['docs/*','examples/*','test/*.py','test/rmrs/*']
            }
    #, include_package_data=True # package the files listed in MANIFEST.in
    , entry_points={
            'console_scripts': [
            'pyfvs=pyfvs.__main__:main'
            ]
        }
    , classifiers=[
            'Development Status :: 3 - Alpha'
            , 'Environment :: Console'
            , 'Intended Audience :: Developers'
            , 'Intended Audience :: End Users/Desktop'
            , 'Intended Audience :: Science/Research'
            , 'Natural Language :: English'
            , 'Programming Language :: Python'
            , 'Programming Language :: Fortran'
            ]
    , keywords=''
    , cmdclass={'build_ext':build_ext_subclass},
    )
