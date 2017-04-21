import os
import sys
import shutil
import re

from setuptools import setup, Extension
from Cython.Build import cythonize
from Cython.Distutils import build_ext
import numpy

# NOTE: Python 2.7 C compiler for Windows
#       https://www.microsoft.com/en-us/download/details.aspx?id=44266
#       Python 3.4 requires VS 2010
#       Python 3.5 requires VS 2015

__version__ = re.search(
        r'__version__\s*=\s*[\'"]([^\'"]*)[\'"]',  # It excludes inline comment too
        open('pyfvs/__init__.py').read()).group(1)

description = open('./pyfvs/README.txt').readline().strip()
long_desc = open('./pyfvs/README.txt').read().strip()

if ((os.name=='nt') and (sys.version_info[:2]>=(3, 5))
        and (numpy.version.version<='1.13')):
    # Monkey patch numpy for MinGW until version 1.13 is mainstream
    # This is fixes the of building extensions with Python 3.5+ resultinging
    #       the error message `ValueError: Unknown MS Compiler version 1900`
    # numpy_fix uses the patch referenced here:
    #       https://github.com/numpy/numpy/pull/8355
    root = os.path.split(__file__)[0]
    sys.path.insert(0,os.path.join(root,'numpy_fix'))
    import misc_util, mingw32ccompiler
    sys.modules['numpy.distutils.mingw32ccompiler'] = mingw32ccompiler
    sys.modules['numpy.distutils.misc_util'] = misc_util

_is_64bit = (getattr(sys, 'maxsize', None) or getattr(sys, 'maxint')) > 2 ** 32
_is_windows = sys.platform == 'win32'

# Collect all Cython source files as a list of extensions
extensions = cythonize([
        Extension("pyfvs.*"
            , sources=["pyfvs/*.pyx"]
            , include_dirs=[numpy.get_include()]
            )])

setup(
    name='pyfvs'
    , version=__version__
    , description=description
    , long_description=long_desc
    , url='https://github.com/tharen/PyFVS'
    , author="Tod Haren"
    , author_email="tod.haren@gmail.com"
    , setup_requires=['cython', 'numpy>=1.11', 'pytest-runner']
    , tests_require=['pytest', ]
    , install_requires=['numpy>=1.11', ]  # 'numexpr']
    , ext_modules=extensions
    , packages=['pyfvs', 'pyfvs.keywords']
    , package_data={
            '':['*.pyd', '*.cfg', '*.so', 'README.*', 'version']
            , 'pyfvs':['docs/*', 'examples/*', 'test/*.py', 'test/rmrs/*']
            }
    # , include_package_data=True # package the files listed in MANIFEST.in
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
    )
