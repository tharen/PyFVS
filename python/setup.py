import os
import shutil

from setuptools import setup, Extension
from Cython.Build import cythonize
import numpy

# NOTE: Python 2.7 C compiler for Windows
#       https://www.microsoft.com/en-us/download/details.aspx?id=44266
#       Python 3.4 requires VS 2010
#       Python 3.5 requires VS 2015

# TODO: Get the version from the git tag and or revision.
version = '0.0.1a0'

if os.name=='nt':
    so_ext = 'pyd'
    extra_link_args = []
    extra_compile_args = []
else:
    extra_link_args = []
    extra_compile_args = []
    so_ext = 'so'

description = open('./README.txt').readline().strip()
long_desc = open('./README.txt').read().strip()

shutil.copyfile('./README.txt', 'pyfvs/README.txt')

# Collect all Cython source files as a list of extensions
extensions = [
        Extension("pyfvs.*", ["pyfvs/*.pyx"]
            , extra_link_args=extra_link_args
            , extra_compile_args=extra_compile_args
            )]

setup(
    name='pyfvs'
    , version=version
    , description=description
    , long_description=long_desc
    , url='https://github.com/tharen/PyFVS'
    , author="Tod Haren"
    , author_email="tod.haren@gmail.com"
    , setup_requires=['cython','numpy>=1.9',]
    , tests_require=['nose2','nose-parameterized']
    , install_requires=['numpy>=1.9',] #'numexpr']
    , ext_modules = cythonize(extensions)
    , include_dirs=[numpy.get_include()]
    , packages=['pyfvs',]
    , include_package_data=True # package the files listed in MANIFEST.in
    , entry_points={
            'console_scripts': [
            'fvs=pyfvs.__main__:main'
            ]
        }
    , test_suite='nose2.collector.collector'
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
