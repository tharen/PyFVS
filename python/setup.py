import os
from glob import glob

import setuptools
from setuptools import find_packages
from numpy.distutils.core import setup, Extension

# TODO: Get the version from the git tag and or revision.
version = '0.0.1a0'

if os.name=='nt':
    so_ext = 'pyd'
else:
    so_ext = 'so'

pyfvs_files = glob('pyfvs/*.{}'.format(so_ext))
pyfvs_files.extend(['pyfvs/pyfvs.cfg',])

description = open('./README.txt').readline().strip()
long_desc = open('./README.txt').read().strip()

setup(
    name='pyfvs'
    , version=version
    , description=description
    , long_description=long_desc
    , url='https://github.com/tharen/PyFVS'
    , author="Tod Haren"
    , author_email="tod.haren@gmail.com"
    , packages=find_packages()
    , include_package_data=True
    , data_files = [
            ('pyfvs',pyfvs_files)
            , ('pyfvs/docs',glob('pyfvs/docs/*'))
            , ('pyfvs/examples',glob('pyfvs/examples/*'))
            ]
    , entry_points={
            'console_scripts': [
            'fvs=pyfvs.__main__:main'
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
