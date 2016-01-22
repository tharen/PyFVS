import os
from glob import glob

import setuptools
from setuptools import find_packages
from numpy.distutils.core import setup, Extension

if os.name=='nt':
    so_ext = 'pyd'
else:
    so_ext = 'so'

pyfvs_files = glob('pyfvs/*.{}'.format(so_ext))
pyfvs_files.extend(['pyfvs/pyfvs.cfg',])

setup(
    name='pyfvs'
    , description="Python wrappers for the Open-FVS libraries"
    , author="Tod Haren"
    , author_email="tod.haren<at>gm**l.com"
    , packages=find_packages()
    , include_package_data=True
    , data_files = [
            ('pyfvs',pyfvs_files)
            , ('pyfvs/docs',glob('pyfvs/docs/*'))
            , ('pyfvs/examples',glob('pyfvs/examples/*'))
            ]
    , entry_points={
            'console_scripts': [
            'fvs=pyfvs.fvs:main'
            ]
        }
    )
