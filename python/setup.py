import os
import shutil

from setuptools import setup, Distribution

# TODO: Get the version from the git tag and or revision.
version = '0.0.1a0'

if os.name=='nt':
    so_ext = 'pyd'
else:
    so_ext = 'so'

description = open('./README.txt').readline().strip()
long_desc = open('./README.txt').read().strip()

shutil.copyfile('./README.txt', 'pyfvs/README.txt')

# Force setuptools to make a platform dependent distribution
# http://lucumr.pocoo.org/2014/1/27/python-on-wheels/
class BinaryDistribution(Distribution):
    def is_pure(self):
        return False

setup(
    name='pyfvs'
    , version=version
    , description=description
    , long_description=long_desc
    , url='https://github.com/tharen/PyFVS'
    , author="Tod Haren"
    , author_email="tod.haren@gmail.com"
    , test_requires=['nose2','nose-parameterized']
    , install_requires=['numpy>=1.9',]
    , distclass=BinaryDistribution
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
