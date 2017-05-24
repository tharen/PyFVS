import os
import sys
import shutil
import re
import subprocess

from setuptools import setup, Extension, Command
from Cython.Build import cythonize
from Cython.Distutils import build_ext
import numpy

# NOTE: Python 2.7 C compiler for Windows
#       https://www.microsoft.com/en-us/download/details.aspx?id=44266
#       Python 3.4 requires VS 2010
#       Python 3.5 requires VS 2015

description = open('./pyfvs/README.txt').readline().strip()
long_desc = open('./pyfvs/README.txt').read().strip()

version_path = 'pyfvs/_version.py'

def update_version():
    """
    Update the contents of _version.py with Git metadata.
    """
    try:
        r = subprocess.check_output(['git',])
        
    except:
        print('Error: git must be available in the PATH environment.')
        raise
        
    contents = open(version_path).read()
    s = re.search(r'__version__ = \'(?P<version>.*)\'', contents)
    version = s.group('version')
    
    commit = subprocess.check_output(
            ['git', 'rev-parse', '--short', '--verify', 'HEAD']
            ).decode('utf-8').strip()
    
    # Check for dirty state
    r = subprocess.check_output(
            ['git', 'status', '--short', '-uno']
            ).decode('utf-8').strip()
    if r:
        commit += '-dirty'
    
    contents = re.sub(
            r"(__git_commit__) = '(.*)'"
            , r"\1 = '{}'".format(commit)
            , contents)
    
    try:
        desc = subprocess.check_output(
                ['git', 'describe', '--tags', '--dirty']
                ).decode('utf-8').strip()
        
        # Check the most recent tag version
        m = re.match('pyfvs-v(?P<version>\d+\.\d+\.\d+)(.*)', desc)
        if m:
            tag_version = m.group('version')
        else:
            tag_version = version
        
        # If this is a more recent version, e.g. release canditate
        if version>tag_version:
            desc = ''
    
    except:
        # For shallow clones git describe may fail.
        desc = ''
        
    contents = re.sub(
            r"(__git_describe__) = '(.*)'"
            , r"\1 = '{}'".format(desc)
            , contents)

    fn = os.path.join(os.path.dirname(__file__), version_path)
    with open(fn, 'w') as fp:
        fp.write(contents)

    print('Updated {}: {}'.format(version_path, desc))

def get_version():
    try:
        f = open(version_path)
    except EnvironmentError:
        return None
    for line in f.readlines():
        mo = re.match("__version__ = '([^']+)'", line)
        if mo:
            ver = mo.group(1)
            return ver
    return None

class Version(Command):
    description = "update {} from Git repo".format(version_path)
    user_options = []
    boolean_options = []
    def initialize_options(self):
        pass
    def finalize_options(self):
        pass
    def run(self):
        update_version()
        print('Version is now {}'.format(get_version()))

if ((os.name == 'nt') and (sys.version_info[:2] >= (3, 5))
        and (numpy.version.version <= '1.13')):
    # Monkey patch numpy for MinGW until version 1.13 is mainstream
    # This fixes building extensions with Python 3.5+ resulting in
    #       the error message `ValueError: Unknown MS Compiler version 1900
    # numpy_fix uses the patch referenced here:
    #       https://github.com/numpy/numpy/pull/8355
    root = os.path.split(__file__)[0]
    sys.path.insert(0, os.path.join(root, 'numpy_fix'))
    import misc_util, mingw32ccompiler
    sys.modules['numpy.distutils.mingw32ccompiler'] = mingw32ccompiler
    sys.modules['numpy.distutils.misc_util'] = misc_util

_is_64bit = (getattr(sys, 'maxsize', None) or getattr(sys, 'maxint')) > 2 ** 32
_is_windows = sys.platform == 'win32'

if _is_windows and _is_64bit:
    args = ['-static-libgcc', '-static-libstdc++', '-Wl,--allow-multiple-definition']
    defs = [('MS_WIN64', None), ]
else:
    args = []
    defs = []

# Collect all Cython source files as a list of extensions
extensions = cythonize([
        Extension("pyfvs.*"
            , sources=["pyfvs/*.pyx"]
            , include_dirs=[numpy.get_include()]
            , extra_compile_args=args
            , extra_link_args=args
            , define_macros=defs
            )])

setup(
    name='pyfvs'
    , version=get_version()
    , description=description
    , long_description=long_desc
    , url='https://github.com/tharen/PyFVS'
    , author="Tod Haren"
    , author_email="tod.haren@gmail.com"
    , setup_requires=['cython', 'numpy>=1.11', 'pytest-runner','twine']
    , tests_require=['pytest']
    , install_requires=['numpy>=1.11', 'pandas']
    , ext_modules=extensions
    , packages=['pyfvs', 'pyfvs.keywords']
    , package_data={
            '':['*.pyd', '*.cfg', '*.so', 'README.*']
            , 'pyfvs':['docs/*', 'examples/*', 'test/*.py', 'test/rmrs/*']
            }
    # , include_package_data=True # package the files listed in MANIFEST.in
    , entry_points={
            'console_scripts': ['pyfvs=pyfvs.__main__:main']
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
    , cmdclass={"version": Version, }
    )
