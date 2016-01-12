"""
Class module for controlling and interogating FVS using the Python variant 
modules compiled with Open-FVS.

Created on Nov 29, 2014

@author: tod.haren@gmail.com
"""

import os
import sys
import logging
import logging.config
import random

import pyfvs

# FIXME: This is a hack for PyDev scripting
# os.chdir(os.path.split(__file__)[0])

pyfvs.init_logging()
log = logging.getLogger('pyfvs.fvs')

class FVS(object):
    """
    Provides an abstraction layer and convenience methods for running FVS.
    
    Access to the FVS API routines as well as additional FVS core routines,
        and common arrays and variables are accessible using standard Python
        attribute access conventions, eg. cycles = fvs.contrl.ncyc to find out
        how many projection cycles were executed. API and other subroutines 
        have expected input variables and return numpy arrays or Python objects.
    
    See the auto-generated HTML help file provided with each variant library 
        for a complete list of available subroutines and common variables.
    
    * NOTE: The FVS subroutines, etc. are all converted to lower case by F2PY.
    
    Basic usage:
    fvs = FVS(<variant abbreviation>)
    fvs.run_fvs(<path to keyword file>)
    
    # Return a tuple of species codes given an FVS ID using the FVS API.
    spp_attrs = fvs.fvsspeciescode(16)
    
    TODO: Add methods for execution with the start/stop routines.
    TODO: Add methods to collect tree attribute arrays.
    """
    def __init__(self, variant, random=False, config=pyfvs.get_config()):
        """
        Initialize a FVS variant library.
        
        @param variant:
        @param random: If True the FVS random number generater will be
                        reseeded on each call to run_fvs. If False the 
                        generator will be set to a fixed value of 1.0
        @param config:
        """
        self.variant = variant
        self.random = random
        self.config = config

        if not self.random:
            self._random_seed = 12345.0

        self.fvslib_path = None
        self._load_fvslib()

    def _load_lib_ini(self):
        fvslib_path = self.config['fvs_lib']['fvslib_path']

    def _init_fvslib_path(self):
        """
        Initialize the folder to load variant libraries from.
        """
        fvslib_path = self.config['fvs_lib']['fvslib_path']

        if os.path.exists(fvslib_path):
            self.fvslib_path = os.path.abspath(fvslib_path)
            return

        # Adjust fvs_bin path relative to the configuration file
        fvslib_path = os.path.join(
                os.path.dirname(pyfvs.config_path), fvslib_path)
        if os.path.exists(fvslib_path):
            self.fvslib_path = os.path.abspath(fvslib_path)
            return

        # Try the environment variable
        fvslib_path = os.environ('FVSLIB_PATH')
        if os.path.exists(fvslib_path):
            self.fvslib_path = os.path.abspath(fvslib_path)
            return

        if not self.fvslib_path:
            raise IOError('Could not find a suitable FVSLIB_PATH')

    def _discover_libs(self):
        """
        Search the fvslib_path for libraries available for import.
        """

        self._init_fvslib_path()

        self.variant_libs = {}
        files = os.listdir(self.fvslib_path)
        ext = pyfvs.platform_ext()
        for f in files:
            p = os.path.join(self.fvslib_path, f)
            if (os.path.isfile(p)
                    and f.startswith('pyfvs')
                    and f.endswith(ext)):
                n = f.split('.')[0]
                d = {'lib_name':n, 'lib_path':p}
                self.variant_libs[n[5:]] = d

        if not self.variant_libs:
            raise ValueError(
                    'No variant libraries found in : {}'.format(self.fvslib_path))

    def _load_fvslib(self):
        """
        Load the requested FVS variant library.
        """

#         self._discover_libs()
#         lib = self.variant_libs[variant]
#         print(lib)
#
#         if sys.version_info[:2] >= (3, 5):
#             import importlib.util
#             spec = importlib.util.spec_from_file_location(lib['lib_name'], lib['lib_path'])
#             self.fvslib = importlib.util.module_from_spec(spec)
#             spec.loader.exec_module(self.fvslib)
#
#         elif sys.version_inof[0] <= 2:
#             import imp
#             foo = imp.load_source(libname, self.fvslib_path)
#             foo.MyClass()

        variant = 'pyfvs%s' % self.variant.lower()
        self.fvslib = __import__(variant)
        log.debug('Loaded FVS variant {} library from {}'.format(
                self.variant, self.fvslib.__file__))

        # Initialize the FVS parameters and arrays
        # FIXME: This api function is subject to change
        self.fvslib.fvs_step.init_blkdata()

    def __getattr__(self, attr):
        """
        Return an object from self.fvslib
        """
        try:
            return self.fvslib.__dict__[attr]
        except KeyError:
            msg = 'No object {} in FVS{}.'.format(attr, self.variant)
            log.exception(msg)
            raise KeyError(msg)

    def set_random_seed(self, seed=None):
        """
        Reseed the FVS random number generator.  If seed is provided it will
        be used as the seed, otherwise a random number will be used.
        
        Args
        ----
        @param seed: None, or a number to seed the random number generator with. 
        """
        if seed is None:
            seed = random.random()

        self.ransed(True, seed)

    def _init_fvs(self, keywords):
        """
        Initialize FVS with the given keywords file.
        
        Args
        ----
        @param keywords: Path of the keyword file initialize FVS with.
        """
        if not os.path.exists(keywords):
            msg = 'The specified keyword file does not exist: {}'.format(keywords)
            log.error(msg)
            raise ValueError(msg)

        self.keywords = keywords
        self.fvslib.fvssetcmdline('--keywordfile={}'.format(keywords))

    def run_fvs(self, keywords):
        """
        Execute an FVS run for the given keyword file and return the error code.
        
        Args
        ----
        @param keywords: Path of the keyword file initialize FVS with.
        """
        self._init_fvs(keywords)

        if self.random:
            self.set_random_seed()
        else:
            self.set_random_seed(self._random_seed)

        r = self.fvslib.fvs()
        if r == 1:
            msg = 'FVS returned error code {}.'.format(r)
            log.error(msg)
            raise IOError(msg)

        if r != 0 and r <= 10:
            log.warning('FVS return with error code {}.'.format(r))

        if r > 10:
            log.error('FVS encountered an error, {}'.format(r))

        return r

    def get_summary(self, variable):
        """
        Return the FVS summary value for a single projection cycle.
        
        Args
        ----
        @param variable: The summary variable to return. One of the following:
                        year, age, tpa, total cuft, merch cuft, merch bdft, 
                        removed tpa, removed total cuft, removed merch cuft, 
                        removed merch bdft, baa after, ccf after, top ht after, 
                        period length, accretion, mortality, sample weight, 
                        forest type, size class, stocking class 
        """
        variables = {'year': 0
            , 'age': 1
            , 'tpa': 2
            , 'total cuft': 3
            , 'merch cuft': 4
            , 'merch bdft': 5
            , 'removed tpa': 6
            , 'removed total cuft': 7
            , 'removed merch cuft': 8
            , 'removed merch bdft': 9
            , 'baa after':10
            , 'ccf after':11
            , 'top ht after':12
            , 'period length':13
            , 'accretion':14
            , 'mortality':15
            , 'sample weight':16
            , 'forest type':17
            , 'size class':18
            , 'stocking class':19
            }

        try:
            i = variables[variable.lower()]
        except KeyError:
            msg = '{} is not an available summary variable({}).'.format(
                    variable, variables.keys())
            raise KeyError(msg)
        except:
            raise

        # Return the summary values for the cycles in the run
        ncyc = self.contrl_mod.ncyc
        return(self.fvslib.outcom_mod.iosum[i, :ncyc + 1])

def test():
    # Config file for testing
    pyfvs.config_path = os.path.join(os.path.split(__file__)[0], 'pyfvs.cfg')

    import pylab
    kwds = r'C:\workspace\Open-FVS\PyFVS\tests\pyfvs\fvspnc\pnt01.key'

    # Demonstrate the stochastic variability in the FVS routines.
    iters = 10
    fvs = FVS('pnc', random=True)

    # Get species codes
    spp_attrs = fvs.fvsspeciescode(16)
    print(spp_attrs)

    for i in range(iters):
        fvs.run_fvs(kwds)

        # Plot the BDFT volume
        bdft = fvs.get_summary('merch bdft')
        years = fvs.get_summary('year')
        pylab.plot(years, bdft)

    pylab.show()

if __name__ == '__main__':
    test()
