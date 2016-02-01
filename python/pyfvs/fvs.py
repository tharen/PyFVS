"""
pyfvs.fvs

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
import importlib

#sys.path.append(os.path.abspath(os.path.dirname(__file__) + '/' + '..'))

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
    """
    # TODO: Add methods for execution with the start/stop routines.
    # TODO: Add methods to collect tree attribute arrays.
    def __init__(self, variant, stochastic=False, config=None):
        """
        Initialize a FVS variant library.
        
        :param variant: FVS variant abbreviation, PN, WC, etc.
        :param stochastic: If True the FVS random number generater will be
                        reseeded on each call to run_fvs. If False the 
                        generator will be set to a fixed value of 1.0
        :param config: Optional configuration file for logging, etc.
        """
        # TODO: Document configuration options

        self.variant = variant
        self.stochastic = stochastic

        if not config:
            self.config = pyfvs.get_config()
        else:
            self.config = config

        if not self.stochastic:
            self._random_seed = 12345.0

        self.fvslib_path = None
        self._load_fvslib()
        self.trees = FVSTrees(self)

    def _load_fvslib(self):
        """
        Load the requested FVS variant library.
        """

        variant_ext = 'pyfvs.pyfvs%sc' % self.variant.lower()[:2]
        try:
            self.fvslib = importlib.import_module(variant_ext)

        except ImportError:
            log.error('No library found for variant {}.'.format(self.variant))
            raise

        except:
            raise

        log.debug('Loaded FVS variant {} library from {}'.format(
                self.variant, self.fvslib.__file__))

        # Initialize the FVS parameters and arrays
        # FIXME: This api function is subject to change
        self.fvslib.fvs_step.init_blkdata()

    def __getattr__(self, attr):
        """
        Return an attribute from self.fvslib if it is n ot defined locally.
        """

        try:
            return getattr(self.fvslib, attr)

        except AttributeError:
            msg = 'No FVS object {}.'.format(attr,)
            log.exception(msg)
            raise AttributeError(msg)

    def set_random_seed(self, seed=None):
        """
        Reseed the FVS random number generator.  If seed is provided it will
        be used as the seed, otherwise a random number will be used.
        
        :param seed: None, or a number to seed the random number generator with. 
        """

        if seed is None:
            seed = random.random()

        self.ransed(True, seed)

    def _init_fvs(self, keywords):
        """
        Initialize FVS with the given keywords file.
        
        :param keywords: Path of the keyword file initialize FVS with.
        """

        if not os.path.exists(keywords):
            msg = 'The specified keyword file does not exist: {}'.format(keywords)
            log.error(msg)
            raise ValueError(msg)

        self.keywords = keywords
        self.fvslib.fvssetcmdline('--keywordfile={}'.format(keywords))

    def init_projection(self, keywords):
        """
        Initialize a projection with the provided keyword file.
        
        :param keywords: Path to the FVS keywords file.
        """

        if not os.path.exists(keywords):
            msg = 'The keyword file does not exist: {}'.format(keywords)
            log.error(msg)
            raise ValueError(msg)

        r = self.fvs_step.fvs_init(keywords)

        # TODO: Handle and format error codes
        if not r == 0:
            log.error('FVS returned non-zero: {}'.format(r))

        if self.stochastic:
            self.set_random_seed()
        else:
            self.set_random_seed(self._random_seed)

    def grow_projection(self, cycles=1):
        """
        Execute the FVS projection.
        
        :param cycles: Number of cycles to execute. Set to zero to run all 
                remaining cycles. 
        """

        if not cycles:
            cycles = self.num_cycles - self.current_cycle

        for n in range(cycles + 1):
            r = self.fvs_step.fvs_grow()
            # TODO: Handle non-zero exit codes

    def end_projection(self):
        """
        Terminate the current projection.
        """
        # Finalize the projection
        r = self.fvs_step.fvs_end()

        if r == 1:
            msg = 'FVS returned error code {}.'.format(r)
            log.error(msg)
            raise IOError(msg)

        if r != 0 and r <= 10:
            log.warning('FVS returned with error code {}.'.format(r))

        if r > 10:
            log.error('FVS encountered an error, {}'.format(r))

        return r

    def execute_projection(self, keywords):
        """
        Convenience method to execute all cycles.
        
        :param keywords: Path of the keyword file initialize FVS with.
        """

        self.init_projection(keywords)
        self.grow_projection(0)
        self.end_projection()

    @property
    def current_cycle(self):
        return self.contrl_mod.icyc

    @property
    def num_cycles(self):
        return self.contrl_mod.ncyc

    def get_summary(self, variable):
        """
        Return the FVS summary value for a single projection cycle.
        
        :param variable: The summary variable to return. One of the following:
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
        return(self.fvslib.outcom_mod.iosum[i, :self.num_cycles + 1])

class FVSTrees(object):
    """
    Provides runtime access to tree attribute arrays.
    """
    def __init__(self, parent):
        self.parent = parent

    @property
    def num_recs(self):
        return self.parent.contrl_mod.itrn

    @property
    def tpa(self):
        return self.parent.arrays_mod.prob[:self.num_recs + 1]

    @property
    def dbh(self):
        """Diameter at breast height"""
        return self.parent.arrays_mod.dbh[:self.num_recs + 1]

    @property
    def age(self):
        """Tree age"""
        return self.parent.arrays_mod.age[:self.num_recs + 1]

    @property
    def cfv(self):
        """Total cubic foot volume per tree"""
        return self.parent.arrays_mod.cfv[:self.num_recs + 1]

    @property
    def bfv(self):
        """Total scribner board foot volume per tree"""
        return self.parent.arrays_mod.bfv[:self.num_recs + 1]
