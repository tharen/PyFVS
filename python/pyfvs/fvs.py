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
import tempfile

import numpy as np
import pandas as pd
import pandas.io

import pyfvs
from pyfvs.keywords import keywords as kw

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
    def __init__(self, variant, stochastic=False, bootstrap=0, config=None):
        """
        Initialize a FVS variant library.

        :param variant: FVS variant abbreviation, PN, WC, etc. (Required)
        :param stochastic: If True the FVS random number generater will be
                reseeded on each call to run_fvs. If False the generator will
                be set to a fixed value of 1.0. (Optional)
        :param bootstrap: Number of bootstrap resamples of the plot data to
                execute. (Optional)
        :param config: Configuration dictionary. (Optional)
        """
        # TODO: Document configuration options

        self.variant = variant
        self.stochastic = stochastic

        if not config:
            self.config = pyfvs.get_config()
        else:
            self.config = config

        self._random_seed = 55329  # Default FVS random number seed

        self.fvslib_path = None
        self.fvslib = None
        self.projection_active = False

        self._load_fvslib()

        # if self.fvslib is None:
            # raise ValueError('Error initializing the FVS library.')

        self._keywords = None
        self._workspace = None
        self.trees = FVSTrees(self)
        self._spp_codes = None
        self._spp_seq = None

    def _load_fvslib(self):
        """
        Load the requested FVS variant library.
        """

        # FIXME: Variant libraries should be compiled without the climate fvs trailing 'c'
        if self.variant.lower() in ('oc', 'op'):
            variant_ext = 'pyfvs.pyfvs%s' % self.variant.lower()[:2]

        else:
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
        self.fvslib.tree_data.init_tree_data()
        # self.fvslib.initialize_api.init()

    # TODO: Add species code translation methods.
    @property
    def spp_codes(self):
        """
        Return the list of FVS species codes used in this variant.
        """
        if self._spp_codes is None:
            jsp = self.fvslib.plot_mod.jsp
            # F2PY returns arrays of characters with the wrong shape and order
            # Transform and reshape so the array is in the expected form
            jsp = jsp.T.reshape(-1, 4).view('S4').astype(str)[:, 0]
            self._spp_codes = np.char.strip(jsp)

        return self._spp_codes

    @property
    def spp_seq(self):
        """
        Return a dictionary of species code translations {fvs code: fia code,}
        """
        if self._spp_seq is None:
            maxspp = self.prgprm_mod.maxsp
            seq = {self.spp_codes[x]:x + 1 for x in range(maxspp)}
            self._spp_seq = seq

        return self._spp_seq


        # lookup for the FVS sequence code of each species
        # fvslib.spp_seq = {fvslib.spp_codes[x]:x + 1 for x in range(fvslib.prgprm_mod.maxsp)}


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

        :param seed: An odd number to seed the random number generator with.
        """

        if seed is None:
            self._random_seed = random.choice(range(1, 100000, 2))
        else:
            self._random_seed = seed

        # Seed the FVS random number generator
        self.ransed(True, self._random_seed)

        # Seed the numpy and python random number generators as well
        np.random.seed(self._random_seed)
        random.seed(self._random_seed)

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

    @property
    def workspace(self):
        """
        Return the current workspace folder.
        """
        if self._workspace is None:
            # FIXME: look in self.config first
            self._workspace = tempfile.mkdtemp(prefix='pyfvs')

        return self._workspace

    @workspace.setter
    def workspace(self, workspace):
        """
        Set the current workspace folder.
        
        Args
        ----
        workspace: Path to set as the current FVS workspace.
        """
        if not os.path.exists(workspace):
            raise ValueError('Workspace folder must already exist.')

        self._workspace = workspace

    # TODO: Further define how KeywordSet objects are used by and FVS instance
    #       They should be the preferred source, instead of a path to an
    #       existing file.
    @property
    def keywords(self):
        """
        Return the current keywords instance, creating one if necessary.
        """
        if self._keywords is None:
            self.init_keywords()

        return self._keywords

    @keywords.setter
    def keywords(self, keywords):
        """
        Set the current keywords instance.
        
        Args
        ----
        keywords: Instance of KeywordSet class.
        """

        if not isinstance(keywords, kw.KeywordSet):
            raise TypeError('kwd object must be an instance of KeywordSet')

        self._keywords = keywords

    def init_keywords(self, title='', comment=''):
        """
        Return an initialized KeywordSet instance.
        
        Args
        ----
        title: Keywords title
        comment: Keywords comment
        
        Returns
        -------
        keywords: An initialized KeywordSet object
        """
        self._keywords = kw.KeywordSet(title=title, comment=comment)
        return self._keywords

    def init_projection(self, keywords=None):
        """
        Initialize a projection with the provided keywords.
        
        Args
        ----
        keywords: Path to the FVS keywords file, or a KeywordSet instance.
                If None then use self.keywords.
        """

        if keywords is None:
            keywords = self.keywords

        if isinstance(keywords, kw.KeywordSet):
            keywords.write(self.workspace)

        elif not os.path.exists(keywords):
            msg = 'The keyword file does not exist: {}'.format(keywords)
            log.error(msg)
            raise ValueError(msg)

        # fvs_init requires a path
        r = self.fvs_step.fvs_init(keywords)

        # TODO: Handle and format error codes
        if not r == 0:
            log.error('FVS returned non-zero: {}'.format(r))

        if self.stochastic:
            self.set_random_seed()

        self.projection_active = True

    def grow_projection(self, cycles=1):
        """
        Execute the FVS projection.

        :param cycles: Number of cycles to execute. Set to zero to run all
                remaining cycles.
        """

        if not cycles:
            cycles = self.num_cycles - self.current_cycle

        for n in range(cycles):
            r = self.fvs_step.fvs_grow()
            # TODO: Handle non-zero exit codes

        return r

    def __del__(self):
        """
        Cleanup when the instance is destroyed.
        """
        if self.projection_active:
            self.end_projection()

    def end_projection(self):
        """
        Terminate the current projection.
        """
        # Finalize the projection
        r = self.fvs_step.fvs_end()
        self.projection_active = False

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
        r = self.end_projection()
        return r

    def write_treelist(self, plots, treelist_path):
        """
        Write out an FVS treelist file

        Args
        ----
        plots: Iterator of Pandas dataframe or numpy ndarray of plot tree records
        path: Treelist file path.
        """

        tmplt = pyfvs.treelist_format['template']

        if plots:
            with open(treelist_path, 'w') as out:
                for plot in plots:
                    if isinstance(plot, np.ndarray):
                        names = plot.dtype.names
                        for rec in plot:
                            out.write(tmplt.format(
                                    **dict(zip(names, rec))) + '\n')
                    elif isinstance(plot, pd.DataFrame):
                        names = plot.columns.values
                        for r, rec in plot.iterrows():
                            out.write(tmplt.format(
                                    **dict(zip(names, rec))) + '\n')

                    elif isinstance(plot, str):
                        # Assume the plot data is a Pandas dataframe in JSON clothing
                        plot_df = pandas.io.json.read_json(plot)
                        names = plot_df.columns.values
                        for r, rec in plot_df.iterrows():
                            out.write(tmplt.format(
                                    **dict(zip(names, rec))) + '\n')
                    else:
                        # TODO: Handle zero tree plots
                        continue
        else:
            raise ValueError('One or more plots are required, got nothing.')

    @property
    def current_cycle(self):
        return int(self.contrl_mod.icyc)

    @property
    def num_cycles(self):
        return int(self.contrl_mod.ncyc)

    def get_summary(self, variable):
        """
        Return an FVS summary value through the current projection cycle.

        :param variable: The summary variable to return. One of the following:
                        year, age, tpa, total cuft, merch cuft, merch bdft,
                        removed tpa, removed total cuft, removed merch cuft,
                        removed merch bdft, baa after, ccf after, top ht after,
                        period length, accretion, mortality, sample weight,
                        forest type, size class, stocking class
        """

        # Map summary variable to the array index
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
    Provide runtime access to tree attribute arrays.
    """
    def __init__(self, parent):
        self.parent = parent

    @property
    def num_recs(self):
        return self.parent.contrl_mod.itrn

    @property
    def plot_id(self):
        return self.parent.arrays_mod.itre[:self.num_recs + 1]

    @property
    def tree_id(self):
        return self.parent.arrays_mod.idtree[:self.num_recs + 1]

    @property
    def tpa(self):
        return self.parent.arrays_mod.prob[:self.num_recs + 1]

    @property
    def dbh(self):
        """Diameter at breast height"""
        return self.parent.arrays_mod.dbh[:self.num_recs + 1]

    @property
    def height(self):
        return self.parent.arrays_mod.ht[:self.num_recs + 1]

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
