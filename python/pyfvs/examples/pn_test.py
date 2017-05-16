"""
Demonstrate usage of the pyfvs package.

Created on Dec 5, 2014

@author: THAREN
"""

import os
import sys
from matplotlib import pyplot

import pyfvs
import pyfvs.fvs

pyfvs.fvs.log.setLevel('DEBUG')

def main(iters=10):
    # Config file for testing
    # PyFVS uses a config file written as a Python dictionary.
    # The config file is used to initialize logging and FVS library paths.
    #pyfvs.config_path = os.path.join(os.path.split(__file__)[0], 'pyfvs.cfg')

    # As with commandline FVS a keyword file is required
    kwds = os.path.join(os.path.split(__file__)[0], 'pn_test.key')

    # Demonstrate the stochastic variability in the FVS routines.
    fvs = pyfvs.fvs.FVS('pnc', stochastic=True)

    # Get species codes
    spp_attrs = fvs.fvsspeciescode(16)
    print(spp_attrs)

    for i in range(iters):
        fvs.execute_projection(kwds)

        # Plot the BDFT volume
        # get_summary is a helper method of the FVS class
        bdft = fvs.get_summary('merch bdft')
        years = fvs.get_summary('year')
        pyplot.plot(years, bdft)

    pyplot.show(block=True)

def test_main():
    """Execute automated test."""
    def my_show(*args, **kargs):
        """dummy function so test code doesn't block."""
        return None
        
    pyplot.show = my_show
    main(iters=2)
    
if __name__ == '__main__':
    main()
