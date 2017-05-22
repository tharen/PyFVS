#demo.py
"""
Demonstrate basic usage of the PyFVS extension.

Author: Tod Haren, tod.haren <at> gm....com
"""

import os
import matplotlib.pyplot as pyplot

# import the fvs module
import pyfvs.fvs

def main():

    # Initialize FVS for the required variant
    fvs = pyfvs.fvs.FVS(variant='pn')
    kwds = os.path.join(os.path.split(__file__)[0], 'demo.key')
    
    # Execute a basic FVS run
    # execute_projection is a helper method of the FVS class.
    fvs.execute_projection(kwds)

    # Plot the projected merchantable volume
    age = fvs.get_summary('age')
    bdft = fvs.get_summary('merch bdft') * 1.0e-3
    pyplot.plot(age, bdft)
    pyplot.title('Merch. VPA')
    pyplot.xlabel('Age')
    pyplot.ylabel('VPA $(mbf)$')
    pyplot.show(block=True)

    # Plot several variables at once
    pyplot.subplot(2,2,1)
    tpa = fvs.get_summary('tpa')
    pyplot.plot(age, tpa)
    pyplot.title('TPA')
    pyplot.xlabel('Age')
    pyplot.ylabel('TPA')

    pyplot.subplot(2,2,2)
    baa = fvs.get_summary('baa after')
    pyplot.plot(age, baa)
    pyplot.title('BAA')
    pyplot.xlabel('Age')
    pyplot.ylabel(r'BAA $(ft^2 \times ac^{-1})$')

    pyplot.subplot(2,2,3)
    bdft = fvs.get_summary('top ht after')
    pyplot.plot(age, bdft)
    pyplot.title('Top Ht.')
    pyplot.xlabel('Age')
    pyplot.ylabel('Top Ht. $(ft)$')

    pyplot.subplot(2,2,4)
    bdft = fvs.get_summary('merch bdft')*1.0e-3
    pyplot.plot(age, bdft)
    pyplot.title('Merch. VPA')
    pyplot.xlabel('Age')
    pyplot.ylabel('VPA $(mbf)$')
    pyplot.tight_layout()

    pyplot.show(block=True)

def test_main():
    """Execute automated test."""
    def my_show(*args, **kargs):
        """dummy function so test code doesn't block."""
        return None
        
    pyplot.show = my_show
    main()

if __name__=='__main__':
    main()
