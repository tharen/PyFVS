# fvs_demo.py
"""
Demonstrate basic usage of the PyFVS extension.

Author: Tod Haren, tod.haren <at> gm....com
"""

import matplotlib.pyplot as pyplot

# import the fvs module
import pyfvs.fvs

# Initialize FVS for the required variant
fvs = pyfvs.fvs.FVS(variant='pn')

# Execute a basic FVS run
# run_fvs is a helper method of the FVS class.
fvs.run_fvs('./demo.key')

# Plot the projected merchantable volume
age = fvs.get_summary('age')
bdft = fvs.get_summary('merch bdft')
pyplot.plot(age, bdft)
pyplot.title('Merchantable Volume Per Acre')
pyplot.xlabel('Age')
pyplot.ylabel('VPA (bdft)')
pyplot.show(block=True)

# Plot several variables at once
pyplot.subplot(2,2,1)
tpa = fvs.get_summary('tpa')
pyplot.plot(age, tpa)
pyplot.title('Trees Per Acre')
pyplot.xlabel('Age')
pyplot.ylabel('TPA')

pyplot.subplot(2,2,2)
baa = fvs.get_summary('baa after')
pyplot.plot(age, baa)
pyplot.title('Basal Area After Treatment')
pyplot.xlabel('Age')
pyplot.ylabel('BAA (sq. ft.)')

pyplot.subplot(2,2,3)
bdft = fvs.get_summary('top ht after')
pyplot.plot(age, bdft)
pyplot.title('Top Height After Treatment')
pyplot.xlabel('Age')
pyplot.ylabel('Top Ht. (ft.)')

pyplot.subplot(2,2,4)
bdft = fvs.get_summary('merch bdft')
pyplot.plot(age, bdft)
pyplot.title('Merchantable Volume Per Acre')
pyplot.xlabel('Age')
pyplot.ylabel('VPA (bdft)')

pyplot.show(block=True)
