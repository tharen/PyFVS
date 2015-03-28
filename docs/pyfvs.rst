PyFVS
=====

PyFVS provides Python wrappers and helper code to make automation of FVS
a breeze.

| View the `code`_
| Visit the `wiki`_
| Read the `docs`_

.. _code: https://github.com/tharen/PyFVS
.. _wiki: https://github.com/tharen/PyFVS/wiki/PyFVS
.. _docs: http://pyfvs.readthedocs.org/index.html

Example Usage::

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
   # **As with commandline FVS passing a keyword file assumes a 
   #     matching treelist file in the same folder
   fvs.run_fvs('./demo.key')

   # Plot the projected merchantable volume
   age = fvs.get_summary('age')
   bdft = fvs.get_summary('merch bdft')
   pyplot.plot(age, bdft)
   pyplot.title('Merchantable Volume Per Acre')
   pyplot.xlabel('Age')
   pyplot.ylabel('VPA (bdft)')
   pyplot.show(block=True)