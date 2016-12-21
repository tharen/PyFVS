Introduction
============

PyFVS is being developed to provide Pythonic wrappers and utilities for the 
Forest Vegetation Simulator libraries. PyFVS is a fork of the Open-FVS project
and will periodically pull from upstream.  

Project Goals
-------------

PyFVS is a work in progress.  Some of the initial goals of the project include:  

  * Provide a basic, low level, Python extension for the FVS library provided
    by the Open-FVS projected.
  * Add additional functionality on top of the basic FVS library to improve 
    I/O and programatic control of simulations.
  * Abstract the keyword file parameterization system with set of Python
    modules and classes.
  * Provide high-level wrappers in the form of Python classes that abstract
    the access to FVS internals, arrays, subroutines, etc.
  * Integrate with other libraries, Pandas, Seaborn, etc. for data I/O
    , reporting, and analysis workflows.
  * Offer Open-FVS to the  Python community through standard Python semantics,
    e.g `pip install pyfvs`


What it's not
-------------

  * PyFVS is not intended to be a repository for development of forest modeling
    algorithms. That is best handled through the main Open-FVS repository.  
    However, PyFVS when combined with the dearth of statistical packages 
    available for Python could be incorporated into the workflow.


Prerequisites
-------------

For basic functionality PyFVS only requires a reasonably modern Numpy.

  - Numpy: http://www.numpy.org/


Usage
-----

See the :doc: `tutorial` for additional examples.

Basic example for executing a pre-defined keyword file:

.. code-block:: python
   
   >>>from pyfvs import fvs
   >>>f = fvs.FVS('pn')
   >>>r = f.execute_projection('/path/to/keywordfile.key')
  
  