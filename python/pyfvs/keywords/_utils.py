#_utils.py
"""
Utility functions and routines shared amongst modules
"""

import os
import time
import sys

#Python classes can have dynamic properties in lue of attributes
#use this makeProperty magic functions to simplify the creation of
#properties.  It's a little awkward to read at first, but is pretty cool
def makeProperty(fcn):
    """
    Helper function to create a class property.  Its used as a decorator
        to magically create class properties.

    fcn would be a static method that defines a fget, fset, fdel functions
        and the doc string for the property.  This functionally hides the
        fget, etc methods from the caller.
    """
    return property(doc=fcn.__doc__, **fcn())
