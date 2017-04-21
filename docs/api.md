Application Programming Interface
---------------------------------

PyFVS adds a number of enhancements to the existing FVS API and control routines.

PyFVS supports two levels of access to the FVS API, a low level API, closely 
aligned with the core Open-FVS API, as well as a Python class with additional 
functionality.

# Open-FVS API

The FVS interface has evolved over time as computer technology improved and the
user base expanded and became more and more sophisticated in the application
of forest growth models. Other growth model projects began to release and 
support shared libraries (e.g. DLL's) to enable tighter integration with related
software systems. Open-FVS was established, at least in part, to support the 
development of a more open growth modeling infrastructure. Users and maintainers
began adapting the core FVS code base to a more flexible shared library and 
application programing interface design, while still maintaining the legacy
command line and GUI tools that many users require.

The Open-FVS API includes a number of subroutines for executing and FVS, interogating
the system at runtime, and for bi-directional data I/O. This API has been exposed
as a shared library for use in R, Python, VBA, .NET, and many other programming
languages. PyFVS exposes all these functions and more. More information on this
portion of the API is available on the Open-FVS wiki.

  - [FVS API](https://sourceforge.net/p/open-fvs/wiki/FVS_API/)
  - [rFVS](https://sourceforge.net/p/open-fvs/wiki/rFVS/)

# FVS Step API
# Data API

# Python FVS Class

The canonical Python use case would be the FVS class, `pyfvs.fvs.FVS`. This class
wraps the step API and provides access to all lower level data and subroutines.

An FVS object is initialized with the desired variant abbreviation. The methods,
properties, and attributes of the returned FVS instance are then used to 
initialize, execute, and retreive data for on or more growth projections.

# FVS Keywords Module

PyFVS includes a module for creating FVS keyword files as a collection of 
keyword objects.

# Additional Features

  - use_fast_age
  - use_fvs_morts
  