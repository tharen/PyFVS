PyFVS Getting Started
=====================

The **[PyFVS](https://github.com/tharen/PyFVS)** project provides compiled Python extensions and classes 
for the Forest Vegetation Simulator (FVS), a forest growth model. PyFVS 
is a fork of the [Open-FVS]() project. PyFVS aims to remain current with 
the 'trunk' of the Open-FVS SVN code base, however there may be a lag. 
PyFVS includes specific modifications to improve Python integration and 
to add desired functionality. PyFVS is an open-source project without 
dedicated support. As such a fair degree of user knowledge and the 
requisit caution may necessary to use it successfully. 

This guide will walk you through the current preferred method to build 
PyFVS from scratch on Windows (64 bit). From there you will be able to 
hack, test, and issue pull requests. The guide assumes the reader is 
reasonably familiar with Python, tools used in building open-source 
software, and the FVS model. PyFVS is also known to work on both Linux 
and OS X. The reader is referred to the AppVeyor and Travis-CI 
continuous integration routines for more working examples of these other 
platforms (OS X CI is pending). The procedures in this guide can surely be 
modified and/or improved. Any suggested improvements would be welcomed.

# Outline

  1. [Tools](#Tools)
  1. [Workspace](#Workspace)
  1. [Get the Code](#Get-the-Code)
  1. [Setup Python](#Setup-Python)
  1. [Setup the Environment](#Setup-the-Environment)
  1. [Configure CMake](#)
  1. [Build PyFVS](#)
  1. [Install for Development](#Install-for-Development)
  1. [Testing](#)
  1. [Packaging](#)

# Tools

The following is a suggested set of tools that may be used to build and 
develop PyFVS on Windows. It is by no means definitive as only included
to assist users who may be new to Python, etc. Links are provided to tools 
that provide the necessary functionality, and not meant as an 
endorsement of any one product.

  - **Operating System** - This guide assumes a Windows 64 bit system is being used.
      
      `Windows 7 Professional, Intel Core i7 @ 2.8GHz`
      
    PyFVS will also compile and run on Linux and OS X, but these are not 
    specifically addressed here.
    
  - **Text editor** - A good text editor with syntax highlighting and other 
    programmer friendly tools is a must.
  
    - [Notepad++](https://notepad-plus-plus.org/)
  
  - **Console Emulator** - PyFVS is built using command line tools. With heavy use
    the Windows command prompt becomes cumbersome.  A console emulator provides 
    additional functionality that one may find useful.

     - [ConEmu](https://conemu.github.io/)
   
  - **Git client** - If you intend to pull updates, contribute code, etc. you will need
    [Git]() installed. Keep in mind that Git for Windows includes 
    a basic posix environment (MSYS) that includes the `sh.exe` shell command. This 
    conflicts with the MinGW environment (below). The simple solution is to avoid
    adding Git to the PATH variable and rely on the GUI. From the GUI an isolated 
    shell environment can be launched if needed.

    - [Git Extensions](https://gitextensions.github.io/) provides a no fuss 
      GUI environment

  - **Python** - A Python virtual environment is the best way to isolate your build
    environment and ensure repeatable results. PyFVS is built and tested using
    the Conda system, specifically [Miniconda](https://conda.io/miniconda.html).
    If you are not familiar with virtual environments please read through the 
    information on the Miniconda website. Other Python environments may work as
    well as long as it provides development headers and libraries for Python
    and Numpy. The resulting packages should be compatible with other Python
    distributions of the same version and architecture, e.g Python.org, ArcGIS, 
    but YMMV.
    
    - [Windows 64 bit installer](https://repo.continuum.io/miniconda/Miniconda3-latest-Windows-x86_64.exe)
    
  - **Integrated Development Environment** - Rather than switching between number of
    language specific editors, a good universal IDE will improve the code,
    compile, debug, test cycle. There are a number of good IDE's available for 
    Python, but few that incorporate support for multiple programming languages
    well. The Eclipse IDE with the PyDev and Photran extensions are well liked.
    While not discussed here it can be setup to manage compiling, debugging, 
    etc. using the MinGW toolchain.
    
    - [Eclipse IDE](https://eclipse.org/ide/) - Start with the download for C/C++
    - [PyDev Extension](http://www.pydev.org) - Install instructions are
      located [here](http://www.pydev.org/manual_101_install.html).
      See "Installing with the update site".
    - [Photran](http://www.eclipse.org/photran/) - This is available through the
      main Eclipse software channel.
    
    Additionally you may like to install one of the CMake and Git extensions 
    for Eclipse.
    
  - **C and Fortran Compiler** - PyFVS is built using the GCC compilers and 
    associated tools. The MinGW project provides open-source GCC compilers and
    tools targetting Windows. Compilers from other providers may work as well.
        
    - [MinGW-w64 6.2.0](https://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/6.2.0/threads-win32/seh/), 
      is currently used with PyFVS. More recent versions may also work. The
      mingw-builds for win32 threading and seh exception handling seem to work 
      well. The MinGW-w64 provides an installer, however there is no need for it.
      Simply download the archive and extract it to a convenient location on
      your computer. The archives are `.7z` format so you will need an archive 
      utility like [7-zip](http://www.7-zip.org/download.html) to extract it.
    
    >**Note:** PyFVS has **not** been tested with the Python specific variants of MinGW, 
    including the conda provided package.      
    
    >**Note:** MinGW is capable of targetting various architectures, aside from 
    that of the native machine. However, the Python version used in the build
    process must be compatible with the target architecture. So if you wish to 
    target say Python 2.7, 32 bit, you will need to adjust accordingly. This has
    not been thoroughly tested.
    
  - **CMake** - CMake is used to manage the PyFVS build process.
  
    - [CMake](https://cmake.org/)
  
# Workspace


Create a workspace to organize your project. This can be what ever you want. 
However, it is usually advisable to avoid spaces in paths.

    mkdir c:\pyfvs
    cd c:\pyfvs

# Get the Code

From a git command line clone the repository.

    cd c:\pyfvs
    git clone https://github.com/tharen/PyFVS.git`

Or use your Git GUI to clone the repository to a subfolder in your workspace.

Optionally, you could extract a static copy of the code from the 
[zip file](https://github.com/tharen/PyFVS/archive/master.zip) provided by 
GitHub.

In any case you should now have a pyfvs subolder, e.g. 
`c:\pyfvs\pyfvs`. The contents will be familiar to you if you have 
experience with the Open-FVS project. The key differences from Open-FVS 
are the *api*, and *python* subfolders. *api* is a mis-nomer as it also 
serves as a landing spot for modified core FVS files until a better 
folder structure is devised. 

# Setup Python

You are encouraged to use a virtual environment to manage the build environement.
This will help to ensure repeatable results and minimize package version conflicts.

    cd c:\pyfvs\pyfvs
    conda update conda
    conda env create -n pyfvs_env python=3.4 -f environment.yml
    activate pyfvs_env

This will refresh your conda environment and install the packages necessary to
build, test, and package PyFVS. PyFVS will also work with other versions of 
Python so long as the necessary packages are available. The environment.yml 
file in the repository accepts the latest version of packages, with some exceptions.
If a revision to a package causes the build to break a known good version 
can be specified in the environment.yml file. Calling the following command will
downgrade all packages to a consistent compatible state.

    conda env update -n pyfvs_env -f environment.yml
    
In future development sessions you can simply call `activate pyfvs_env` from
the command line.

>**Note**: You should edit the environment.yml file to specify your target
python version.

>**Tip**: Conda environments generally come with the necessary development libraries and
headers, etc. However, it may be necessary to (re)build the Python static 
library for your flavor of MinGW. A Python script `bin/get_libpython.py`
is provide to facilitate this step. Once the PATH variables are setup (below) 
simply execute `python gen_libpython.py` from the bin folder. This step may not
be necessary as MinGW becomes more stable.

# Setup the Environment

To ensure repeatable results, create a batch file called `set_env.bat` in the 
root of your workspace with the following contents, adapted to you local setup.
Any other programs you need in the local path or environment variables can be 
added.

    @echo off
    
    set PYFVS_OLDPATH=%PATH%
    set PATH=C:\mingw-w64\x86_64-6.2.0-release-win32-seh-rt_v5-rev1\mingw64\bin;%PATH%
    set PATH=C:\progs\cmake\bin;%PATH%

Execute this batch file when you start a development session.

>**Note:** Git for windows includes it's own sh.exe. This conflicts with the 
MinGW tools and must not be present in the PATH variable. You will receive an 
error message when CMake configures saying as much.

>**Tip:** Conda environments can execute script on 
[activate/deactivate](https://conda.io/docs/using/envs.html#saved-environment-variables). 
This is useful for setting environment variables, etc. Simply copy the set_env.bat file into 
`C:\Miniconda3\envs\pyfvs_env\etc\conda\activate.d`. Create the subfolders as
necessary. Conda will execute any files in this folder during activation. A 
complementary file to undo the changes, e.g. unset_env.bat, can be placed in 
a folder called deactivate.d.

# Configure CMake

The build is designed to take place in a subfolder of the *bin* folder within the
*pyfvs* repository root folder. To configure the CMake project issue the
following. A version of these steps is available in the `bin/config.bat` file.

    cd c:\pyfvs\pyfvs\bin
    mkdir build
    cd build
    cmake -G "MinGW Makefiles" .. ^
        -DFVS_VARIANTS="pnc;wcc" ^
        -DCMAKE_SYSTEM_NAME=Windows ^
        -DWITH_PYEXT=Yes ^
        -DNATIVE_ARCH=ON ^
        -DCMAKE_BUILD_TYPE=Debug
        -DCMAKE_INSTALL_PREFIX=Open-FVS

CMake will generate the necessary folder hierarchy, makefiles, etc. Note that 
the target Python environment, e.g. pyfvs_env, must be active for CMake to find
the appropriate Python executable, packages, etc.

  - As the name suggests `-DFVS_VARIANTS` specifies which FVS variant libraries
  you want to compile. **Not all variants are currently available through 
  PyFVS**, but can be added as needed.
  
  - If `-DNATIVE_ARCH` is ON then CMake will attempt to set optimizations 
  for the local machine architecture. Turn this off if you will be sharing
  compiled libraries in a heterogeneous environment.
  
  - `-DCMAKE_BUILD_TYPE` sets certain compiler optimization flags. Current 
  options include Debug and Release.
  
  - `-DCMAKE_INSTALL_PREFIX` sets the location the compiled libraries, packages,
  etc. will be copied to when the install target is run. In this case it will
  simply be a local subfolder.

Any additional CMake variables can be added. CMake has options for controlling
the compiler, build options, verbosity, etc.

>**Note:** This folder structure is a holdover from the Open-FVS
project and may be restructured at some point in the future to be consistent 
with current best practices.

# Build PyFVS

To build all specified variants and install to the target location in one shot
using eight parallel make processes.

    cmake --build . --target install -- -j8

To build only one variant on a single core. This is useful for debugging a build.

    cmake --build . --target pnc

# Install for Development and Testing

Once the build is complete you can test it inplace. However, if you will be 
working on the Python code you may want to [install for development]().

    cd c:\pyfvs\pyfvs\bin\build\Open-FVS\python
    python setup.py build_ext --inplace --compiler=mingw32
    pip install .

This will compile the additional cython extension install the pyfvs package
to your site-packges folder. Call the CLI to verify everything worked correctly.

    pyfvs --help
    pyfvs --help-variants

# Testing

The tests can be run automatically using [pytest](https://docs.pytest.org/en/latest/).

    cd c:\pyfvs\pyfvs\bin\build\Open-FVS\python
    pytest

The tests can be run from the PyFVS command line interface as well.

    pyfvs --run-tests

>**Note:** The tests attempt all supported variants and will fail, silently, in
some cases, if not all the supported variants are available.

# Packaging

Finally, create a binary package to shared with other users or archive.

    cd c:\pyfvs\pyfvs\bin\build\Open-FVS\python
    python setup.py bdist_wheel

Other setuptools based packages are also possible, MSI, zip, etc.

# Install for Development and Testing

A special `dev` CMake target has been set to assist with development and 
debugging. The dev target will copy the compiled python extension libraries to
the python/pyfvs folder in the source tree. From there you can work directly
on the Python source.

Ensure remenants of previous pyfvs installations are not present.

    pip uninstall pyfvs
    
You also need to check for erroneous paths left over from failed installs.
Open C:\Miniconda3\envs\pyfvs_env\Lib\site-packages\easy-install.pth in a text 
editor and make sure there are no paths refering to previous install attempts.

When building the PyFVS libraries (above) avoid calling the `install` target to avoid confusion.
The install target performs additional steps related to documentation, etc. and 
will only serve to clutter your workspace if all you are doing is hacking on the
Python code.

From the build subfolder execute CMake with the dev target. This target ensures
the PyFVS libraries are compiled and then copies them to the python\pyfvs source
folder. Be sure not to commit them to git!

    cd c:\pyfvs\pyfvs\bin\build
    cmake --build . --target dev

No go to the Python source folder and install in development mode.

    cd c:\pyfvs\pyfvs\python
    python setup.py build_ext --inplace --force
    pip install -e .

This will not install PyFVS to your site-packages, but instead add the source
folder to the PYTHONPATH environment in the easy-install.pth file. This will 
also compile the CLI executable and add it to your scripts folder. Now you can
hack away on the Python code and your edits will be "live" with no need to 
copy files back and forth, etc. If you edit the Cython extension files *.pyx 
you will have to repeat the call to setup.py as above. Likewise if you edit the
fortran code you'll need to rebuild the PyFVS library using CMake.

    cmake --build c:\pyfvs\pyfvs\bin\build --target dev
    