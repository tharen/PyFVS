# Open-FVS

This branch, [svn-trunk-ci](https://github.com/tharen/PyFVS/tree/svn-trunk-ci), 
tracks trunk in the official SVN
[Open-FVS](https://sourceforge.net/p/open-fvs/code/HEAD/tree/trunk/) 
repository. Minor modification to the build scripts have been included to 
improve automated builds and platform independence.

CI Build Status:
 - Travis-CI: [![Build Status](https://travis-ci.org/tharen/PyFVS.svg?branch=svn-trunk-ci)](https://travis-ci.org/tharen/PyFVS/branches)
 - AppVeyor: [![Build Status](https://ci.appveyor.com/api/projects/status/github/tharen/PyFVS?branch=svn-trunk-ci&svg=true)](https://ci.appveyor.com/project/tharen/pyfvs?branch=svn-trunk-ci)

# Building

The CMake based build scripts are slightly modified from Open-FVS. The changes make it easier to 
build a subset of variants, handles OS specific build logic, and adds a "bundle" target to co-locate 
compiled binaries, but not install them. See [CMakeLists.txt](https://github.com/tharen/PyFVS/blob/svn-trunk-ci/bin/CMakeLists.txt) for details.

**NOTE:** Using the CMake based build will overwrite bin/makefile. Don't commit these changes.

**NOTE:** Using MSYS2 the build is essentially the same. Simply change the system name on the command line.
"Unix Makefiles" works equally well with the MSYS2 bash shell. However, the other "* Makefile"
should work as well.
  
    cd bin
    # Configure CMake
    cmake -G"Unix Makefiles" . -DFVS_VARIANTS=pnc,wcc,op -DCMAKE_SYSTEM_NAME=[Linux|Windows]
    # Build and bundle all requested variants
    make -f makefile_all bundle
