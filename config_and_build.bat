set oldpath=%path%
set path=C:\progs\mingw-w64\x86_64-5.3.0-posix-seh-rt_v4-rev0\mingw64\bin
REM set path=C:\progs\mingw-w64\x86_64-5.3.0-win32-sjlj-rt_v4-rev0\mingw64\bin
::set path=C:\progs\msys64\mingw64\bin
set path=%path%;C:\progs\cmake-3.4.1-win32-x86\bin
set path=C:\progs\Git\bin;C:\progs\Git\usr\bin;%path%
set path=%path%;C:\Windows;C:\Windows\System32
REM set path=%path%;C:\Windows\System32\WindowsPowerShell\v1.0

REM set python=C:\Python3.4.4.amd64
REM set python_ver=3.4
REM set env_name=pyfvs_py34
REM set python_arch=amd64

set python=C:\Python2.7.11.amd64
set python_ver=27
set env_name=pyfvs_py27
set python_arch=amd64

:: Configure the build environment
set proj_root=%~dp0
set build_root=%~dp0\bin\build
mkdir %build_root%
pushd %build_root%

REM set numpy_url=http://downloads.sourceforge.net/project/numpy/NumPy/1.10.2/numpy-1.10.2-win32-superpack-python2.7.exe
REM powershell -Command "(New-Object Net.WebClient).DownloadFile('%numpy_url%', 'numpy.exe')"

:: Create and activate the target Python environment
call %python%\Scripts\pip install virtualenv
if not exist "%env_name%" (
    call %python%\Scripts\virtualenv.exe %env_name%
    )
call %env_name%\Scripts\activate

REM goto :build_libpython

REM :: Install needed packages
python -m pip install --upgrade pip
REM pip install -r %proj_root%\requirements.txt
pip install nose-parameterized nose2

:: Determine if numpy is installed
python -c "import numpy as n;print('.'.join(n.version.version.split('.')[:2]))" > foo || goto :install_numpy
set /p numpy_ver= < foo
del foo
echo Numpy version %numpy_ver% currently installed
if %numpy_ver% geq 1.9 goto :skip_numpy

:install_numpy
:: Use numpy from: http://www.lfd.uci.edu/~gohlke/pythonlibs/#numpy
start /wait c:\progs\git\usr\bin\curl.exe -fsS -o numpy-1.10.4+vanilla-cp27-none-win_amd64.whl https://free5.s3-us-west-2.amazonaws.com/appveyor-builds/numpy/numpy-1.10.4%%2Bvanilla-cp27-none-win_amd64.whl
%env_name%\Scripts\pip install --no-index --find-links=. "numpy>=1.9"
if %errorlevel% neq 0 (
    echo Failed to install Numpy
    goto :exit
    )
:skip_numpy

:build_libpython
:: Build the MinGW import library
if not exist "%env_name%\libs\libpython%python_ver%.a" (
    echo Generate a MinGW import library for python%python_ver%.dll
    mkdir %build_root%\%env_name%\libs
    pushd %build_root%\%env_name%\libs
    set python_dll=%SystemRoot%\System32\python%python_ver%.dll
    echo %python_dll%
    if %python_arch%==win32 (
        set python_dll=%SystemRoot%\SysWOW64\python%python_ver%.dll
        )
    call gendef %python_dll%
    REM gendef %SystemRoot%\SysWoW64\python%python_ver%.dll
    call dlltool --dllname python%python_ver%.dll --def python%python_ver%.def --output-lib libpython%python_ver%.a
    popd
    )
REM goto :exit

set PATH=%PATH:C:\progs\git\bin;=%

:: Configure CMake
cmake -G "MinGW Makefiles" .. ^
    -DFVS_VARIANTS="pnc;wcc" ^
    -DCMAKE_SYSTEM_NAME=Windows ^
    -DWITH_PYEXT=Yes ^
    -DCMAKE_INSTALL_PREFIX=Open-FVS || goto :error_configure

:: Compile and install locally
mingw32-make -j8 install 2> build_err.log || goto :error_build

set PATH=%OLDPATH%

goto :exit

:error_build
echo Build failed.
goto :exit

:error_configure
echo CMake configure failed.
goto :exit

:exit
popd
set path=%oldpath%
exit /b %errorlevel%
