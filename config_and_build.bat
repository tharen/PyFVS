set oldpath=%path%

set PATH=C:\progs\mingw-w64\x86_64-6.2.0-release-win32-seh-rt_v5-rev1\mingw64\bin
set PATH=%PATH%;C:\Windows\System32;C:\Windows
set PATH=C:\progs\cmake\bin;%PATH%
REM set PATH=C:\Ruby22-x64\bin;%PATH%
REM set PATH=C:\progs\Git\bin;C:\progs\Git\usr\bin;%PATH%
set path=%path%;C:\Miniconda3\Scripts;C:\Miniconda3

:: Activate the Python environment
call activate pyfvs_py34_amd64

:: Configure the build environment
set proj_root=%~dp0
set build_root=%~dp0\bin\build
mkdir %build_root%
pushd %build_root%

REM goto :build_libpython

REM :: Install needed packages
python -m pip install --upgrade pip
REM pip install -r %proj_root%\requirements.txt
pip install nose-parameterized nose2

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

:: Configure CMake
cmake -G "MinGW Makefiles" .. ^
    -DFVS_VARIANTS="pnc;wcc;soc" ^
    -DCMAKE_SYSTEM_NAME=Windows ^
    -DWITH_PYEXT=Yes ^
    -DCMAKE_INSTALL_PREFIX=Open-FVS || goto :error_configure

:: Compile and install locally
cmake --build . --target install -- -j8 2> build_err.log || goto :error_build

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
