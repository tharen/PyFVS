set OLDPATH=%PATH%

REM if %PYTHON_ARCH% == "64" (
    REM set MINGW_PATH=C:\msys64\mingw64\bin
    REM set win32=No
REM ) else (
    REM set MINGW_PATH=C:\msys64\mingw32\bin
    REM set win32=Yes
REM )

set PATH=%PYTHON%;C:\msys64\mingw64\bin;C:\Program Files (x86)\cmake\bin
:: set PATH=%PYTHON%;C:\msys64\mingw32\bin;C:\Program Files (x86)\cmake\bin
REM set PATH=%PYTHON%;%MINGW_PATH%;C:\Program Files (x86)\cmake\bin
set PATH=%PATH%;C:\Windows\System32;C:\Windows
echo %PATH%
echo %PYTHONPATH%
call python -c "import sys;print(sys.executable)"
call python -c "import sys;print(sys.version)"

:: Python and CMake include with MSYS MinGW conflict with the target executables
del %MINGW_PATH%\python.exe
del %MINGW_PATH%\cmake.exe

:: Activate the target Python environment
call %PYTHON%\Scripts\activate %ENV_NAME%
::set PATH=%PYTHON_HOME%;%PYTHON_HOME%\Scripts;%PATH%

:: Build libpython just incase it's absent
call python bin\gen_libpython.py

call python -c "import sys;print(sys.version)"
call python -c "import sys;print(sys.executable)"

:: Configure CMake
mkdir bin\build
pushd bin\build
cmake -G "MinGW Makefiles" .. ^
    -DFVS_VARIANTS="pnc;wcc;soc;cac" ^
    -DCMAKE_SYSTEM_NAME=Windows ^
    -DNATIVE_ARCH=No ^
    -D32BIT_TARGET=%win32% ^
    -DWITH_PYEXT=Yes ^
    -DCMAKE_BUILD_TYPE=Release ^
    -DCMAKE_INSTALL_PREFIX=Open-FVS || goto :error_configure

:: Compile and install locally
cmake --build . --target install -- -j4 2> build_err.log || goto :error_build

:: Exit clean
goto :exit

:: Report errors from each phase
:error_configure
set err_code = 2
echo CMake configure failed.
goto :err_exit

:error_build
set err_code = 1
echo Build failed.
goto :err_exit

:err_exit
set PATH=%OLDPATH%
exit /b %err_code%

:exit
popd
set PATH=%OLDPATH%
exit /b 0
