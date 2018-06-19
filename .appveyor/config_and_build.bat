set OLDPATH=%PATH%

set win32=No

set PATH=%MINGW_PATH%\bin;%MINICONDA%;%MINICONDA%\Scripts
set PATH=%PATH%;C:\Program Files (x86)\cmake\bin
set PATH=%PATH%;C:\Windows\System32;C:\Windows
echo %PATH%
echo %PYTHONPATH%

:: Python and CMake include with MSYS MinGW conflict with the target executables
del %MINGW_PATH%\python.exe
del %MINGW_PATH%\cmake.exe

call activate pyfvs

pushd python
call python setup.py version
popd

call python -c "import sys;print(sys.version)"
call python -c "import sys;print(sys.executable)"

:: Build libpython just in case it's absent
call python bin\gen_libpython.py

copy %MINGW_PATH%\x86_64-w64-mingw32\lib\libmsvcr100.a %MINICONDA%\envs\pyfvs\libs
if %PYTHON_VERSION%=="2.7" (
    copy %MINGW_PATH%\x86_64-w64-mingw32\lib\libmsvcr90.a %MINICONDA%\envs\pyfvs\libs
    )

:: Configure CMake
mkdir bin\build
cd bin\build
cmake -G "MinGW Makefiles" .. ^
    -DFVS_VARIANTS=%FVS_VARIANTS% ^
    -DCMAKE_SYSTEM_NAME=Windows ^
    -DNATIVE_ARCH=No ^
    -D32BIT_TARGET=%win32% ^
    -DWITH_PYEXT=Yes ^
    -DCMAKE_BUILD_TYPE=Release ^
    -DCMAKE_INSTALL_PREFIX=Open-FVS || goto :error_configure

:: Compile and install locally
cmake --build . --target install 2> build_err.log || goto :error_build

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
