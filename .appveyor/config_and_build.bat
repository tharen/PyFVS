set OLDPATH=%PATH%
set PATH=%PYTHON%;C:\msys64\mingw64\bin;C:\Program Files (x86)\cmake\bin
set PATH=%PATH%;C:\Windows\System32;C:\Windows
echo %PATH%
echo %PYTHONPATH%
call python -c "import sys;print(sys.executable)"
call python -c "import sys;print(sys.version)"

:: Python and CMake include with MSYS MinGW conflict with the target executables
del C:\msys64\Mingw64\bin\python.exe
del C:\msys64\Mingw64\bin\cmake.exe

:: Activate the target Python environment
::call %PYTHON%\Scripts\activate %ENV_NAME%
::set PATH=%PYTHON_HOME%;%PYTHON_HOME%\Scripts;%PATH%

:: Configure CMake
mkdir bin\build
pushd bin\build
cmake -G "MinGW Makefiles" .. ^
    -DFVS_VARIANTS="pnc;wcc" ^
    -DCMAKE_SYSTEM_NAME=Windows ^
    -DWITH_PYEXT=Yes ^
    -DCMAKE_INSTALL_PREFIX=Open-FVS || goto :error_configure

:: Compile and install locally
mingw32-make install 2> build_err.log || goto :error_build

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
exit /b %err_code%

:exit
popd
set PATH=%OLDPATH%
exit /b 0
