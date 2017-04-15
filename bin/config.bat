set oldpath=%path%

REM call activate pyfvs_py34_amd64
python gen_libpython.py

if not exist ".\build_org" mkdir ".\build_org"
cd .\build_org

set path=%path:C:\progs\git\bin;=%
set path=%path:C:\progs\git\usr\bin;=%

::-DFVS_VARIANTS="pnc;wcc;soc;cac"
cmake -G "MinGW Makefiles" .. ^
    -DFVS_VARIANTS="pnc;wcc;soc;cac;oc" ^
    -DCMAKE_SYSTEM_NAME=Windows ^
    -DWITH_PYEXT=Yes ^
    -DCMAKE_INSTALL_PREFIX=Open-FVS || goto :error_configure

:: Compile and install locally
echo cmake --build . --target install -- -j8
REM cmake --build . --target install -- -j8 2> build_err.log || goto :error_build

goto :exit

:error_build
echo Build failed.
goto :exit

:error_configure
echo CMake configuration error.
goto :exit

:exit
popd
set path=%oldpath%
exit /b %errorlevel%
