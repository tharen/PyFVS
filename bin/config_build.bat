set oldpath=%PATH%

REM set PATH=C:\progs\mingw-w64\x86_64-5.3.0-win32-sjlj-rt_v4-rev0\mingw64\bin
set PATH=C:\progs\mingw-w64\x86_64-6.2.0-release-win32-seh-rt_v5-rev1\mingw64\bin
set PATH=%PATH%;C:\Windows\System32;C:\Windows
set PATH=C:\progs\cmake\bin;%PATH%
REM set PATH=C:\progs\Git\bin;C:\progs\Git\usr\bin;%PATH%
set path=%path%;C:\Miniconda3;C:\Miniconda3\Scripts

if not exist ".\build" mkdir ".\build"
cd .\build

call activate pyfvs_py34_amd64

python ..\gen_libpython.py

set PATH=%PATH:C:\progs\git\bin;=%
set PATH=%PATH:C:\progs\git\usr\bin;=%

cmake -G "MinGW Makefiles" .. ^
    -DFVS_VARIANTS="pnc;wcc;soc;cac" ^
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

:exit
popd
set path=%oldpath%
exit /b %errorlevel%
