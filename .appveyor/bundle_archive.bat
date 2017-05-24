:: Create the wheel and source zip artifacts

call activate pyfvs
set ARCHIVE_NAME=pyfvs-%PKG_VERSION%-Python%PYTHON_VERSION%_%PYTHON_ARCH%-windows-ci.zip

cd Open-FVS\python

:: Create the wheel as source archive
call python setup.py build_ext --inplace --compiler=mingw32
call python setup.py bdist_wheel
call python setup.py sdist --formats=zip

cp dist/* %APPVEYOR_BUILD_FOLDER%

:: Create the zip archive
:: FIXME: adapt this to use `sdist` after the install target is changed
cd %APPVEYOR_BUILD_FOLDER%\bin\build
7z a -bd %APPVEYOR_BUILD_FOLDER%\%ARCHIVE_NAME% Open-FVS\*

exit /b 0
