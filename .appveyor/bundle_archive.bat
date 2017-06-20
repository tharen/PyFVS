:: Create the wheel and source zip artifacts

call activate pyfvs
set ARCHIVE_NAME=pyfvs-%PKG_VERSION%-Python%PYTHON_VERSION%_%PYTHON_ARCH%-windows

cd Open-FVS\python

:: Create the wheel as source archive
call python setup.py build_ext --inplace --compiler=mingw32
call python setup.py bdist_wheel
cp dist/* %APPVEYOR_BUILD_FOLDER%

:: Don't bundle source distribution since it will include compiled pyd's
::   and is thus platform specific
REM call python setup.py sdist --formats=zip

:: Create a git archive instead of an sdist
pushd %APPVEYOR_BUILD_FOLDER%
call git archive --format=zip --output=%APPVEYOR_BUILD_FOLDER%\%ARCHIVE_NAME%.zip HEAD
popd

REM :: Create the zip archive of the build folder
REM cd %APPVEYOR_BUILD_FOLDER%\bin\build
REM 7z a -bd %APPVEYOR_BUILD_FOLDER%\%ARCHIVE_NAME%-build.zip Open-FVS\*

exit /b 0
