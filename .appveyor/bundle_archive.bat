:: Create the wheel and source zip artifacts

call activate pyfvs
set ARCHIVE_NAME=pyfvs-%PKG_VERSION%-Python%PYTHON_VERSION%_%PYTHON_ARCH%-windows-ci.zip

:: Create the wheel
cd Open-FVS\python
call python setup.py build_ext --inplace
call python setup.py bdist_wheel
mv dist/*.whl %APPVEYOR_BUILD_FOLDER%

:: Create the zip archive
:: FIXME: adapt this to use `sdist` after the install target is changed
cd %APPVEYOR_BUILD_FOLDER%\bin\build
7z a -bd %APPVEYOR_BUILD_FOLDER%\%ARCHIVE_NAME% Open-FVS\*

exit /b 0
