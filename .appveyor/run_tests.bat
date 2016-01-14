set PYTHONPATH=%APPVEYOR_BUILD_FOLDER%\bin\build\Open-FVS\python;%PYTHONPATH%

pushd %APPVEYOR_BUILD_FOLDER%\bin\build\Open-FVS\python\test
%PYTHON% -m unittest test_variants

popd
