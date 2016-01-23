
:: Make sure the named Python environment is active
call %PYTHON%\Scripts\activate %ENV_NAME%
pip install --upgrade nose-parameterized nose2 wheel

:: Move into the package parent folder
pushd %APPVEYOR_BUILD_FOLDER%\bin\build\Open-FVS\python

call python setup.py bdist_wheel
cd dist
call pip install --no-index --find-links . pyfvs
call fvs --run-tests

move /y dist\*.whl %APPVEYOR_BUILD_FOLDER%

popd
