
:: Make sure the named Python environment is active
REM call %PYTHON%\Scripts\activate %ENV_NAME%

:: Report the current numpy version
call python -c "import numpy;print('Numpy version:',numpy.version.version)"

:: Move into the package parent folder
pushd %APPVEYOR_BUILD_FOLDER%\bin\build\Open-FVS\python

echo %PYTHONPATH%
call python -c "import sys;print(sys.executable)"
call python -c "import sys;print(sys.version)"

:: Execute all test scripts
call python -m nose2
popd
