
:: Make sure the named Python environment is active
call activate pyfvs

:: Report the current numpy version
call python -c "import numpy;print('Numpy version:',numpy.version.version)"

echo %PYTHONPATH%
call python -c "import sys;print(sys.executable)"
call python -c "import sys;print(sys.version)"

:: Move into the package parent folder
cd %APPVEYOR_BUILD_FOLDER%
:: Install the compiled wheel
call pip install --no-index --find-links . pyfvs

:: Execute all test scripts
:: CLI
call pyfvs --run-tests
:: Tests module
call pytest -rsx --pyargs pyfvs.test

:: Test examples
call pytest %APPVEYOR_BUILD_FOLDER%\python\pyfvs\examples\demo.py
call pytest %APPVEYOR_BUILD_FOLDER%\python\pyfvs\examples\pn_test.py
