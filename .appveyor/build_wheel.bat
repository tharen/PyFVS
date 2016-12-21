
set pythonpath=

:: Make sure the named Python environment is active
call %PYTHON%\Scripts\activate %ENV_NAME%
pip install --upgrade nose-parameterized nose2 wheel

:: Move into the package parent folder
pushd %APPVEYOR_BUILD_FOLDER%\bin\build\Open-FVS\python

echo %PYTHONPATH%
call python -c "import sys;print(sys.executable)"
call python -c "import sys;print(sys.version)"


:: Build and test the binary wheel
%CMD_IN_ENV% python setup.py build_ext --inplace || goto :errhdlr
%CMD_IN_ENV% python setup.py bdist_wheel || goto :errhdlr
cd dist
call pip install --no-index --find-links . pyfvs || goto :errhdlr
call fvs --run-tests || goto :errhdlr

move /y *.whl %APPVEYOR_BUILD_FOLDER%

goto :exit

:errhdlr
echo Error building wheel package
echo %cd%
dir
pip list

:exit
popd
exit /b %errorlevel%