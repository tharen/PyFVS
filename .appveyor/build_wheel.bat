
set pythonpath=

:: Make sure the named Python environment is active
call %PYTHON%\Scripts\activate %ENV_NAME%
pip install --upgrade nose-parameterized nose2 wheel

:: Move into the package parent folder
pushd %APPVEYOR_BUILD_FOLDER%\bin\build\Open-FVS\python

call python setup.py bdist_wheel || goto :err_hldr
cd dist
call pip install --no-index --find-links . pyfvs || goto :err_hdlr
call fvs --run-tests || goto :err_hdlr

move /y pyfvs*.whl %APPVEYOR_BUILD_FOLDER%

goto :exit

:err_hdlr
echo Error building wheel package
echo %cd%
dir
pip list

:exit
popd
exit /b %errorlevel%