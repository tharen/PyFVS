set ARCHIVE_NAME=pyfvs-%APPVEYOR_BUILD_VERSION%-Python%PYTHON_VERSION%_%PYTHON_ARCH%-windows-ci.zip
cd bin\\build
7z a -bd %APPVEYOR_BUILD_FOLDER%\%ARCHIVE_NAME% Open-FVS\*
popd
exit /b 0