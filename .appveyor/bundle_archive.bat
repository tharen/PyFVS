set ARCHIVE_NAME=open-fvs-%APPVEYOR_REPO_TAG_NAME%-Python%PYTHON_VERSION%_%PYTHON_ARCH%-windows-ci.zip
pushd bin\\build
7z a -bd .\artifacts\%ARCHIVE_NAME% Open-FVS\*
popd
exit /b 0