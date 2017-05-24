:: Deploy compiled wheels to PYPI
:: First argument should be the PYPI password

pushd %APPVEYOR_BUILD_FOLDER%\bin\build\Open-FVS\python

:: populate pypirc file for twine
echo [distutils]                                  > %USERPROFILE%\\.pypirc
echo index-servers =                             >> %USERPROFILE%\\.pypirc
echo     pypi                                    >> %USERPROFILE%\\.pypirc
echo [pypi]                                      >> %USERPROFILE%\\.pypirc
echo repository=https://pypi.python.org/pypi     >> %USERPROFILE%\\.pypirc
echo username=%PYPI_USERNAME%                    >> %USERPROFILE%\\.pypirc
echo password=%1                                 >> %USERPROFILE%\\.pypirc
echo [testpypi]                                  >> %USERPROFILE%\\.pypirc
echo repository=https://testpypi.python.org/pypi >> %USERPROFILE%\\.pypirc
echo username=%PYPI_USERNAME%                    >> %USERPROFILE%\\.pypirc
echo password=%1                                 >> %USERPROFILE%\\.pypirc

:: upload to pypi for windows
set HOME=%USERPROFILE%
::ps: If ($env:APPVEYOR_REPO_TAG -eq "true" -And $env:APPVEYOR_REPO_BRANCH -eq "master") { Invoke-Expression "twine upload -r pypi --skip-existing dist/*" 2>$null } Else { write-output "Not on a tag on master, won't deploy to pypi"}

if %APPVEYOR_REPO_BRANCH%=="master" (
    if %APPVEYOR_REPO_TAG%=="true" (
        echo Tagged commit on master, deploying to pypi.
        twine upload -r pypi --skip-existing dist/* 2>$null
    
    ) else (
        echo Not on a tag on master, won't deploy to pypi.
        
) else (
    if %APPVEYOR_REPO_BRANCH%=="dev" (
        :: Push to PYPI test server
        echo On dev branch, deploy to PYPI test.
        twine upload -r testpypi --skip-existing dist/* 2>$null
    
    ) else (
        echo Not dev or tag on master, won't deploy to pypi.
    
)

popd
