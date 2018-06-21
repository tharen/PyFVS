@echo off
:: Deploy compiled wheels and source archives to PYPI
:: TWINE_USERNAME and TWINE_PASSWORD should set as global variables

pushd %APPVEYOR_BUILD_FOLDER%\bin\build\Open-FVS\python

if "%APPVEYOR_REPO_BRANCH%" == "dev" (
    conda install twine --yes -c conda-forge
    echo On dev branch, deploy to PYPI test.
    call twine upload dist/* --skip-existing --repository-url https://test.pypi.org/legacy/ || goto twineerror
    goto finish
    )

if "%APPVEYOR_REPO_BRANCH%" == "master" (
    if "%APPVEYOR_REPO_TAG%" == "true" (
        conda install twine --yes -c conda-forge
        echo Tagged revision on master, deploy to PYPI.
        call twine upload dist/* --skip-existing || goto twineerror
        goto finish
    ))

echo Not on dev and not a tag on master, skipping pypi deploy.
goto exit

:twineerror
echo ERROR: Twine upload failed!
goto exit

:finish
echo Twine upload complete.
goto exit

:exit
popd
exit 0
