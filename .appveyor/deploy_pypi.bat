@echo off
:: Deploy compiled wheels and source archives to PYPI
:: TWINE_USERNAME and TWINE_PASSWORD should set as global variables

pushd %APPVEYOR_BUILD_FOLDER%\bin\build\Open-FVS\python

if "%APPVEYOR_REPO_BRANCH%" == "dev" (
    echo On dev branch, deploy to PYPI test.
    
    echo Upload dev package to test.pypi.org.
    call twine upload dist/* --skip-existing --repository-url https://test.pypi.org/legacy/ && (
        echo twine upload complete.
    ) || (
        echo twine upload failed!
    )
)

if "%APPVEYOR_REPO_BRANCH%" == "master" (
    if "%APPVEYOR_REPO_TAG%" == "true" (
        echo Upload package to pypi.org.
        call twine upload dist/* --skip-existing && (
            echo twine upload complete.
        ) || (
            echo twine upload failed!
        )
)

echo Not on dev and not a tag on master, skipping pypi deploy.
goto exit

:exit
popd
exit 0
