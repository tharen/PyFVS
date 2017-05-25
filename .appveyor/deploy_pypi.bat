:: Deploy compiled wheels and source archives to PYPI
:: TWINE_USERNAME and TWINE_PASSWORD should set as global variables

pushd %APPVEYOR_BUILD_FOLDER%\bin\build\Open-FVS\python
pip install twine

if %APPVEYOR_REPO_BRANCH%=="master" (
    if %APPVEYOR_REPO_TAG%=="true" (
        echo Tagged commit on master, deploying to pypi.
        set TWINE_REPOSITORY_URL=https://pypi.python.org/pypi
        twine upload --skip-existing dist/* 2>$null
    
    ) else (
        echo Not on a tag on master, won't deploy to pypi.
        
) else (
    if %APPVEYOR_REPO_BRANCH%=="dev" (
        :: Push to PYPI test server
        echo On dev branch, deploy to PYPI test.
        set TWINE_REPOSITORY_URL=https://testpypi.python.org/pypi
        twine upload --skip-existing dist/* 2>$null
    
    ) else (
        echo Not dev or tag on master, won't deploy to pypi.
    
)

popd
