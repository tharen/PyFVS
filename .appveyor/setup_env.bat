:: Create a conda environment
call %PYTHON%\Scripts\conda create -q -y --name=%ENV_NAME% ^
    --file %APPVEYOR_BUILD_FOLDER%\.appveyor\requirements_conda.txt
call %PYTHON%\Scripts\conda info --envs
call %PYTHON%\Scripts\activate %ENV_NAME%
set PYTHON_HOME=%PYTHON%\envs\%ENV_NAME%

:: Install requirements not provided by Anaconda
::%PYTHON_HOME%\Scripts\pip install git+https://github.com/wolever/nose-parameterized.git
call %PYTHON_HOME%\Scripts\pip install ^
    -r %APPVEYOR_BUILD_FOLDER%\.appveyor\requirements_pip.txt
