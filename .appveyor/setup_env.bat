:: Prepare the environment
call %PYTHON%\\Scripts\\conda create -q -y --name=%ENV_NAME% --file %APPVEYOR_BUILD_FOLDER%\\requirements.txt
call %PYTHON%\\Scripts\\conda info --envs
call %PYTHON%\\Scripts\\activate %ENV_NAME%
set PYTHON_HOME=%PYTHON%\\envs\\%ENV_NAME%

:: Install nose-parameterized with pip until is becomes available on Anaconda
::%PYTHON_HOME%\\Scripts\\pip install git+https://github.com/wolever/nose-parameterized.git
call %PYTHON_HOME%\\Scripts\\pip install nose-parameterized

:: Python and CMake include with MSYS MinGW conflict with the target executables
del C:\\msys64\\Mingw64\\bin\\python.exe
del C:\\msys64\\Mingw64\\bin\\cmake.exe