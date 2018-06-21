set PATH=%MINICONDA%;%MINICONDA%\Scripts;%PATH%
set PATH=%MINGW_PATH%\bin;%PATH%
conda config --set always_yes yes --set changeps1 no
conda config --add channels conda-forge
conda update -q conda

REM Edit the environment.yml file for the target Python version
REM call sed -i -E 's/(python=)(.*)/\1'%PYTHON_VERSION%'/' ./environment.yml
REM Create the conda environment
REM conda env create -q -n pyfvs -f %APPVEYOR_BUILD_FOLDER%\\environment.yml

set pkg=numpy pandas cython pyodbc click setuptools wheel sphinx alabaster matplotlib pip twine
conda env create --force -q -n pyfvs python=%PYTHON_VERSION% %pkg%
pip install pytest sphinxcontrib-napoleon

conda info -a
