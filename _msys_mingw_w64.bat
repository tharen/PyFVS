@echo off
:: Build envirnonment for PyFVS

set srcroot=%~dp0

set USERPROFILE=c:\users\tharen
set HOMEDRIVE=c:
set HOMEPATH=\users\tharen
set HOME=%HOMEDRIVE%%HOMEPATH%

::set PATH=C:\progs\cmake\bin
set PATH=c:\progs\Notepad++

set PATH=C:\Anaconda3;C:\Anaconda3\DLLs;C:\Anaconda3\Scripts;%PATH%

set PYTHONPATH=%srcroot%\bin\open-fvs\python
set PATH=%srcroot%\bin\open-fvs\bin;%PATH%

:: Activate the target Python environment
call c:\Anaconda3\Scripts\activate.bat pyfvs_python3.5

set MSYSTEM=MINGW64

set CHERE_INVOKING=1
start c:\progs\console2\console.exe ^
		-d %srcroot% ^
		-w "Open-FVS (MSY2+MinGW-w64 %MSYSTEM%)" ^
		-r "cmd /C C:\progs\msys64\usr\bin\bash.exe --login -i"
