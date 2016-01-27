
set proj_root=%~dp0

::set PATH=C:\progs\msys64\mingw64\bin
REM set PATH=C:\progs\mingw-w64\x86_64-5.1.0-win32-seh-rt_v4-rev0\mingw64\bin
REM set PATH=C:\progs\mingw-w64\x86_64-5.3.0-posix-seh-rt_v4-rev0\mingw64\bin
set PATH=C:\progs\mingw-w64\x86_64-5.3.0-win32-sjlj-rt_v4-rev0\mingw64\bin
set PATH=%PATH%;C:\Windows\System32;C:\Windows
set PATH=C:\progs\cmake\bin;%PATH%
set PATH=C:\Ruby22-x64\bin;%PATH%
set PATH=C:\progs\Git\bin;C:\progs\Git\usr\bin;%PATH%

::set PATH=C:\Anaconda3;C:\Anaconda3\Scripts;%PATH%

::call C:\Anaconda3\Scripts\activate.bat pyfvs_python3.5

start c:\progs\console2\console.exe ^
		-d %proj_root% ^
		-w "PyFVS (CMD+MinGW-w64)" ^
