
set proj_root=%~dp0

set PATH=C:\progs\mingw-w64\i686-5.3.0-win32-sjlj-rt_v4-rev0\mingw32\bin
set PATH=%PATH%;C:\Windows\System32;C:\Windows
set PATH=C:\progs\cmake\bin;%PATH%
set PATH=C:\Ruby22-x64\bin;%PATH%
set PATH=C:\progs\Git\bin;C:\progs\Git\usr\bin;%PATH%

start c:\progs\console2\console.exe ^
		-d %proj_root% ^
		-w "PyFVS (CMD+MinGW-w32)" ^
