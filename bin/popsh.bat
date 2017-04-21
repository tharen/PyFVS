@echo off&cls
setlocal EnableDelayedExpansion
set $line=%path%
set $line=%$line: =#%
set $line=%$line:;= %

for "tokens=*" %%a in (%$line%) do echo "%%a" | find /i "git" || set $newpath=!$newpath!;"%%a"
REM set $newpath=!$newpath:#= !
REM echo set path=!$newpath:~1!