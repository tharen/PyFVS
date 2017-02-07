
:popsh
::@echo off&cls
setlocal EnableDelayedExpansion
set old_path=%path%
set $line=%path%
set $line=%$line: =#%
set $line=%$line:;= %

for %%a in (%$line%) do echo %%a | find /i "sh" || set $newpath=!$newpath!;%%a
set $newpath=!$newpath:#= !
echo set path=!$newpath:~1!