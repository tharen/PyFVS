@echo off
:: Refresh the *.sum.save files from the official FVS release.

set fvs_bin=c:\fvsbin

set variants=pn wc so ca op oc ec

for %%v in (%variants%) do call :run_fvs %%v
goto end

:run_fvs
echo FVS Variant: %1
set v=%1
set fn=%1_bareground
call %fvs_bin%\fvs%v%.exe --keywordfile=%fn%.key
echo move %fn%.sum %fn%.sum.save

goto :eof

:end
