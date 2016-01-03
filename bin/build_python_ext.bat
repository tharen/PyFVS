
:: Hack to workaround max character variable length
:: http://stackoverflow.com/a/21724914/673590
for /f "usebackq delims=" %%f in ("pymod_source.txt") do set "pymod_source=%%f"
::set /p pymod_source=<pymod_source.txt

call f2py -h pyfvs%1.pyf -m pyfvs%1 --overwrite-signature %pymod_source%
call f2py -c -lodbc32 --fcompiler=gnu95 --compiler=mingw32 ./pyfvs%1.pyf libFVS%1_static.a
