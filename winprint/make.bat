@echo off

cd resources\
call make.bat
copy icons32x32\icons.res ..\icons.res
cd ..\

dcc32 -B WinPrint.dpr 

del icons.res