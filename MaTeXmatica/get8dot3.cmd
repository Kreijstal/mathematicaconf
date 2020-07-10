@echo off

::Convert double backslashes to single backslashes
set STRING=%1
SET STRING=%STRING:"=%
SET STRING=%STRING:"=%

::Convert long path (with spaces) into a short path
for %%x in ("%STRING%") do set JavaHome=%%~dpsx
echo %JavaHome%

start %JavaHome%

:DONE

