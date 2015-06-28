@ECHO OFF
FOR %%C IN (
    %EMACS%
    d:\tools\emacs-24\bin\runemacs.exe
	d:\emacs-24\bin\runemacs.exe
    %~dp0\emacs\bin\runemacs.exe
    ) DO (
    echo Trying %%C
    IF EXIST %%C (
        SET RUNEMACS=%%C
        GOTO FOUND
    )
)

:NOTFOUND
echo Could not find Emacs. Consider setting the EMACS environment variable.
pause
exit /B 1

:FOUND
set HOME=%~dp0
set EMACSLOADPATH=%~dp0;
start %RUNEMACS% --debug-init || pause
