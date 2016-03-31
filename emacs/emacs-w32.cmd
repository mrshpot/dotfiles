@ECHO OFF
TITLE Emacs

FOR %%C IN (
    %EMACS%
    d:\tools\emacs-24\bin\runemacs.exe
	d:\emacs-24\bin\runemacs.exe
    %~dp0\emacs\bin\runemacs.exe
    ) DO (
    echo Trying %%C
    IF EXIST %%C (
        SET RUNEMACS=%%C
        GOTO EMACS_FOUND
    )
)

:EMACS_NOTFOUND
echo Could not find Emacs. Consider setting the EMACS environment variable.
pause
exit /B 1

:EMACS_FOUND
set EMACSLOADPATH=%~dp0;%~dp0\site-lisp;
set HOME=%USERPROFILE%

FOR %%C IN (
	%MINGW_PATH%
	d:\mingw
	) DO (
	IF EXIST %%C (
		SET MSYSTEM=MINGW32
		echo Starting Emacs under MinGW.
		start %%C\msys\1.0\bin\sh.exe --login -c "$RUNEMACS"
		exit /B
	) ELSE (
		echo debug %%C is not a MinGW installation.
	)
)
echo Starting Emacs without MinGW.
start %RUNEMACS% --debug-init || pause
