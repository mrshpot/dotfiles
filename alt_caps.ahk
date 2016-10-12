;
; AutoHotkey Version: 1.x
; Language:       English
; Platform:       Win9x/NT
; Author:         Taras Shpot <mrshpot@gmail.com>
;
; Script Function:
;	Switch language
;

#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.

!Capslock::
  SetFormat, Integer, H
  Locale1=0x4090409  ; English
  Locale2=0xf0a80422  ; Extended Ukrainian
  WinGet, WinID,, A
  ThreadID:=DllCall("GetWindowThreadProcessId", "Int", WinID, "Int", "0")
  InputLocaleID:=DllCall("GetKeyboardLayout", "Int", ThreadID)
  if(InputLocaleID=Locale1)
    SendMessage, 0x50,, % Locale2,, A
  else
    SendMessage, 0x50,, % Locale1,, A
Exit
