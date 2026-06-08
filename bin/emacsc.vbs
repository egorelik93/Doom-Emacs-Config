Dim shell, args, argStr, i
Set shell = CreateObject("WScript.Shell")

argStr = ""
For i = 0 To WScript.Arguments.Count - 1
    argStr = argStr & " """ & WScript.Arguments(i) & """"
Next

shell.Run "pwsh.exe -WindowStyle Hidden -NonInteractive -File ""%HOME%\.doom.d\bin\emacsc.ps1 """ & argStr, 0, True