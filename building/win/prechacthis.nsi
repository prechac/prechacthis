; prechacthis.nsi
;
;--------------------------------

; The name of the installer
Name "PrechacThis"

; The file to write
OutFile "PrechacThis_Installer.exe"

; The default installation directory
InstallDir $PROGRAMFILES\PrechacThis

; Registry key to check for directory (so if you install again, it will 
; overwrite the old one automatically)
InstallDirRegKey HKLM "Software\NSIS_PrechacThis" "Install_Dir"

; Request application privileges for Windows Vista
RequestExecutionLevel admin

;--------------------------------

; Pages

Page components
Page directory
Page instfiles

UninstPage uninstConfirm
UninstPage instfiles

;--------------------------------

; The stuff to install
Section "PrechacThis (required)"

  SectionIn RO
  
  ; Set output path to the installation directory.
  SetOutPath $INSTDIR
  
  ; Put file there
  File "prechacthis.exe"
  File "http_stream.dll"
  File "libpl.dll"
  File "memfile.dll"
  File "mime.dll"
  File "plterm.dll"
  File "pthreadVC.dll"
  File "readutil.dll"
  File "sgml2pl.dll"
  File "socket.dll"
  File "time.dll"
  
  ; Write the installation path into the registry
  WriteRegStr HKLM SOFTWARE\NSIS_PrechacThis "Install_Dir" "$INSTDIR"
  
  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\PrechacThis" "DisplayName" "NSIS Example2"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\PrechacThis" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\PrechacThis" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\PrechacThis" "NoRepair" 1
  WriteUninstaller "uninstall.exe"
  
SectionEnd

; Optional section (can be disabled by the user)
Section "Start Menu Shortcuts"

  CreateDirectory "$SMPROGRAMS\PrechacThis"
  CreateShortCut "$SMPROGRAMS\PrechacThis\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
  CreateShortCut "$SMPROGRAMS\PrechacThis\PrechacThis.lnk" "$INSTDIR\prechacthis.exe" "" "$INSTDIR\prechacthis.exe" 0
  
SectionEnd

;--------------------------------

; Uninstaller

Section "Uninstall"
  
  ; Remove registry keys
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\PrechacThis"
  DeleteRegKey HKLM SOFTWARE\NSIS_PrechacThis

  ; Remove files and uninstaller
  Delete $INSTDIR\*.*

  ; Remove shortcuts, if any
  Delete "$SMPROGRAMS\PrechacThis\*.*"

  ; Remove directories used
  RMDir "$SMPROGRAMS\PrechacThis"
  RMDir "$INSTDIR"

SectionEnd
