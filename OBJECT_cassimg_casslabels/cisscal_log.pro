;;	cisscal_log.pro
;;	Version tag: $Id: cisscal_log.pro,v 1.1 2004/02/16 16:18:44 mwe Exp $
;;

PRO CISSCAL_Log, LogThing0, LogThing1, LogThing2, LogThing3, $
		FILENAME=FileName, LOGYES=LogYes, NOLOG=NoLog, APPEND=Append
;+
; NAME: CISSCAL_Log
;
; PURPOSE: 'Log to file' utility for IDL Cassini Image Calibration suite
;		Provides redirectable information/error logging.
;
; MAJOR TOPICS: CISSCAL
;
; CALLING SEQUENCE: CISSCAL_Log, LogThing0, LogThing1, LogThing2, LogThing3, $
;		FILENAME=FileName, LOGYES=LogYes, NOLOG=NoLog, APPEND=Append
;
; OPTIONAL INPUT PARAMETERSS:
;     LogThing0..3:	Information to log (eventually via a PRINTF statement)
;
; KEYWORD PARAMETERS:
;     FILENAME:		Name of a file to which to send log messages until
;			 further notice. The strings 'stdout' and 'stderr' are
;			 also recognised and handled appropriately.
;			 The string 'gui' is also recognised, and causes output
;			 to be delegated to the procedure GuiPrint.
;     NOLOG:		Switches logging OFF until further notice
;     LOGYES:		Switches logging OFF until further notice (overrides NOLOG)
;     APPEND:		If FILENAME is set, then append to that file rather than
;			 overwriting it: otherwise this keyword is ignored
; COMMON BLOCKS and STRUCTURES:
;     LogCommon (for internal persistence only).
;
; SIDE EFFECTS:
;     None.
;
; MODIFICATION HISTORY:  Initial coding: Kevin Beurle, November 2000
;                        Modifications:  consult the CISSCAL CVS repository
;-

;	Just to preserve LogLun, LogOn between invocations (c.f. 'static' in C)
COMMON LogCommon, LogLun, LogOn

;	LogLun is the LUN that we are currently sending logging information
;	 to: a value of zero means we are sending it to the GuiPrint procedure.
;	LogOn is the current logging status: log nothing if this is zero

;	Has LogLun been defined yet? Default to logging on stdout (-1) if not.
LogInfo = SIZE(LogLun)
IF ( LogInfo[LogInfo[0]+1] EQ 0 ) THEN BEGIN
  LogLun = -1
  LogOn = 1
ENDIF

;	Handle logging ON and OFF switches: ON takes precedence
IF KEYWORD_SET(NoLog) THEN LogOn = 0
IF KEYWORD_SET(LogYes) THEN LogOn = 1

;	If a filename is given then close the current
;		logfile and direct logging to the named one:
;	Close the current LUN unless it's stdout (-1) or stderr (-2)
;	If the LUN came from a GET_LUN call (100..128), we should release it
;	Reselect stdout as LogLun for safety
IF KEYWORD_SET(FileName) THEN BEGIN
  IF (LogLun GT 0 ) THEN CLOSE, LogLun
  IF (LogLun GE 100) AND (LogLun LE 128) THEN BEGIN
    FREE_LUN, LogLun
    LogLun = -1
  ENDIF
;	Now open the new log stream:
;	 just set LogLun appropriately if the name is 'stdin' or 'stderr'
;	 otherwise open the named file for output
;	Presume that logging should be switched ON unless it was explicitly denied
  IF STRLOWCASE(FileName) EQ 'stdout' THEN BEGIN
    LogLun = -1
  ENDIF ELSE IF STRLOWCASE(FileName) EQ 'stdout' THEN BEGIN
    LogLun = -2
  ENDIF ELSE IF STRLOWCASE(FileName) EQ 'gui' THEN BEGIN
    LogLun = 0
  ENDIF ELSE BEGIN
    OPENW, LogLun, FileName, /GET_LUN, APPEND=Append
  ENDELSE
  IF NOT KEYWORD_SET(NoLog) THEN LogOn = 1
ENDIF

;	If logging is switched on we can now do the actual output
IF LogOn THEN BEGIN
;	On some versions and platforms an empty LogThing0
;	 was triggering an 'undefined' error: fix this if so
  LTSize = SIZE(LogThing0)
  IF LTSize[LTSize[0]+1] EQ 0 THEN LogThing0 = '<Undefined message fix>'

  IF LogLun NE 0 THEN BEGIN
;	This is ugly, but it works for now...
;	(alas, there is no varargs equivalent in IDL)
;	Four parameters was the most I have found elsewhere in the code so far
    IF N_PARAMS() EQ 1 THEN PRINTF, LogLun, LogThing0
    IF N_PARAMS() EQ 2 THEN PRINTF, LogLun, LogThing0, LogThing1
    IF N_PARAMS() EQ 3 THEN PRINTF, LogLun, LogThing0, LogThing1, LogThing2
    IF N_PARAMS() EQ 4 THEN PRINTF, LogLun, LogThing0, LogThing1, LogThing2, LogThing3
  ENDIF ELSE BEGIN
;	And this is equally ugly
    GuiString = ''
    IF N_PARAMS() GE 1 THEN GuiString = STRING(LogThing0)
    IF N_PARAMS() EQ 2 THEN GuiString = GuiString + ' ' + STRING(LogThing1)
    IF N_PARAMS() EQ 3 THEN GuiString = GuiString + ' ' + STRING(LogThing2)
    IF N_PARAMS() EQ 4 THEN GuiString = GuiString + ' ' + STRING(LogThing3)
    GuiPrint, GuiString
  ENDELSE
ENDIF

END
