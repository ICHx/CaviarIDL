;###############################################################################
;#	                    Manage events of the widget 						   #
;###############################################################################
PRO caviar_xvarinfo_event, event
		
	; Get state from the first child of the widget root:
	child = WIDGET_INFO(event.HANDLER, /CHILD)
	WIDGET_CONTROL, child, GET_UVALUE=pState
	
	parent = WIDGET_INFO(event.ID, /PARENT)
	WIDGET_CONTROL, event.HANDLER, TLB_GET_OFFSET=tlbOffset
	
;	CATCH, Error_status
;	IF Error_status NE 0 THEN BEGIN
;		res = DIALOG_MESSAGE(!ERROR_STATE.MSG, DIALOG_PARENT=event.HANDLER, /CENTER)
;		RETURN	
;	ENDIF
	
	uname = WIDGET_INFO(event.id, /UNAME)
	CASE uname OF
		"TOP_BASE": BEGIN
			IF ISA(event, 'WIDGET_BASE') THEN BEGIN
				WIDGET_CONTROL, /HOURGLASS
				
				WIDGET_CONTROL, (*pstate).wVarTbl, SCR_XSIZE=event.X, SCR_YSIZE=event.Y
				
			ENDIF
		END
		
		ELSE:
	ENDCASE
				
END
		
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;       CAVIAR_XVARINFO
; PURPOSE:
;		      
;		
; CATEGORY:
;       Widgets
;
; CALLING SEQUENCE:
;       caviar_xvarinfo
; INPUTS:
;       var
; KEYWORDS:
;		GROUP
; OUTPUTS:
;       None.
; COMMON BLOCKS:
;       None.
; SIDE EFFECTS:
;       A widget window is created.
; RESTRICTIONS:
;       
; MODIFICATION HISTORY:
;		2013, October			MEUNIER L-E		OBSPM>IMCCE
;			- Written
;-------------------------------------------------------------------------------

PRO caviar_xvarinfo, var, TITLE=title, GROUP=group
	
		
	; Test if widget window is already launched
	;IF XRegistered('caviar_xvarinfo') NE 0 THEN RETURN
	
	IF ~KEYWORD_SET(title) THEN title='CaVIaR_XVARINFO'
	
	; Get Monitor information
	oInfo = OBJ_NEW('IDLsysMonitorInfo')
	rects = oInfo->GetRectangles(/EXCLUDE_TASKBAR)
	OBJ_DESTROY, oInfo

	max = MAX(rects[3,*], monIndex)
	scrXsize = rects[2,monIndex]-32
	scrYsize = rects[3,monIndex]-80
	IF (SIZE(var))[0] EQ 1 THEN BEGIN
		var = TRANSPOSE(var)
		tblWidth = !D.X_CH_SIZE*(MAX(STRLEN(var[0,*]))+2)
	ENDIF ELSE tblWidth = !D.X_CH_SIZE*[MAX(STRLEN(var[0,*]))+2, MAX(STRLEN(var[1,*]))+2]
	
			 
	;***************************************************************************
	; Create widget
	;***************************************************************************
	mainBase = WIDGET_BASE(GROUP=group, TITLE=title, UNAME="TOP_BASE", /COLUMN, $
					/TLB_SIZE_EVENTS, XOFFSET=rects[0,monIndex]+32, $
									  YOFFSET=rects[1,monIndex]+22)
	extraBase = WIDGET_BASE(mainBase, /ROW, XPAD=0, YPAD=0, SPACE=0)	
	
									
		wVarTbl = WIDGET_TABLE(extraBase, UNAME="VAR_TABLE", $
			VALUE=var, /NO_ROW_HEADERS, /NO_COLUMN_HEADERS, ALIGNMENT=0, $
			COLUMN_WIDTHS=tblWidth, BACKGROUND_COLOR=[200,200,200], $
			/SCROLL, SCR_XSIZE=(TOTAL(tblWidth)+25<scrXsize), SCR_YSIZE=scrYsize)

						
	;___________________________________________________________________________
	
	WIDGET_CONTROL, mainBase, /REALIZE, /HOURGLASS
	
	
	
	state = {wVarTbl:wVarTbl}
	WIDGET_CONTROL, extraBase, SET_UVALUE=PTR_NEW(state, /NO_COPY)
					
	XMANAGER, 'caviar_xvarinfo', mainBase, /JUST_REG
END
