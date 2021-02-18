;***************************************************************************************************
; NAME: SGROUP_SET_VALUE
; PURPOSE: 
;***************************************************************************************************
PRO move_set_value, id, value

	COMPILE_OPT hidden		; Don't show up in HELP output unless HIDDEN keyword is used.
	ON_ERROR, 2				; Return to caller.

	; Retrieve the state.
	stash = WIDGET_INFO(id, /CHILD)
	WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY
	


	; Restore the state.
	WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY

END


;***************************************************************************************************
; NAME: SGROUP_GET_VALUE
; PURPOSE: 
;***************************************************************************************************
FUNCTION move_get_value, id

	COMPILE_OPT hidden		; Don't show up in HELP output unless HIDDEN keyword is used.
	ON_ERROR, 2				; Return to caller.

	; Retrieve the structure from the child that contains the sub ids.
	handlerChild = WIDGET_INFO(id, /CHILD)
	WIDGET_CONTROL, handlerChild, GET_UVALUE=state, /NO_COPY
	
	; Get value:
	value = {tFactor:state.tFactor, rFactor:state.rFactor}

	; Restore the state.
	WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY

    RETURN, value
END


;*******************************************************************************
; Example of mouse move routine:
;*******************************************************************************
; NAME: MOVE_MOUSE
; PURPOSE: Allow user to translate or rotate graphical objects in a window by moving the mouse in it.
;		This routine call an external routine with a translation vector or a rotation angle as 
;		parameter allowing the user to manage object(s) motion.
;
;			Translation vector: 2-Elements array of double value for x and y translation.
;			Rotation angle: Scalar double representing an angle in radian.
;
; KNOWN ISSUES:
;		To use this routine on Mac OSX, the X11 preferences have to be modified. In the "windows" 
;		tab, check all the 3 options (don't know witch particular option cause the problem !).
;-------------------------------------------------------------------------------
;PRO move_mouse, TFACTOR=tFactor, RFACTOR=rFactor, RCENTER=centerVec
;    
;    PRINT, "Press a button and move the mouse over the window:"            	
;	IF KEYWORD_SET(tFactor) THEN PRINT, "    Left button   => translate
;	IF KEYWORD_SET(rFactor) THEN PRINT, "    Middle button => rotate 
;	PRINT, "    Right button  => quit"
;	
;	WSET, winID
;	
;	IF NOT KEYWORD_SET(centerVec) THEN centerVec=[!D.X_SIZE/2.0D, !D.Y_SIZE/2.0D]
;	CURSOR, xmA, ymA, /DEVICE
;	ptA = [xmA, ymA]
;	
;	working=1
;	WHILE working EQ 1 DO BEGIN
;		
;		CURSOR, xmB, ymB, /NOWAIT, /DEVICE
;		ptB = [xmB, ymB]
;		
;		; Left click -> compute translation vector:
;		IF !mouse.button EQ	1 && ~ARRAY_EQUAL(ptA,ptB) && KEYWORD_SET(tFactor) $
;		THEN move_callback, TRANSLATE = tFactor * [ptB[0]-ptA[0], ptB[1]-ptA[1]]
;		
;		; Left click -> compute rotation angle
;		IF !mouse.button EQ	2 && ~ARRAY_EQUAL(ptA,ptB) && KEYWORD_SET(rFactor) THEN BEGIN
;			ptA_c = ptA - centerVec
;			ptB_c = ptB - centerVec
;			
;			angle = atan(ptB_c[1]/ptB_c[0])-atan(ptA_c[1]/ptA_c[0])
;			IF angle LT -!DPI/2 THEN angle += !DPI
;			IF angle GT  !DPI/2 THEN angle -= !DPI
;			
;			move_callback, ROTATE=rFactor*angle			
;		ENDIF
;		
;		;Right click -> quit the loop and stop the procedure
;		IF !mouse.button EQ 4 THEN working=0	
;		
;		; Mouse position B become mouse position A
;		ptA = ptB
;	ENDWHILE
;	
;	RETURN
;END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_XMOVE_EVENT
; PURPOSE: Manage events from the widget
;---------------------------------------------------------------------------------------------------	  		  
FUNCTION move_event, ev
	
	COMPILE_OPT hidden		; Don't show up in HELP output unless HIDDEN keyword is used.
	
	; Get state from the first child of the compound widget handler:
	handlerChild = WIDGET_INFO(ev.handler, /CHILD)
	WIDGET_CONTROL, handlerChild, GET_UVALUE=state, /NO_COPY
	
	uname = WIDGET_INFO(ev.id, /UNAME)
	CASE uname OF

		"TFACTOR": BEGIN
			state.tFactor = DOUBLE(ev.STR)
			type=0
			val=state.tFactor
		END
							
		"MOVE_UL":  BEGIN
			type=1
			val=state.tFactor*[-1, 1]
		END
		"MOVE_U": 	BEGIN 
			type=2
			val=state.tFactor*[ 0, 1]
		END
		"MOVE_UR": 	BEGIN
			type=3
			val=state.tFactor*[ 1, 1]
		END
		"MOVE_L":	BEGIN 
			type=4
			val=state.tFactor*[-1, 0]
		END
		"MOUSE":    BEGIN
			type=5
			val={tFactor:state.tFactor, rFactor:state.rFactor}
		END
		"MOVE_R": 	BEGIN 
			type=6
			val=state.tFactor*[ 1, 0]
		END
		"MOVE_DL": 	BEGIN 
			type=7
			val=state.tFactor*[-1,-1]
		END
		"MOVE_D": 	BEGIN
			type=8
			val=state.tFactor*[ 0,-1]
		END
		"MOVE_DR":  BEGIN 
			type=9
			val=state.tFactor*[ 1,-1]
		END
		
		"RFACTOR": BEGIN
			state.rFactor = DOUBLE(ev.STR)
			type=10
			val=state.rFactor
		END
		
		"ROTATE_L": BEGIN 
			type=11
			val= state.rFactor
		END
		"ROTATE_R": BEGIN 
			type=12
			val=-state.rFactor
		END

	ENDCASE
	ret = { ID:ev.handler, TOP:ev.top, HANDLER:0L, TYPE:type, VALUE:val}
	
	; Restore the state information before exiting routine:
	WIDGET_CONTROL, handlerChild, SET_UVALUE=state, /NO_COPY

	RETURN, ret
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
;
;-------------------------------------------------------------------------------
FUNCTION cw_move, parent, TRANSLATE=translate, ROTATE=rotate, $
		UVALUE = uvalue, UNAME = uname, $
		FRAME = frame, COLUMN=column, ROW=row, $
		MAP = map, SCROLL = scroll, TAB_MODE = tab_mode, SPACE = space, $
		GROUP_LEADER=grouLeader, $
		XOFFSET = xoffset, XPAD = xpad, XSIZE = xsize, X_SCROLL_SIZE = x_scroll_size, $
		YOFFSET = yoffset, YPAD = ypad, YSIZE = ysize, Y_SCROLL_SIZE = y_scroll_size
		
		
	
	;***************************** Keywords ************************************
	translate = KEYWORD_SET(translate)
	rotate    = KEYWORD_SET(rotate)
	IF ~translate && ~rotate THEN translate=(rotate=1)
	
	
	col = KEYWORD_SET(column)
	row = KEYWORD_SET(row)
	IF ~col && ~row THEN col=1
	
	
	version = WIDGET_INFO(/version)
  	IF version.toolkit EQ 'OLIT' THEN def_space_pad=4 ELSE def_space_pad=3
	IF N_ELEMENTS(space) EQ 0         THEN space = def_space_pad
	IF N_ELEMENTS(xoffset) EQ 0       THEN xoffset=0
	IF N_ELEMENTS(xpad) EQ 0          THEN xpad = def_space_pad
	IF N_ELEMENTS(xsize) EQ 0         THEN xsize = 0
	IF N_ELEMENTS(x_scroll_size) EQ 0 THEN x_scroll_size = 0
	IF N_ELEMENTS(yoffset) EQ 0       THEN yoffset=0
	IF N_ELEMENTS(ypad) EQ 0          THEN ypad = def_space_pad
	IF N_ELEMENTS(ysize) EQ 0         THEN ysize = 0	
		
	;***************************** Parameters **********************************
	tFactorList = ['0.1', '1', '10']
	tFactorList_dfltID = 1
	tFactor = translate ? DOUBLE(tFactorList[tFactorList_dfltID]) : !NULL
	
	rFactorList = ['0.1', '1', '10']
	rfactorList_dfltID = 1
	rFactor = rotate    ? DOUBLE(rFactorList[rFactorList_dfltID]) : !NULL		
	
	;***************************** Icons & Tooltips ****************************
	bttnSize = 34
	
	ico_path = GETENV('CAVIAR_ICONS_PATH')
	ico_mouse			= ico_path+'mouse.bmp'
	ico_arrow_upleft  	= ico_path+'arrow_up_left.bmp'
	ico_arrow_left  	= ico_path+'arrow_left.bmp'
	ico_arrow_downleft  = ico_path+'arrow_down_left.bmp'
	ico_arrow_upright 	= ico_path+'arrow_up_right.bmp'
	ico_arrow_right 	= ico_path+'arrow_right.bmp'
	ico_arrow_downright = ico_path+'arrow_down_right.bmp'
	ico_arrow_up 		= ico_path+'arrow_up.bmp'
	ico_arrow_down 		= ico_path+'arrow_down.bmp'
	ico_rotate_left 	= ico_path+'rot_left3.bmp'
	ico_rotate_right	= ico_path+'rot_right3.bmp'
	
	;# Tooltips
	mouse_tooltip = "Use the mouse over the image to move object." $
				 + " Push right button to translate, middle button to rotate, right button to quit"	   
		
	
	; Set 'extra' base value as 'state' structure with all needed widgets parameters:
	state = {tFactor:tFactor, rFactor: rFactor}
			 
			 
	;***************************************************************************
	; Define widgets and bases:
	;***************************************************************************
	mainBase = WIDGET_BASE(parent, UVALUE=uvalue, UNAME=uname, $
		TAB_MODE = tab_mode, $
		EVENT_FUNC = "move_event", $
		FUNC_GET_VALUE = "move_get_value", $
		PRO_SET_VALUE = "move_set_value")
	
	extraBase = WIDGET_BASE(mainBase, UVALUE=state,XPAD=0,YPAD=0)
	
	base = WIDGET_BASE(extraBase, /ALIGN_CENTER, $
		COLUMN=col, ROW=row, FRAME=frame, SCROLL=scroll, SPACE=space, $
		XPAD=xpad, XSIZE=xsize, X_SCROLL_SIZE=x_scroll_size, $
		YPAD=ypad, YSIZE=ysize, Y_SCROLL_SIZE=y_scroll_size)
		
						
	;___________________________________________________________________________
	base1 = WIDGET_BASE(base, /COL, /BASE_ALIGN_LEFT, XPAD=0, YPAD=0, SPACE=0)
		
	IF translate THEN BEGIN
		IF col THEN BEGIN
			base11=WIDGET_BASE(base1, /ROW, /BASE_ALIGN_LEFT, XPAD=0, YPAD=0)
				wLbl = WIDGET_LABEL(base11, VALUE="Shift", /ALIGN_LEFT)
				wShiftCbox = WIDGET_COMBOBOX(base11, /EDIT, XSIZE=60, $
										 	 VALUE=tFactorList, UNAME="TFACTOR")
				wLbl = WIDGET_LABEL(base11, VALUE="pixel", /ALIGN_LEFT)
		ENDIF ELSE BEGIN
			base11=WIDGET_BASE(base1, /COL, /BASE_ALIGN_LEFT, XPAD=0, YPAD=0)
				wLbl = WIDGET_LABEL(base11, VALUE="Shift (pixel) ", /ALIGN_LEFT)
				wShiftCbox = WIDGET_COMBOBOX(base11, /EDIT, XSIZE=60, XOFFSET=20, $
										 	 VALUE=tFactorList, UNAME="TFACTOR")
		ENDELSE							 	 
	ENDIF
	
	IF rotate THEN BEGIN
		IF col THEN BEGIN
			base12=WIDGET_BASE(base, /ROW, /BASE_ALIGN_LEFT, XPAD=0, YPAD=0)
				wLbl = WIDGET_LABEL(base12, VALUE="Angle", /ALIGN_LEFT)
				wAngleCbox = WIDGET_COMBOBOX(base12, /EDIT, XSIZE=60, $
										 	 VALUE=rFactorList, UNAME="RFACTOR")
				wLbl = WIDGET_LABEL(base12, VALUE="degree", /ALIGN_LEFT)
		ENDIF ELSE BEGIN
			base12=WIDGET_BASE(base1, /COL, /BASE_ALIGN_LEFT, XPAD=0, YPAD=0)
				wLbl = WIDGET_LABEL(base12, VALUE="Angle (degree) ", /ALIGN_LEFT)
				wAngleCbox = WIDGET_COMBOBOX(base12, /EDIT, XSIZE=60, XOFFSET=20, $
										 	 VALUE=rFactorList, UNAME="RFACTOR")
		ENDELSE
	ENDIF
	
	;_______________________________________________________________________
	base2 = WIDGET_BASE(base, /ROW, /ALIGN_CENTER, /BASE_ALIGN_CENTER, XPAD=0, YPAD=0, SPACE=0)
	IF translate THEN BEGIN
		wTbase = WIDGET_BASE(base2, /COLUMN, /BASE_ALIGN_CENTER, XPAD=0, YPAD=0, SPACE=0)
		
		;----------
		base21 = WIDGET_BASE(wTbase, /ROW, XPAD=0, YPAD=0, SPACE=0)
		
			wTUL = WIDGET_BUTTON(base21, UNAME="MOVE_UL", BITMAP=1, /MASK, VALUE=ico_arrow_upleft, $
					XSIZE=bttnSize, YSIZE=bttnSize)
		
			wTU  = WIDGET_BUTTON(base21, UNAME="MOVE_U", BITMAP=1, /MASK, VALUE=ico_arrow_up, $
					XSIZE=bttnSize, YSIZE=bttnSize)
		
			wTUR = WIDGET_BUTTON(base21, UNAME="MOVE_UR", BITMAP=1, /MASK, VALUE=ico_arrow_upright, $
					XSIZE=bttnSize, YSIZE=bttnSize)
		;----------
		base22 = WIDGET_BASE(wTbase, /ROW, XPAD=0, YPAD=0, SPACE=0)
		
			wTL     = WIDGET_BUTTON(base22, UNAME="MOVE_L", BITMAP=1, /MASK, VALUE=ico_arrow_left, $
						XSIZE=bttnSize, YSIZE=bttnSize)
		
			wMouse = WIDGET_BUTTON(base22, UNAME="MOUSE", XSIZE=bttnSize, YSIZE=bttnSize, $
						BITMAP=1, VALUE=ico_mouse, TOOLTIP=mouse_tooltip)
		
			wTR     = WIDGET_BUTTON(base22, UNAME="MOVE_R", BITMAP=1, /MASK, VALUE=ico_arrow_right, $
						XSIZE=bttnSize, YSIZE=bttnSize)
		;----------
		base23 =  WIDGET_BASE(wTbase, /ROW, XPAD=0, YPAD=0, SPACE=0)
		
			wTDL = WIDGET_BUTTON(base23, UNAME="MOVE_DL", XSIZE=bttnSize, YSIZE=bttnSize, $
					BITMAP=1, /MASK, VALUE=ico_arrow_downleft)
		
			wTD  = WIDGET_BUTTON(base23, UNAME="MOVE_D", XSIZE=bttnSize, YSIZE=bttnSize, $
					BITMAP=1, /MASK, VALUE=ico_arrow_down)
		
			wTDR = WIDGET_BUTTON(base23, UNAME="MOVE_DR", XSIZE=bttnSize, YSIZE=bttnSize, $
					BITMAP=1, /MASK, VALUE=ico_arrow_downright)
					
	ENDIF
	IF rotate THEN BEGIN
		;_______________________________________________________________________
		wRbase = WIDGET_BASE(base2, /COL, /ALIGN_CENTER, XPAD=0, YPAD=0, SPACE=0)							 
		
			wTRL = WIDGET_BUTTON(wRbase, UNAME="ROTATE_L", XSIZE=bttnSize, YSIZE=bttnSize, $
						BITMAP=1, VALUE=ico_rotate_left)
		
			wTRR = WIDGET_BUTTON(wRbase, UNAME="ROTATE_R", XSIZE=bttnSize, YSIZE=bttnSize, $
					BITMAP=1, VALUE=ico_rotate_right)
	ENDIF
						
	
	; Set widgets value:
	IF translate THEN WIDGET_CONTROL, wShiftCbox, SET_COMBOBOX_SELECT=tFactorList_dfltID
	IF rotate    THEN WIDGET_CONTROL, wAngleCbox, SET_COMBOBOX_SELECT=rFactorList_dfltID
	

	
	;***********************************************************************************************
	; Return the compound widget base ID:
	RETURN, mainBase
END			
