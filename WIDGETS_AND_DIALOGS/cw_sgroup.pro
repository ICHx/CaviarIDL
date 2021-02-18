;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;	CW_SGROUP
; PURPOSE:
;	CW_SGROUP is a compound widget that simplifies creating a base of sliders with a title for each
;	one (set in input), the value of each slider and a button to return to the initial value.
;	Events for the individual sliders are handled transparently, and a CW_SGROUP event returned.
;   This event return, in addition to 'ID', 'TOP, 'HANDLER', the index of the slider within the 
;	base which have been changed and the current values of all sliders.
;
; CATEGORY:
;	Compound widgets.
;
; CALLING SEQUENCE:
;	widget = CW_SGROUP(parent, nsliders)
;
; INPUTS:
;       PARENT: The ID of the parent widget.
;		NSLIDERS: Number of sliders to create/manage.
;
; KEYWORD PARAMETERS:
;	COLUMN: Sliders will be arranged in the number of columns specified by this keyword.
;   FONT: The name of the font to be used for the button titles. If this keyword is not specified,
;		the default font is used.
;   FRAME: Specifies the width of the frame to be drawn around the base.
;   MAP: If set, the base will be mapped when the widget is realized (the default).
;	ROW: Buttons will be arranged in the number of rows specified by this keyword.
;   SCROLL:  If set, the base will include scroll bars to allow viewing a large base through a 
;		smaller viewport.
;	VALUE: The initial value of the buttons. This is equivalent to the later statement:
;       WIDGET_CONTROL, widget, set_value=value
;
;   SPACE: The space, in pixels, to be left around the edges of a row or column major base.
;	UVALUE: The user value to be associated with the widget.
;   UNAME: The user name to be associated with the widget.
;   XOFFSET: The X offset of the widget relative to its parent.
;   XPAD: The horizontal space, in pixels, between children of a row or column major base.
;   XSIZE: The width of the base.
;   X_SCROLL_SIZE: The width of the viewport if SCROLL is specified.
;   YOFFSET: The Y offset of the widget relative to its parent.
;   YPAD: The vertical space, in pixels, between children of a row or column major base.
;   YSIZE: The height of the base.
;   Y_SCROLL_SIZE: The height of the viewport if SCROLL is specified.
;	X_SLIDER_SIZE: The width of slider.
;	X_LABEL_SIZE: The width of the label frame. Does not change the text size!
;	Y_SLIDER_SIZE: The height of the slider.
;	Y_LABEL_SIZE: The height of the label frame. Does not change the text size!
; OUTPUTS:
;       The ID of the created widget is returned.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;   This widget generates event structures with the following definition:
;
;       event = { ID:0L, TOP:0L, HANDLER:0L, INDEX:0, VALUE:0 }
;
;   The INDEX field return the index of the slider that generate the event.
;	The VALUE field return the values of all the sliders created/managed by the widget.
;
; PROCEDURE:
;	WIDGET_CONTROL, id, SET_VALUE=value can be used to change the
;		current value(s) displayed by the widget.
;
;	WIDGET_CONTROL, id, GET_VALUE=var can be used to obtain the current
;		value(s) displayed by the widget.
;
; MODIFICATION HISTORY:
;	2013 february 12		L-E. MEUNIER			OBSPM>IMCCE
;		- Created
;	2013 october 24			L-E. MEUNIER			OBSPM>IMCCE
;		- Correct errors in 'sgroup_set_value' procedure.
;---------------------------------------------------------------------------------------------------

;***************************************************************************************************
; NAME: SGROUP_VALUE2INT
; PURPOSE: Calculate integer slider value from floating-point.
; 		   Note: The "value" argument will be truncated to lie within bottom...top.
;***************************************************************************************************
FUNCTION sgroup_value2int, val, min, max, step
	COMPILE_OPT hidden
	
	; Make sure new value is within range. Different test if bottom > top.
    FOR i=0, N_ELEMENTS(val)-1 $
    DO val[i] = (min[i] LE max[i]) ? min[i] > val[i] < max[i] : max[i] > val[i] < min[i]

    RETURN, ROUND((val-min)/step)
END

;*******************************************************************************
; NAME: SGROUP_INT2VALUE
; PURPOSE: Calculate floating-point value from integer slider.
;*******************************************************************************
FUNCTION sgroup_int2value, int, min, max, step
	COMPILE_OPT hidden
    RETURN, int*step+min
END


;*******************************************************************************
; NAME: SGROUP_SET_VALUE
; PURPOSE: 
;*******************************************************************************
PRO sgroup_set_value, id, value

	COMPILE_OPT hidden		; Don't show up in HELP output unless HIDDEN keyword is used.
	ON_ERROR, 2				; Return to caller.

	; Retrieve the state.
	stash = WIDGET_INFO(id, /CHILD)
	WIDGET_CONTROL, stash, GET_UVALUE=pState
	
	; Get slider value rounded to step:
	IF N_ELEMENTS(value) NE N_ELEMENTS((*pState).step) $
	THEN MESSAGE, "The value must have the same size as sliders in the widget"
	ival = sgroup_value2int(value, (*pState).min, (*pState).max, (*pState).step)
	
	; Set the value for the 'state':
	(*pState).val = value
	
	; Set the value for the widgets:
	FOREACH parent, (*pState).sbaseId DO BEGIN
		WIDGET_CONTROL, parent, GET_UVALUE=i
		FOREACH child_L1, WIDGET_INFO(parent, /ALL_CHILDREN) DO BEGIN
			CASE WIDGET_INFO(child_L1, /NAME) OF
				'LABEL': WIDGET_CONTROL, child_L1, SET_VALUE=(*pState).lblTxt[i]+STRING(value[i], FORMAT=(*pState).lblFormat[i])
				'BASE': BEGIN
					FOREACH child_L2, WIDGET_INFO(child_L1, /ALL_CHILDREN) DO BEGIN
						IF WIDGET_INFO(child_L2, /NAME) EQ 'SLIDER' $
						THEN WIDGET_CONTROL, child_L2, SET_VALUE=ival[i]
					ENDFOREACH
				END
				ELSE:
			ENDCASE
		ENDFOREACH
	ENDFOREACH

END


;***************************************************************************************************
; NAME: SGROUP_GET_VALUE
; PURPOSE: 
;***************************************************************************************************
FUNCTION sgroup_get_value, id

	COMPILE_OPT hidden		; Don't show up in HELP output unless HIDDEN keyword is used.
	ON_ERROR, 2				; Return to caller.

	; Retrieve the structure from the child that contains the sub ids.
	handlerChild = WIDGET_INFO(id, /CHILD)
	WIDGET_CONTROL, handlerChild, GET_UVALUE=pState
	
	; Get value:
	value = (*pState).val

    RETURN, value
END

;*******************************************************************************
; NAME: SGROUP_EVENT
; PURPOSE: 
;*******************************************************************************
FUNCTION sgroup_event, ev

	
	COMPILE_OPT hidden		; Don't show up in HELP output unless HIDDEN keyword is used.
	
	; Get state from the first child of the compound widget handler:
	handlerChild = WIDGET_INFO(ev.handler, /CHILD)
	WIDGET_CONTROL, handlerChild, GET_UVALUE=pState
	
	
	; Get the value:
	CASE WIDGET_INFO(ev.id, /UNAME) OF
		; Get slider value:
		'SLIDER': BEGIN
			ival = ev.value
			WIDGET_CONTROL, ev.id, GET_UVALUE=wLblId
		END

		; Get initial value store in the reset button UVALUE and update the slider:
		'BUTTON': BEGIN
			WIDGET_CONTROL, ev.id, GET_UVALUE=rstBttn_uvalue
			ival   = rstBttn_uvalue.ival
			wLblId = rstBttn_uvalue.wLblID
			WIDGET_CONTROL, rstBttn_uvalue.wSldID, SET_VALUE=ival		
		END
	ENDCASE
	
	; Get slider index store in its main parent base UVALUE:
	WIDGET_CONTROL, WIDGET_INFO(wLblId, /PARENT), GET_UVALUE=index

	
	; Convert the new value to float and set state:
	(*pState).val[index] = sgroup_int2value(ival, (*pState).min[index], $
							(*pState).max[index], (*pState).step[index])
				
	; Update label:
	txt = (*pState).lblTxt[index] + STRING((*pState).val[index], FORMAT=(*pState).lblFormat[index])
	WIDGET_CONTROL, wLblId, SET_VALUE=txt
	
		
	ret = { ID:ev.handler, TOP:ev.top, HANDLER:0L, INDEX:index, VALUE:(*pState).val}	
		
  RETURN, ret
END



;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CW_SGROUP
; PURPOSE:
;-------------------------------------------------------------------------------
FUNCTION cw_sgroup, parent, nsliders, $
    TEXT = txt, VALUE = val, MINIMUM = min, MAXIMUM = max, STEP = step, FORMAT = format, $
    UVALUE = uvalue, $
    UNAME = uname, $
	COLUMN = column, ROW = row, FRAME = frame, LABEL_TOP=label_top, $
	MAP = map, SCROLL = scroll, TAB_MODE = tab_mode, SPACE = space, $
    XOFFSET = xoffset, XPAD = xpad, XSIZE = xsize, X_SCROLL_SIZE = x_scroll_size, $
    YOFFSET = yoffset, YPAD = ypad, YSIZE = ysize, Y_SCROLL_SIZE = y_scroll_size, $
    X_SLIDER_SIZE = x_slider_size, X_LABEL_SIZE = x_label_size, $
	Y_SLIDER_SIZE = y_slider_size, Y_LABEL_SIZE = y_label_size
	
	
	IF N_PARAMS() NE 2 $
	THEN MESSAGE, 'Must specify a parent for CW_SGROUP and number of sliders to display'
	;ON_ERROR, 2					;return to caller
	
	;***********************************************************************************************
	; Defaults for keywords	
	version = WIDGET_INFO(/version)
  	IF version.toolkit eq 'OLIT' THEN def_space_pad=4 ELSE def_space_pad=3
	IF N_ELEMENTS(uname) EQ 0         THEN uname = 'CW_SGROUP_UNAME'
	IF N_ELEMENTS(uvalue) EQ 0        THEN uvalue = 0
	
  	IF N_ELEMENTS(column) EQ 0        THEN column = 0
	IF N_ELEMENTS(row) EQ 0           THEN row = 1 - (column = 1)
	IF N_ELEMENTS(frame) EQ 0         THEN frame = 0
	IF N_ELEMENTS(label_top) EQ 0     THEN label_top = 0
	
	IF N_ELEMENTS(map) EQ 0     	  THEN map = 1
	IF N_ELEMENTS(scroll) EQ 0        THEN scroll = 0
	IF N_ELEMENTS(tab_mode) NE 0      THEN tab_mode = 0
	
	IF N_ELEMENTS(space) EQ 0         THEN space = def_space_pad
	IF N_ELEMENTS(xoffset) EQ 0       THEN xoffset=0
	IF N_ELEMENTS(xpad) EQ 0          THEN xpad = def_space_pad
	IF N_ELEMENTS(xsize) EQ 0         THEN xsize = 0
	IF N_ELEMENTS(x_scroll_size) EQ 0 THEN x_scroll_size = 0
	IF N_ELEMENTS(x_slider_size) EQ 0 THEN x_slider_size = 0
	IF N_ELEMENTS(x_label_size)  EQ 0 THEN x_label_size = 0
	IF N_ELEMENTS(yoffset) EQ 0       THEN yoffset=0
	IF N_ELEMENTS(ypad) EQ 0          THEN ypad = def_space_pad
	IF N_ELEMENTS(ysize) EQ 0         THEN ysize = 0
	IF N_ELEMENTS(y_scroll_size) EQ 0 THEN y_scroll_size = 0
	IF N_ELEMENTS(y_slider_size) EQ 0 THEN y_slider_size = 0
	IF N_ELEMENTS(y_label_size)  EQ 0 THEN y_label_size = 0
	
	IF N_ELEMENTS(txt) NE nsliders    THEN txt = REPLICATE('', nsliders)
	IF N_ELEMENTS(val) NE nsliders    THEN val = REPLICATE(50, nsliders)
	IF N_ELEMENTS(min) NE nsliders    THEN min = REPLICATE(0, nsliders)
	IF N_ELEMENTS(max) NE nsliders    THEN max = REPLICATE(100, nsliders)
	IF N_ELEMENTS(step) NE nsliders   THEN step = REPLICATE(1, nsliders)
	IF N_ELEMENTS(format) NE nsliders THEN format = REPLICATE('(G0)', nsliders)
	
	
	
	ival = sgroup_value2int(val, min, max, step)
	imin = sgroup_value2int(min, min, max, step)
	imax = sgroup_value2int(max, min, max, step)
	
	
	
	
	;***********************************************************************************************
	; Define widgets and bases:
	mainBase = WIDGET_BASE(parent, UVALUE=uvalue, UNAME=uname, $
		TAB_MODE = tab_mode, $
		EVENT_FUNC = "sgroup_event", $
		FUNC_GET_VALUE = "sgroup_get_value", $
		PRO_SET_VALUE = "sgroup_set_value")
	
	extraBase = WIDGET_BASE(mainBase)
	
	
	base = WIDGET_BASE(extraBase, /ALIGN_CENTER, $
		COLUMN=column, ROW=row, FRAME=frame, SCROLL=scroll, SPACE=space, $
		XPAD=xpad, XSIZE=xsize, X_SCROLL_SIZE=x_scroll_size, $
		YPAD=ypad, YSIZE=ysize, Y_SCROLL_SIZE=y_scroll_size)
    
    
    sbase = LONARR(nsliders)
	FOR i=0, nsliders-1 DO BEGIN
		
		IF label_top $
		THEN sbase[i] = WIDGET_BASE(base, UVALUE=i, /COLUMN) $
		ELSE sbase[i] = WIDGET_BASE(base, UVALUE=i, /ROW)
		
		IF N_ELEMENTS(font) EQ 0 THEN BEGIN
			
			wLbl  = WIDGET_LABEL(sbase[i], /ALIGN_LEFT, XSIZE=x_label_size, YSIZE=y_label_size, $
				VALUE=txt[i]+STRING(val[i], FORMAT=format[i]), UNAME='LABEL')
			
			ssbase = WIDGET_BASE(sbase[i], /ROW, XPAD=0, YPAD=0, SPACE=0)
				wSldr = WIDGET_SLIDER(ssbase, /SUPP, /DRAG, XSIZE=x_slider_size, $
					VAL=ival[i], MIN=imin[i], MAX=imax[i], UNAME='SLIDER', UVALUE=wLbl)
			
				wBttn = WIDGET_BUTTON(ssbase, UNAME='BUTTON', VALUE="X", $;"><", $
					UVALUE={ival:ival[i], wSldID:wSldr, wLblID:wLbl}, $
					XSIZE=17, YSIZE=19, /ALIGN_CENTER, TOOLTIP='Reset to default value')
		
		ENDIF ELSE BEGIN
			
			wLbl  = WIDGET_LABEL(sbase[i], /ALIGN_LEFT, XSIZE=x_label_size, YSIZE=y_label_size, $
				VALUE=txt[i]+STRING(val[i], FORMAT=format[i]), UNAME='LABEL', FONT=font)
			
			ssbase = WIDGET_BASE(sbase[i], COLUMN=row, ROW=column, XPAD=0, YPAD=0, SPACE=0)
				wSldr = WIDGET_SLIDER(ssbase, /SUPP, /DRAG, XSIZE=x_slider_size, FONT=font, $
					VAL=ival[i], MIN=imin[i], MAX=imax[i], UNAME='SLIDER', UVALUE=wLbl)
			
				wBttn = WIDGET_BUTTON(ssbase, VALUE="Reset", YSIZE=16, UNAME='BUTTON', FONT=font, $
					UVALUE={ival:ival[i], wSldID:wSldr, wLblID:wLbl})
			
		ENDELSE
		
	ENDFOR
	
	state = {lblTxt:txt, lblFormat:format, $
			 min:min, max:max, step:step, val:val, $
			 sbaseId: sbase} 
	WIDGET_CONTROL, extraBase, SET_UVALUE=PTR_NEW(state, /NO_COPY)
	
	;***********************************************************************************************
	; Return the compound widget base ID:
	RETURN, mainBase
END
