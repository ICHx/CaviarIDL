PRO dialog_getValue_event, event
	
	COMMON DIALOG_GETVALUE, dgv_value, done_type
	
	uname = WIDGET_INFO(event.id, /UNAME)
	CASE uname OF
		"COMBOBOX": BEGIN
			WIDGET_CONTROL, event.id, GET_UVALUE=vList
			dgv_value = (event.index EQ -1) ? event.str : vList[event.index]
		END
		"BUTTONS": BEGIN
			done_type = 1-event.value
			WIDGET_CONTROL, event.top, /DESTROY
		END
		ELSE:
	ENDCASE
	
	RETURN
END




FUNCTION dialog_getValue, LABEL=label, LIST_VALUES=vList, DIALOG_PARENT=parentIn, TITLE=titleIn, $
						  VALUE=value
	
	COMMON DIALOG_GETVALUE, dgv_value, done_type
	
	IF NOT KEYWORD_SET(label) THEN label=''
	done_type=0
	
	
	title = KEYWORD_SET(titleIn) ? STRING(titleIn) : 'Enter or select a value'
	
	IF KEYWORD_SET(parentIn) THEN BEGIN
		parent = parentIn[0]
		IF NOT WIDGET_INFO(parent, /VALID_ID) $
		THEN res=DIALOG_MESSAGE('Invalid widget identifier.',/ERROR)
	ENDIF ELSE BEGIN
		parent = WIDGET_BASE(TITLE=title, MAP=0)   ; create a dummy parent base
	ENDELSE
  
  
	base = WIDGET_BASE(TITLE="Set spacecraft of the image", /COLUMN, GROUP_LEADER=parent, /MODAL, /FLOATING)
		sbase1 = WIDGET_BASE(base, /ROW)
			lbl = WIDGET_LABEL(sbase1, VALUE=label)
			cBox = WIDGET_COMBOBOX(sbase1, /EDITABLE, VALUE=vList, UNAME="COMBOBOX")
		sbase2 = WIDGET_BASE(base, /ALIGN_CENTER)
			bttns = CW_BGROUP(sbase2, ["   Ok   ", " Cancel "], UNAME="BUTTONS", /ROW, SPACE=10)
	WIDGET_CONTROL, base, /REALIZE
	
	WIDGET_CONTROL, cBox, SET_UVALUE=vList
	dgv_value = WIDGET_INFO(cBox, /COMBOBOX_GETTEXT)
	
	XMANAGER, 'dialog_getValue', base, GROUP_LEADER=parent
	
	
	value = dgv_value	
	
	RETURN, done_type
END
