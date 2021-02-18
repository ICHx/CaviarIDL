@haveTag
@caviar_image_load_pkg
@caviar_data_routinesWrapper

;###############################################################################
;#	                     Manage events of the widget 						
;###############################################################################
PRO caviar_image_load_gui_event, event
	
	COMMON CAVIAR_DATA, image, stars, imgStars, planets
	COMMON CAVIAR_PARAMS, dispParams, imgDraw, dispProName, starsCatalogs, find_imgStars_params
	COMMON CAVIAR_LOADDATA, imgType, imgFile, lblType, lblFile, pntgFile
	
	
	; Get state from the first child of the compound widget root:
	child = WIDGET_INFO(event.HANDLER, /CHILD)
	WIDGET_CONTROL, child, GET_UVALUE=pstate
	WIDGET_CONTROL, /HOURGLASS

	CATCH, error
	IF error THEN BEGIN
		msg = ["Unexpected error in 'caviar_image_load_gui_event':", !ERROR_STATE.MSG]
		MESSAGE, /CONT, !ERROR_STATE.MSG_PREFIX+!ERROR_STATE.MSG
		res = DIALOG_MESSAGE(msg, DIALOG_PARENT=event.handler, /CENTER)
		caviar_data_restore
		RETURN
	ENDIF
	
	ret = 0

	uname = WIDGET_INFO(event.id, /UNAME)
	CASE uname OF
		;***********************************************************************
		"IMAGE FORMAT": BEGIN
			WIDGET_CONTROL, event.id, GET_UVALUE=imgTypeList
			imgType = imgTypeList[event.index]
			
			; Set the labels type droplist applicable values from the type list
			; and define the pre-selected item:
			WIDGET_CONTROL, (*pstate).wLblTypeDLst, GET_UVALUE=lblTypeList
			n = N_ELEMENTS(lblTypeList)
			CASE event.index OF
				0: dlStruct = {index: INDGEN(n), default: 0}
				1: dlStruct = {index: INDGEN(n), default: 0}
				2: dlStruct = {index: 1, default: 0}
			ENDCASE
			
			lblType = lblTypeList[dlStruct.index[dlStruct.default]]
			WIDGET_CONTROL, (*pstate).wLblTypeDLst, SET_VALUE=lblTypeList[dlStruct.index]
			WIDGET_CONTROL, (*pstate).wLblTypeDLst, SET_DROPLIST_SELECT=dlStruct.default
			WIDGET_CONTROL, (*pstate).wLblFileFieldBase, SENSITIVE=(lblType EQ 'Internal') ? 0 : 1
		END
			
		
		"SELECT IMAGE FILE": BEGIN
                        vicfilt = [['*.IMG;*.img'], ['CASSINI']]
                        filters = [vicfilt, [['*'],['All files']]]
			CASE imgType OF
                                "CASSINI"      : filters = [vicfilt, [['*'],['All files']]]
				ELSE: filters=['*','All files']
			ENDCASE
			path = GETENV("CAVIAR_IMAGES_DIRECTORY")
			IF NOT FILE_TEST(path, /DIRECTORY) $
			THEN path = ISA(image, 'STRUCT') ? image.path : '.'
			imgFile = DIALOG_PICKFILE(PATH=path, FILTER=filters, $
				TITLE="Select an image:", /MUST_EXIST, DIALOG_PARENT=event.handler)

			WIDGET_CONTROL, (*pstate).wImgFileText, SET_VALUE=imgFile
			
			file = STRMID(imgFile, 0, STRPOS(imgFile, '.'))
			files = [file+'.lbl', file+'.LBL']
			index = WHERE( FILE_TEST(files) )
			IF index[0] NE -1 THEN BEGIN
				lblFile = files[index[0]]
				WIDGET_CONTROL, (*pstate).wLblFileText, SET_VALUE=lblFile
			ENDIF
			
		END
		
		"EDIT IMAGE FILE": BEGIN
			WIDGET_CONTROL, event.id, GET_VALUE=imgFile
			
			file = STRMID(imgFile, 0, STRPOS(imgFile, '.'))
			files = [file+'.lbl', file+'.LBL']
			index = WHERE( FILE_TEST(files) )
			IF index NE -1 THEN BEGIN
				lblFile = files[index]
				WIDGET_CONTROL, (*pstate).wLblFileText, SET_VALUE=lblFile
			ENDIF
		END
		
				
		;***********************************************************************
		"LABEL FORMAT": BEGIN
			WIDGET_CONTROL, event.id, GET_VALUE=lblTypeList
			lblType = lblTypeList[event.index]
			WIDGET_CONTROL, (*pstate).wLblFileFieldBase, SENSITIVE=(lblType EQ 'Internal') ? 0 : 1
		END
		
		"SELECT LABEL FILE": BEGIN
			IF N_ELEMENTS(imgFile) NE 0 $
			THEN path=FILE_DIRNAME(imgFile) $
			ELSE BEGIN
				path = GETENV("DATA_DIR")
				IF NOT FILE_TEST(path, /DIRECTORY) THEN path='.'
			ENDELSE			
			title = "Select the label file:"
			filters = [['*.LBL;*.lbl', '*'],['LBL', 'All files']]
			lblFile = DIALOG_PICKFILE(PATH=path, TITLE=title, $
				FILTER=filters, /MUST_EXIST, DIALOG_PARENT=event.handler)
			
			WIDGET_CONTROL, (*pstate).wLblFileText, SET_VALUE=lblFile
		END
		
		"EDIT LABEL FILE": WIDGET_CONTROL, event.id, GET_VALUE=lblFile
				
		;***********************************************************************
		"SELECT POINTING FILE": BEGIN
			IF N_ELEMENTS(imgFile) NE 0 $
			THEN path = FILE_DIRNAME(imgFile) $
			ELSE BEGIN
				path = GETENV("DATA_DIR")
				IF NOT FILE_TEST(path, /DIRECTORY) THEN path='.'
			ENDELSE	
			title = "Select the pointing file:"
			filters = [['*.qmpf;*.QMPF','*'], ['QMPF','All files']]
			pntgFile = DIALOG_PICKFILE(PATH=path, TITLE=title, $
				FILTER=filters, /MUST_EXIST, DIALOG_PARENT=event.handler)
			
			WIDGET_CONTROL, (*pstate).wPntgFileText, SET_VALUE=pntgFile
		END
		
		"EDIT POINTING FILE": WIDGET_CONTROL, event.id, GET_VALUE=pntgFile
				
		;***********************************************************************
		"LOAD IMAGE": BEGIN
			IF imgType EQ '' THEN MESSAGE, "Please select an image format!"
			IF lblType EQ '' THEN MESSAGE, "Please select a label format!"

                        IF ISA(imgFile, 'String') EQ 0 || FILE_TEST(imgFile, /REGULAR) EQ 0 THEN BEGIN
                            MESSAGE, "Please select an image."
                        ENDIF

			IF lblType NE 'Internal' $
			&& lblFile EQ '' THEN MESSAGE, "Please select a label file!"
			
			; Inform the user that previous data exist and will be erased if continuing:
			IF ISA(image) THEN BEGIN
				msg = ["Loading a new image will erase any unsaved Caviar data.", "Do you want to continue?"]
				res = DIALOG_MESSAGE(msg, /QUESTION, /CENTER, DIALOG_PARENT=event.HANDLER)
				IF res EQ 'No' THEN RETURN
			ENDIF

                        caviar_data_reset, /ALL
                        cspice_kclear

			IF STRUPCASE(lblType) EQ 'INTERNAL' $
			THEN image = caviar_image_load(imgFile, imgType, POINTING_FILE=pntgFile, /GUI) $
			ELSE image = caviar_image_load(imgFile, imgType, POINTING_FILE=pntgFile, /GUI, LABELS_FILE=lblFile) 

			; If no image has been loaded then return without closing the gui:
			IF NOT ISA(image, 'STRUCT') THEN RETURN

			PTR_FREE, pstate
			WIDGET_CONTROL, event.TOP, /DESTROY
			RETURN

		END

		ELSE:
	ENDCASE

	
	IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' $
	|| uname EQ "CLOSE" THEN BEGIN
		PTR_FREE, pstate
		WIDGET_CONTROL, event.TOP, /DESTROY
		RETURN
	ENDIF
END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:     caviar_image_load_gui
; PURPOSE:  Image loading dialog for asking parameters 
; CATEGORY: widget
; CALLING SEQUENCE: caviar_loadImage_gui [, PARENT=base]
; INPUTS: All input parameters are passed as keywords or in common block
; KEYWORDS:
;	PARENT=parentIn: Set this keyword to the parent widget id in which you want  
;			to place this compound widget.
; OUTPUTS: No explicit outputs. A new window is created. 
; COMMON BLOCKS:
;	CAVIAR_LOADDATA
; MODIFICATION HISTORY:
;	2013, June		MEUNIER L-E.		OBSPM>IMCCE
;		- Written
;-------------------------------------------------------------------------------
PRO caviar_image_load_gui, TITLE=title, GROUP_LEADER=groupLeader, $
							XOFFSET=xoffset, YOFFSET=yoffset
	
	COMMON CAVIAR_LOADDATA, imgType, imgFile, lblType, lblFile, pntgFile
	
	; Parameters initialisation
        imgTypeList = ['CASSINI']
	imgType = imgTypeList[0]
	lblTypeList = ['Internal', 'External'];: CASSINI', 'External: ISIS cub']						
	lblType = lblTypeList[0]
	
	; Icons
	ico_path = GETENV('ICONS_PATH')
	ico_pickfile = ico_path+'pickfile.bmp'
	
	;***************************************************************************	
	; Define the widget base and subwidgets tree
	;***************************************************************************
	IF NOT KEYWORD_SET(title) THEN title = "Load new image"
	
	IF KEYWORD_SET(groupLeader) THEN BEGIN
		IF WIDGET_INFO(groupLeader, /VALID_ID) EQ 0 $
		THEN res=DIALOG_MESSAGE('Invalid group leader widget identifier.',/ERROR) $
		ELSE wMAINbase = WIDGET_BASE(TITLE=title, /TLB_KILL_REQUEST_EVENTS, $
									XOFFSET=xoffset, YOFFSET=yoffset, $
									GROUP_LEADER=groupLeader, /FLOATING, /MODAL)
	ENDIF ELSE wMAINbase = WIDGET_BASE(TITLE=title, /TLB_KILL_REQUEST_EVENTS, $
									XOFFSET=xoffset, YOFFSET=yoffset)
								
	wExtraBase = WIDGET_BASE(wMAINbase, /COLUMN, SPACE=5)

	base1 = WIDGET_BASE(wExtraBase, /COLUMN, /ALIGN_LEFT, /BASE_ALIGN_LEFT, /FRAME)
		base11 = WIDGET_BASE(base1, /ROW, /BASE_ALIGN_CENTER)
			wImgFileLbl  = WIDGET_LABEL(base11, VALUE="Image:")
			wImgTypeDLst = WIDGET_DROPLIST(base11, UNAME="IMAGE FORMAT", $
				VALUE=imgTypeList, UVALUE=imgTypeList, XSIZE=240)
;			wImgTypeDLst = WIDGET_DROPLIST(base11, UNAME="IMAGE FORMAT", $
;				VALUE=imgTypeList, UVALUE=imgTypeList, /FLAT, XSIZE=240)
			
		base12 = WIDGET_BASE(base1, /ROW)
			wImgFileBttn = WIDGET_BUTTON(base12, UNAME="SELECT IMAGE FILE", $
				VALUE="Select file", XSIZE=90, YSIZE=28)
			wImgFileText = WIDGET_TEXT(base12, UNAME="EDIT IMAGE FILE", /EDITABLE, XSIZE=100)
	
	
	base2 = WIDGET_BASE(wExtraBase, /COLUMN, /ALIGN_LEFT, /BASE_ALIGN_LEFT, /FRAME)
		base21 = WIDGET_BASE(base2, /ROW, /BASE_ALIGN_CENTER)
			wLblFileLbl  = WIDGET_LABEL(base21, VALUE="Label:")
			wLblTypeDLst = WIDGET_DROPLIST(base21, UNAME="LABEL FORMAT", $
				 VALUE=lblTypeList, UVALUE=lblTypeList, XSIZE=240)
			
		base22 = WIDGET_BASE(base2, /ROW, SENSITIVE=0)
			wLblFileBttn = WIDGET_BUTTON(base22, UNAME="SELECT LABEL FILE", $
				VALUE="Select file", XSIZE=90, YSIZE=28)
			wLblFileText = WIDGET_TEXT(base22, UNAME="EDIT LABEL FILE", /EDITABLE, XSIZE=100)
	

	base3 = WIDGET_BASE(wExtraBase, /COLUMN, /ALIGN_LEFT, /BASE_ALIGN_LEFT, /FRAME)
		base31 = WIDGET_BASE(base3, /ROW, /BASE_ALIGN_CENTER)
			wPtgFileLbl = WIDGET_LABEL(base31, VALUE="Pointing file (optional):", $
				/ALIGN_LEFT, XSIZE=283)
			
		base32 = WIDGET_BASE(base3, /ROW)
			wPntgFileBttn = WIDGET_BUTTON(base32, UNAME="SELECT POINTING FILE", $
				VALUE="Select file", XSIZE=90, YSIZE=28)
			wPntgFileText = WIDGET_TEXT(base32, UNAME="EDIT POINTING FILE", /EDITABLE, XSIZE=100)
	

	base4 = WIDGET_BASE(wExtraBase, /ROW, /ALIGN_CENTER, /BASE_ALIGN_CENTER, SPACE=20)
		wLoadImgBttn = WIDGET_BUTTON(base4, VALUE="LOAD", UNAME="LOAD IMAGE", XSIZE=80, YSIZE=35)

	closeBttn = WIDGET_BUTTON(base4, VALUE="CLOSE", UNAME="CLOSE", XSIZE=80, YSIZE=35)
		
	; Create the widget (i.e. display it)
	WIDGET_CONTROL, wMAINbase, /REALIZE
	
	WIDGET_CONTROL, wImgTypeDLst, SET_DROPLIST_SELECT=0
	WIDGET_CONTROL, wLblTypeDLst, SET_DROPLIST_SELECT=0
	
	state = {wLblFileFieldBase: base22, $
			 wLblTypeDLst: wLblTypeDLst, $
			 wImgFileText: wImgFileText, $
			 wLblFileText: wLblFileText, $
			 wPntgFileText: wPntgFileText}
	WIDGET_CONTROL, wExtraBase, SET_UVALUE=PTR_NEW(state, /NO_COPY)
	
	; Let XMANAGER take control of widget event processing
	XMANAGER, 'caviar_image_load_gui', wMainBase, GROUP_LEADER=groupLeader
END
