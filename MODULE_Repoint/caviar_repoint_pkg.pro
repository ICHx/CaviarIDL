;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; List of procedures/functions of this file:
;	- PRO			caviar_repoint_display
;	- PRO			caviar_repoint_save
;	- FUNCTION		caviar_repoint_gui_init
;	- PRO       	caviar_repoint_gui_event
;	- PRO			caviar_repoint_gui
;-------------------------------------------------------------------------------

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_REPOINT_DISPLAY
; PURPOSE: Display the image and overlay information
;-------------------------------------------------------------------------------
PRO caviar_repoint_display
	
	COMMON CAVIAR_DATA, image, catstars, imgStars, planets, rings
	COMMON CAVIAR_PARAMS, dispParams, imgDraw, dispProName, starsCatalogs, find_imgStars_params
	
	nl = image.nl
	zoomFactor = imgDraw.ZFACTOR
	
	; Save current window and color table
	old_win = !D.WINDOW
	TVLCT, saved_colors, /GET
	
	; Display objects
	caviar_imgstars_display, imgStars, dispParams.imgStars, nl, imgDraw.OFFSET, ZOOMFACTOR=zoomFactor
	
	IF haveTag(image, 'CMAT') && TOTAL(image.CMAT) NE 0 THEN BEGIN
		caviar_catstars_display, catstars, dispParams.catstars, nl, imgDraw.OFFSET, ZOOMFACTOR=zoomFactor
		caviar_satellites_display, planets, dispParams.planets, nl, imgDraw.OFFSET, ZOOMFACTOR=zoomFactor
		caviar_rings_display, rings, dispParams.rings, nl, imgDraw.OFFSET, ZOOMFACTOR=zoomFactor
	ENDIF
	
	TVLCT, saved_colors
	WSET, old_win
END



;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_REPOINT_SAVE
; PURPOSE: Save astrometry results in *.QMPF and *.CSV files.
; INPUT:
;	format: Scalar string containing the saving file format ("QMPF").
;-------------------------------------------------------------------------------
PRO caviar_repoint_save, format
	
	COMMON CAVIAR_DATA, image, stars, imgStars, planets
	COMMON CAVIAR_REPOINT, wRSLTtxt, wSAVElbl
	
	; Save results in QMPF format:
	CASE format OF
		"QMPF": caviar_save2qmpf, FLAG=flag
	ENDCASE
	IF flag THEN BEGIN
		PRINT, "Results saved in ", format, " file."
		WIDGET_CONTROL, wSAVElbl, SET_VALUE=" Done!"
	ENDIF
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_REPOINT_GUI_INIT
; PURPOSE: Set default values/labels/parameters/etc. of the GUI.
;-------------------------------------------------------------------------------
PRO caviar_repoint_gui_init
	COMMON CAVIAR_REPOINT, wRSLTtxt, wSAVElbl
	
	WIDGET_CONTROL, wRSLTtxt, SET_VALUE=' '
	WIDGET_CONTROL, wSAVElbl, SET_VALUE=' '
END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_REPOINT_GUI_EVENT
; PURPOSE: Manage events of the widgets
;-------------------------------------------------------------------------------
PRO caviar_repoint_gui_event, event
	
	COMMON CAVIAR_DATA, image, stars, imgStars, planets
	COMMON CAVIAR_REPOINT, wRSLTtxt, wSAVElbl
	
	IF N_ELEMENTS(image) EQ 0 THEN BEGIN
		msg = ["% CALCSATPOS_GUI:", "Please load an image"]
		res = DIALOG_MESSAGE(msg, /CENTER)
		RETURN
	ENDIF
	
	CATCH, Error_status
	IF Error_status NE 0 THEN BEGIN
		res = DIALOG_MESSAGE(!ERROR_STATE.MSG, DIALOG_PARENT=event.HANDLER, /CENTER)
		RETURN	
	ENDIF
	
	; Get state from the first child of the compound widget root:
	WIDGET_CONTROL, event.HANDLER, GET_UVALUE=pstate
		
	uname = WIDGET_INFO(event.id, /UNAME)
	CASE uname OF
						
		; Start computing iterative pointing correction
		"REPOINT": BEGIN
			WIDGET_CONTROL, /HOURGLASS
			
			nsig = (*pstate).repntNsig & block = (*pstate).repntOpt[0]
			caviar_iteRepoint, NSIG=nsig, BLOCK_TWIST=block, RESIDUALS=res
			
			n=0
			FOREACH star, stars DO IF star.FITTED THEN n++
			WIDGET_CONTROL, wRSLTtxt, $
				SET_VALUE=["Stars fitted: "+STRING(n, '(I0)'), $
						   "Threshold: "+STRING(res.THRESHOLD, '(F-6.3)'), $
						   "RMS residuals (pixel)", $
						   "  Sample: "+STRING(res.SAMPLE, '(F-6.3)'), $
						   "  Line:   "+STRING(res.LINE, '(F-6.3)'), $
						   "  Total:  "+STRING(res.TOTAL, '(F-6.3)')]
			
			caviar_repoint_save, (*pstate).saveFormat
			caviar_display
		END
		
		"REPOINT OPTIONS": (*pstate).repntOpt = event.SELECT
		"REPOINT NSIG": (*pstate).repntNsig = event.VALUE
		"SAVE FORMAT": (*pstate).saveFormat = event.STR
		"SAVE": caviar_repoint_save, (*pstate).saveFormat
		
		ELSE:
	ENDCASE
	
END		
		

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:	CAVIAR_REPOINT_GUI
; PURPOSE: Create a graphical interface to repoint an image by using stars from
;		catalogues and sources found in it.
;		
; CALLING SEQUENCE:
;		caviar_repoint_gui [, PARENT=widgetID]
;				[, XOFFSET=xoffset] [, YOFFSET=yoffset] 
;
; INPUTS: None.
; OPTIONAL KEYWORDS:
;	PARENT: Set this keyword to the parent widget id in which you want to place 
;			this compound widget.
; OUTPUT: None.
;	
; COMMON BLOCKS:
;	CAVIAR_DATA, CAVIAR_PARAMS
;   
; PROCEDURE CALLS:
;	caviar_display				in caviar_gui.pro
;		
; MODIFICATIONS:
;	2012, March			MEUNIER L-E				OBSPM>IMCCE
;		- Written
;	2013, July			MEUNIER L-E				OBSPM>IMCCE
;		- Remove 'default params' columns and add 'Reset default params' button.
;		- Remove possibility to change the 'print' and 'silent' options that are
;		  now both set to 0.
;	2013, January		MEUNIER L-E				OBSPM>IMCCE
;		- Simplify management of find stars parameters and table widget values.
;		- Change routine names: 'fingStars' becomes 'imgstars'
;-------------------------------------------------------------------------------
PRO caviar_repoint_gui, PARENT=parent, XOFFSET=xoffset, YOFFSET=yoffset
	
	COMMON CAVIAR_REPOINT, wRSLTtxt, wSAVElbl
		
	; Test if widget window is already launched
	IF(XRegistered('caviar_repoint_gui') NE 0) THEN RETURN
	
	; Parameters
	saveFormatList = ['QMPF']
	saveFormatList_dfltID = 0
	repntOpt = 1
	repntNsig = 2.5
	save_qmpf = 1
	
	;***************************************************************************
	; Define the widget base and subwidgets tree
	IF KEYWORD_SET(parent) THEN mainBase=parent $
	ELSE mainBase=WIDGET_BASE(TITLE="Repoint image", /COLUMN, $
						XOFFSET=xoffset, YOFFSET=yoffset)
	
	extraBase = WIDGET_BASE(mainBase, /COLUMN, /ALIGN_CENTER, /BASE_ALIGN_CENTER, SPACE=10)
		
		wLCSbase = WIDGET_BASE(extraBase, /COLUMN, XPAD=0, YPAD=0)
		wFISbase = WIDGET_BASE(extraBase, /COLUMN, XPAD=0, YPAD=0)
		wMMSbase = WIDGET_BASE(extraBase, /COLUMN, XPAD=0, YPAD=0)
		
		wREPNTbase = WIDGET_BASE(extraBase, /COL, XPAD=7)
			wlbl = WIDGET_LABEL(wREPNTbase, VALUE='4. Iterative pointing correction')
			
			wREPNTebase = WIDGET_BASE(wREPNTbase, /COL, /FRAME, /ALIGN_CENTER)
				wREPNTsbase = WIDGET_BASE(wREPNTebase, /ROW, /ALIGN_CENTER)
					
					wREPNTsbase1 = WIDGET_BASE(wREPNTsbase, /COL);, XPAD=0, YPAD=0, SPACE=1)
						wRepointBttn = WIDGET_BUTTON(wREPNTsbase1, UNAME="REPOINT", $
							YSIZE=28, VALUE="Repoint")
						wRepOptBGroup = CW_BGROUP(wREPNTsbase1, UNAME="REPOINT OPTIONS", $
							"Block orientation", SET_VALUE=repntOpt, /NONEXCLUSIVE, $
							SPACE=0, XPAD=0, YPAD=0)
						wRepNSIGfield = CW_FIELD(wREPNTsbase1, /ALL_EVENTS, /FLOATING, $
									TITLE="n-Sigma =", VALUE=repntNsig, $
									UNAME="REPOINT NSIG", XSIZE=4)
					
					wREPNTsbase2 = WIDGET_BASE(wREPNTsbase, /COL, XPAD=0, YPAD=0)
						wRSLTtxt = WIDGET_TEXT(wREPNTsbase2, VALUE="       Results", $
							XSIZE=22, YSIZE=6, EDITABLE=0)
				
				wSaveBase = WIDGET_BASE(wREPNTebase, /ROW, /BASE_ALIGN_CENTER)
					wSaveBttn = WIDGET_BUTTON(wSaveBase, UNAME="SAVE", VALUE="Save", XSIZE=60, YSIZE=28)
					wSaveFormatLbl = WIDGET_LABEL(wSaveBase, VALUE="Format:")
					wSaveFormatCbox = WIDGET_COMBOBOX(wSaveBase, UNAME="SAVE FORMAT", $
										VALUE=saveFormatList, XSIZE=60)
				wSAVElbl = WIDGET_LABEL(wSaveBase, VALUE=" ", XSIZE=50)
				
						
	; Create the widget (i.e. display it)
	WIDGET_CONTROL, mainBase, /REALIZE, /HOURGLASS, UPDATE=0
	
	caviar_catstars_gui, PARENT=wLCSbase, XPAD=5, YPAD=3, SPACE=0, /FRAME
	caviar_imgstars_gui, PARENT=wFISbase, XPAD=0, YPAD=0, SPACE=0, /FRAME
	caviar_matchStars_gui, PARENT=wMMSbase, XPAD=6, YPAD=0, SPACE=0, /FRAME
		
	; Set complementary widgets parameters:
	WIDGET_CONTROL, wSaveFormatCbox, SET_COMBOBOX_SELECT=saveFormatList_dfltID
	
	
	; Copy 'state' structure in extra widget uvalue
	state = {saveFormat:saveFormatList[saveFormatList_dfltID], $
			 repntOpt:repntOpt, repntNsig:repntNsig $
			}
	WIDGET_CONTROL, extraBase, SET_UVALUE=PTR_NEW(state, /NO_COPY)
	
	WIDGET_CONTROL, mainBase, /UPDATE
	; Let XMANAGER takes control of widgets event processing
	XMANAGER, 'caviar_repoint_gui', extraBase, /JUST_REG

END
