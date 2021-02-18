@caviar_satModel_pkg.pro
@caviar_satLimbFit_ite.pro
@caviar_satLimbFit_lsq.pro
@caviar_xmove.pro
@caviar_edgrad_pkg.pro
@caviar_edcanny_pkg.pro

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;	List of procedure of this file:
;	- PRO		caviar_satlimbfit_gui_init
;	- PRO 		caviar_satlimbfit_gui_event
;	- PRO 		caviar_satlimbfit_gui
;-------------------------------------------------------------------------------

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_SATLIMBFIT_GUI_INIT
; PURPOSE: Set default values/labels/parameters/etc. of the GUI.
;-------------------------------------------------------------------------------
PRO caviar_satlimbfit_gui_init
	
	COMMON CAVIAR_SATLIMBFIT, satModel, satLimb
	
	; Delete image filtering window:
	DEVICE, WINDOW_STATE=winState
	IF winState[31] NE 0 THEN WDELETE, 31
	
	; Initialize data:
	satModel = {}
	satLimb = {}
END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;	                  Manage events of the widget 		     				   
;-------------------------------------------------------------------------------
PRO caviar_satlimbfit_gui_event, event
	
	COMMON CAVIAR_DATA, image, stars, imgStars, planets
	COMMON CAVIAR_SATPOS, selSatIndex, wSatTblID, wSaveLbl, wSLFbaseYsize
	COMMON CAVIAR_SATLIMBFIT, satModel, satLimb
	
	CATCH, error_status
	IF error_status NE 0 THEN BEGIN
		res = DIALOG_MESSAGE(!ERROR_STATE.MSG, DIALOG_PARENT=event.HANDLER, /CENTER)
		RETURN	
	ENDIF
	
	; Get state from the first child of the compound widget root:
	child = WIDGET_INFO(event.HANDLER, /CHILD)
	WIDGET_CONTROL, child, GET_UVALUE=pstate
	
	WIDGET_CONTROL, /HOURGLASS

	uname = WIDGET_INFO(event.id, /UNAME)
	CASE uname OF
						
		;***********************************************************************
		; Satellite shape model (limb, terminator, equator):
		;***********************************************************************
		"SHAPE MODEL SELECT": BEGIN
			IF selSatIndex EQ !NULL THEN BREAK
			WIDGET_CONTROL, /HOURGLASS
			modelindex = WIDGET_INFO((*pstate).wSMdlist, /DROPLIST_SELECT)
			
			; Change the "visible" parameters base to follow the droplist selection:
			FOR i=0,N_ELEMENTS((*pstate).wSMPARAMSbases)-1 $
			DO WIDGET_CONTROL, (*pstate).wSMPARAMSbases[i], MAP = i EQ modelindex ? 1 : 0
			
			; Resize parameters base & change the button title:
			(*pstate).wSMPbaseYsize= ([1,35,110])[modelindex+1]
			wSLFbaseYsize = (*pstate).wSMPbaseYsize + (*pstate).wLDMbaseYsize + (*pstate).wSLFcplmtYsize
			WIDGET_CONTROL, (*pstate).wSMPbase, SCR_YSIZE=(*pstate).wSMPbaseYsize
			WIDGET_CONTROL, event.HANDLER, SCR_YSIZE=wSLFbaseYsize
			WIDGET_CONTROL, (*pstate).wSMPARAMSbttn, SET_VALUE='-', SET_UVALUE='+'
			
			; Load shape model:
			caviar_satModel_load, modelindex
			
			caviar_display
		END
		
		"SHAPE MODEL LOAD": BEGIN
			IF selSatIndex EQ !NULL THEN BREAK
			
			WIDGET_CONTROL, /HOURGLASS
			
			modelindex = WIDGET_INFO((*pstate).wSMdlist, /DROPLIST_SELECT)
			caviar_satModel_load, modelindex
			
			caviar_display
		END
		
		"SHAPE MODEL MOVE": BEGIN
			IF NOT haveTag(satModel, 'SLLIMB') THEN BEGIN
				msg = "Please load a shape model first!"
				res = DIALOG_MESSAGE(msg, /CENTER, DIALOG_PARENT=event.ID)
				RETURN
			ENDIF
			
			; Change parameters button value:
			WIDGET_CONTROL, (*pstate).wSMPARAMSbttn, GET_VALUE=curVal
			WIDGET_CONTROL, (*pstate).wSMPARAMSbttn, SET_VALUE='-', SET_UVALUE='+'
						
			; Resize parameters base:
			(*pstate).wSMPbaseYsize = 120
			WIDGET_CONTROL, (*pstate).wSMPbase, SCR_YSIZE=(*pstate).wSMPbaseYsize
			wSLFbaseYsize = (*pstate).wSMPbaseYsize + (*pstate).wLDMbaseYsize + (*pstate).wSLFcplmtYsize
			WIDGET_CONTROL, event.HANDLER, SCR_YSIZE=wSLFbaseYsize
			
			print, (*pstate).wSMPARAMSBases[2]
			
			FOR i=0,N_ELEMENTS((*pstate).wSMPARAMSBases)-1 $
			DO WIDGET_CONTROL, (*pstate).wSMPARAMSBases[i], MAP = i EQ 2 ? 1 : 0
		END
		
;		"MOVE": BEGIN
;			CASE event.type OF
;				1:  caviar_satModel_move, TRANSLATE = event.value
;				2:  caviar_satModel_move, TRANSLATE = event.value
;				3:  caviar_satModel_move, TRANSLATE = event.value
;				4:  caviar_satModel_move, TRANSLATE = event.value
;				5:  BEGIN
;						WIDGET_CONTROL, /HOURGLASS
;						;caviar_matchStars_mouseMove, TFACTOR=event.value.tFactor, $
;						;	  						 RFACTOR=event.value.rFactor
;					END	
;				6:  caviar_satModel_move, TRANSLATE = event.value
;				7:  caviar_satModel_move, TRANSLATE = event.value
;				8:  caviar_satModel_move, TRANSLATE = event.value
;				9:  caviar_satModel_move, TRANSLATE = event.value
;				11: caviar_satModel_move, ROTATE = event.value*!DTOR
;				12: caviar_satModel_move, ROTATE = event.value*!DTOR
;				ELSE:
;			ENDCASE
;		END
		
		"SHAPE MODEL PARAMS": BEGIN
			; Change the button title from "+" to "-" or vice cersa:
			WIDGET_CONTROL, event.ID, GET_VALUE=curVal, GET_UVALUE=newVal
			WIDGET_CONTROL, event.ID, SET_VALUE=newVal, SET_UVALUE=curVal
				
			; Resize parameters base:
			visible = curVal EQ '+'
			index = WIDGET_INFO((*pstate).wSMdlist, /DROPLIST_SELECT)
			(*pstate).wSMPbaseYsize = ([1,35,140])[visible*(index+1)]
			WIDGET_CONTROL, (*pstate).wSMPbase, SCR_YSIZE=(*pstate).wSMPbaseYsize
			wSLFbaseYsize = (*pstate).wSMPbaseYsize + (*pstate).wLDMbaseYsize + (*pstate).wSLFcplmtYsize
						WIDGET_CONTROL, event.HANDLER, SCR_YSIZE=wSLFbaseYsize
						
			FOR i=0,N_ELEMENTS((*pstate).wSMPARAMSBases)-1 $
			DO WIDGET_CONTROL, (*pstate).wSMPARAMSBases[i], MAP = i EQ index ? 1 : 0
		END
		
		; Parameters:
		"SM ELLIPSOID NPOINTS": BEGIN
			oldVal = (*pstate).satModelNpts
			(*pstate).satModelNpts = DOUBLE(event.str)
			IF ARRAY_EQUAL(DOUBLE(event.str), oldVal) EQ 0 && (*pstate).smAutoUpdt $
			THEN caviar_satModel_load, 0
			caviar_display
		END
		
		"SM AUTO UPDATE": (*pstate).smAutoUpdt = event.select
		
		;***********************************************************************
		; Satellite Limb Detection:
		;***********************************************************************
		"LIMB DETECTION METHOD": BEGIN
			IF selSatIndex EQ !NULL THEN BREAK
			WIDGET_CONTROL, /HOURGLASS
			methodindex = WIDGET_INFO((*pstate).wLDdlist, /DROPLIST_SELECT)
			
			; Change the mapped base for parameters to follow the droplist selection:
			wLDMsbases = (*pstate).wLDMsbases
			FOR i=0, N_ELEMENTS(wLDMsbases)-1 $
			DO WIDGET_CONTROL, wLDMsbases[i], MAP = i EQ methodindex ? 1 : 0
			
			WIDGET_CONTROL, wLDMsbases[methodindex], GET_UVALUE=ysize
			WIDGET_CONTROL, WIDGET_INFO(wLDMsbases[methodindex], /PARENT), SCR_YSIZE=ysize
			
			; Resize parameters base & change the button title:
			(*pstate).wLDMbaseYsize = ([35,430,330])[methodindex+1]
			wSLFbaseYsize = (*pstate).wSMPbaseYsize + (*pstate).wLDMbaseYsize + (*pstate).wSLFcplmtYsize
			WIDGET_CONTROL, (*pstate).wLDMbase, SCR_YSIZE=(*pstate).wLDMbaseYsize
			WIDGET_CONTROL, event.HANDLER, SCR_YSIZE=wSLFbaseYsize
			WIDGET_CONTROL, (*pstate).wLDMbttn, SET_VALUE='-', SET_UVALUE='+'
		END
		
		"LIMB DETECTION MORE BUTTON": BEGIN
			; Change the button title from "+" to "-" or vice cersa:
			WIDGET_CONTROL, event.ID, GET_VALUE=curVal, GET_UVALUE=newVal
			WIDGET_CONTROL, event.ID, SET_VALUE=newVal, SET_UVALUE=curVal
				
			; Resize parameters base:
			visible = curVal EQ '+'
			methodindex = WIDGET_INFO((*pstate).wLDdlist, /DROPLIST_SELECT)
			(*pstate).wLDMbaseYsize = ([35,430,330])[visible*(methodindex+1)]
			WIDGET_CONTROL, (*pstate).wLDMbase, SCR_YSIZE=(*pstate).wLDMbaseYsize
			wSLFbaseYsize = (*pstate).wSMPbaseYsize + (*pstate).wLDMbaseYsize + (*pstate).wSLFcplmtYsize
			WIDGET_CONTROL, event.HANDLER, SCR_YSIZE=wSLFbaseYsize
						
			FOR i=0,N_ELEMENTS((*pstate).wLDMsbases)-1 $
			DO WIDGET_CONTROL, (*pstate).wLDMsbases[i], MAP = i EQ methodindex ? 1 : 0
		
		END
		
		;***********************************************************************
		; Fit limb from the image with limb from the model:
		;***********************************************************************
		"LIMB FIT": BEGIN
			IF selSatIndex EQ !NULL THEN BREAK
			WIDGET_CONTROL, wSaveLbl, SET_VALUE=""
			WIDGET_CONTROL, /HOURGLASS
			IF N_ELEMENTS(satModel) EQ 0 || N_ELEMENTS(satLimb) EQ 0 THEN BREAK
						
			WIDGET_CONTROL, event.ID, GET_UVALUE=wLFdlist
			methodindex = WIDGET_INFO(wLFdlist, /DROPLIST_SELECT)
			
			CASE methodindex OF
				0: caviar_satLimbFit_ite, satModel, satLimb, $
									TOLERANCE=(*pstate).fitThreshold, sres, lres
				1: caviar_satLimbFit_lsq, planets[selSatIndex], satModel, satLimb, $
									sres, lres
			ENDCASE
			dres = SQRT(sres*sres+lres*lres)
			
			setStruct, satModel, 'SLSIGMA', [[sres],[lres]]
			caviar_satPos_setResults, "LIMB-FIT", satModel.slCenter, dres
			caviar_satPos_gui_table, /UPDATE_VALUES
			caviar_display
		END
		
		"LIMB FIT THRESHOLD": (*pstate).fitThreshold = DOUBLE(event.str)
				
		ELSE:
	ENDCASE	
		
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_SATLIMBFIT_GUI
; PURPOSE: Display a graphical interface with:
;	- The list of satellites visible in the image and their data,
;	- A window to display the satellite image,
;	- Buttons to launch satellite center of figure mesurement routines and to 
;	save the results.
;		
; CATEGORY:
;       Widgets
;
; INPUTS:
;       None.
; KEYWORDS:
;		None.
; OUTPUTS:
;       None.
; COMMON BLOCKS:
;       
; SIDE EFFECTS:
;       A widget window is created.
; RESTRICTIONS:
;       
; MODIFICATION HISTORY:
;		2012, december			MEUNIER L-E		OBSPM>IMCCE
;			- Written
;		2013, april				MEUNIER L-E		OBSPM>IMCCE
;			- Change UI
;-------------------------------------------------------------------------------
PRO caviar_satlimbfit_gui, PARENT=wMAINbase, GROUP_LEADER=groupLeader, $
							XOFFSET=xoffset, YOFFSET=yoffset, TITLE=title, $
							FRAME=frame, XPAD=xpad, YPAD=ypad, SPACE=space
	
	COMMON CAVIAR_GUI, wCAVIARtlb, wSATLIMBFITbase
	COMMON CAVIAR_DATA, image
	COMMON CAVIAR_SATPOS, selSatIndex, wSatTblID, wSaveLbl, wSLFbaseYsize
	COMMON CAVIAR_SATLIMBFIT, satModel, satLimb
						

	; Test if widget window is already launched
	IF(XRegistered('caviar_satLimbFit_gui') NE 0) THEN RETURN

	IF N_ELEMENTS(image) EQ 0 THEN BEGIN
		msg = ["CAVIAR_SATLIMBFIT_GUI:", "Please load an image first!"]
		res = DIALOG_MESSAGE(msg, /CENTER)
		RETURN
	ENDIF
	
		
	;***************************************************************************
	; Set Parameters 
	;***************************************************************************
	SMlist = ["Spice Ellipsoid"]
	nptsList = ['200', '500', '1000', '2000', '5000', '10000']
	nptsList_dfltID = 2
	satModelNpts = LONG(nptsList[nptsList_dfltID])
	
	; For Shape model:
	smcp = { VALUES: [3,1.0,0.4,0.6], FORMAT: ['(I-3)','(G0)','(G0)','(G0)'], $
			 MIN: [1,0.01,0.01,0.1], MAX: [30,1,1,3], STEP: [2,0.01,0.01,0.1], $
			 TEXT: ["Median Width  : ","High threshold: ","Low threshold : ","Convol. sigma : "]}
	smcpVals = smcp.VALUES
	smAutoUpdt = 1
	
	; For Limb detection:
	ldmethods = ["Gradient", "Canny"]
	
	; For Limb fitting:
	LFlist = ["Iterative offset","Least squares"]
	fitThresholdList = ['0.5','1.0','2.0','4.0','6.0','8.0','10.0']
	fitThresholdList_dfltID = 2
	
	wSMPbaseYsize = 1
	wLDMbaseYsize = 35
	wSLFcplmtYsize = 225

	;*************************************************************************** 
	; Define the widget base and subwidgets tree 
	;***************************************************************************
	IF NOT KEYWORD_SET(title) THEN title = "Fit satellite limb"
	
	
	IF KEYWORD_SET(wMAINbase) THEN BEGIN
		IF NOT WIDGET_INFO(wMAINbase, /VALID_ID) $
		THEN res=DIALOG_MESSAGE('Invalid widget identifier.',/ERROR)
	ENDIF ELSE BEGIN
		IF NOT KEYWORD_SET(groupLeader) $
		THEN wMAINbase = WIDGET_BASE(TITLE=title, XOFFSET=xoffset, YOFFSET=yoffset, $
										UNAME="CAVIAR_SATLIMBFIT_GUI") $
		ELSE BEGIN
			IF WIDGET_INFO(groupLeader, /VALID_ID) $
			THEN wMAINbase = WIDGET_BASE(TITLE=title, XOFFSET=xoffset, YOFFSET=yoffset, $
										UNAME="CAVIAR_SATLIMBFIT_GUI", $
										GROUP_LEADER=groupLeader, /FLOATING) $
			ELSE res=DIALOG_MESSAGE('Invalid group leader widget identifier.',/ERROR)
		ENDELSE
	ENDELSE
	wSATLIMBFITbase = wMAINbase
	
	;***************************************************************************
	; Define subwidgets tree:
	;***************************************************************************
	extra = WIDGET_BASE(wMAINbase, /COLUMN, XPAD=xpad, YPAD=ypad)
	
	wbase = WIDGET_BASE(extra, /COLUMN, FRAME=frame, XPAD=0, YPAD=0, SPACE=space, $
									/BASE_ALIGN_CENTER, /ALIGN_CENTER)
		
		wLDbase  = WIDGET_BASE(wbase, /COL, XPAD=0, YPAD=0, SPACE=0)
			wLDsbase1 = WIDGET_BASE(wLDbase, /ROW, /BASE_ALIGN_CENTER, XPAD=0, YPAD=0, SPACE=0)
				wLDdlist = WIDGET_DROPLIST(wLDsbase1, UNAME="LIMB DETECTION METHOD", $
									VALUE=ldmethods, TITLE="Limb detection method:")
				wSpace = WIDGET_BASE(wLDsbase1, XSIZE=32)
				wLDMbttn = WIDGET_BUTTON(wLDsbase1, UNAME="LIMB DETECTION MORE BUTTON", $
								VALUE="+", UVALUE="-", XSIZE=29, YSIZE=29)
				WIDGET_CONTROL, wLDdlist, SET_UVALUE=wLDMbttn
			wLDsbase2 = WIDGET_BASE(wLDbase)
			
		wSpace = WIDGET_BASE(wbase, XSIZE=300, YSIZE=1, /FRAME)
			
		wSMbase  = WIDGET_BASE(wbase, /COL, XPAD=0, YPAD=0, SPACE=0)
			wSMsbase1 = WIDGET_BASE(wSMbase, /ROW, /BASE_ALIGN_CENTER, XPAD=0, YPAD=0, SPACE=0)
				wSMdlist = WIDGET_DROPLIST(wSMsbase1, UNAME="SHAPE MODEL SELECT", $
									VALUE=SMlist, TITLE="Shape model:")
				wSpace = WIDGET_BASE(wSMsbase1, XSIZE=16)
				wSMPARAMSbttn = WIDGET_BUTTON(wSMsbase1, UNAME="SHAPE MODEL PARAMS", $
								VALUE="+", UVALUE="-", XSIZE=29, YSIZE=29)
				WIDGET_CONTROL, wSMdlist, SET_UVALUE=wSMPARAMSbttn
			
			wSMsbase2 = WIDGET_BASE(wSMbase, /ROW, /BASE_ALIGN_CENTER, /ALIGN_CENTER)
				wSMUPDTBttn = CW_BGROUP(wSMsbase2, "Auto-update", $
									UNAME='SM AUTO UPDATE', SET_VALUE=smAutoUpdt, /NONEXCLUSIVE)
				wSpace = WIDGET_BASE(wSMsbase2, XSIZE=77)
				wSMLOADbttn = WIDGET_BUTTON(wSMsbase2, UNAME="SHAPE MODEL LOAD", $
									VALUE="Load", XSIZE=47, YSIZE=29)
				wSMMOVEbttn = WIDGET_BUTTON(wSMsbase2, UNAME="SHAPE MODEL MOVE", $
									VALUE="Move", XSIZE=47, YSIZE=29)
				wSpace = WIDGET_BASE(wSMsbase2, XSIZE=1)
				
			wSMPbase = WIDGET_BASE(wSMbase, SCR_YSIZE=wSMPbaseYsize)
		
		wSpace = WIDGET_BASE(wbase, XSIZE=300, YSIZE=1, /FRAME)
		
		wLFbase = WIDGET_BASE(wbase, /COL, SPACE=0, /BASE_ALIGN_LEFT, XSIZE=300)
	
	; Define SHAPE MODEL PARAMETERS widgets:
	wSMPARAMSbases = LONARR(3)
	FOR i=0,N_ELEMENTS(wSMPARAMSbases)-1 $
	DO wSMPARAMSbases[i] = WIDGET_BASE(wSMPbase, /COL, SCR_XSIZE=300, /ALIGN_CENTER, $
								XPAD=0, YPAD=0, SPACE=0, MAP= i EQ 0 ? 1 : 0)

		wSpiceEllParamsBase = wSMPARAMSbases[0]
			wEllNpts_Base  = WIDGET_BASE(wSpiceEllParamsBase, /ROW)
				wEllNpts_Lbl  = WIDGET_LABEL(wEllNpts_Base, VALUE="Number of points:")
				wEllNpts_dlist = WIDGET_COMBOBOX(wEllNpts_Base, /EDITABLE, /FLAT, $
								XSIZE=80, VALUE=nptsList, UNAME="SM ELLIPSOID NPOINTS")
			WIDGET_CONTROL, wEllNpts_dlist, SET_COMBOBOX_SELECT=nptsList_dfltID
	
;		wSMmove = CW_MOVE(wSMPARAMSbases[2], UNAME="MOVE", /ROW)
	
	; Define LIMB DETECTION PARAMETERS widgets:
	wLDMsbases = LONARR(N_ELEMENTS(ldmethods))
	FOR i=0,N_ELEMENTS(ldmethods)-1 $
	DO wLDMsbases[i] = WIDGET_BASE(wLDsbase2, /COL, /ALIGN_CENTER, $
						XPAD=0, YPAD=0, SPACE=0, MAP= i EQ 0 ? 1 : 0)
		
	; Define LIMB FITTING widgets:							
	wLFlbl = WIDGET_LABEL(wLFbase, VALUE="Fit SHAPE MODEL to DETECTED LIMB", /ALIGN_CENTER)
	wLFsbase = WIDGET_BASE(wLFbase, /ROW, /ALIGN_LEFT, /BASE_ALIGN_CENTER)
		wLFdlist = WIDGET_DROPLIST(wLFsbase, UNAME="LIMB FIT METHOD", $
								VALUE=LFlist, TITLE="Method:")
		wLFbttn = WIDGET_BUTTON(wLFsbase, UNAME="LIMB FIT", VALUE="Fit", $
			UVALUE=wLFdlist, XSIZE=40, YSIZE=29)
	wLFThreshBase = WIDGET_BASE(wLFbase, /ROW, XPAD=0, YPAD=0, SPACE=0)
		wFitThresLbl  = WIDGET_LABEL(wLFThreshBase, VALUE=" Threshold (pixel):")
		wFitThresdlist = WIDGET_COMBOBOX(wLFThreshBase, /EDITABLE, XSIZE=70, $
						VALUE=fitThresholdList, UNAME="LIMB FIT THRESHOLD")
		WIDGET_CONTROL, wFitThresdlist, SET_COMBOBOX_SELECT=fitThresholdList_dfltID
		
	;***************************************************************************
	; Realize main base, control subwidgets and initialize:
	;***************************************************************************
	WIDGET_CONTROL, wMAINbase, /REALIZE, UPDATE=0
	caviar_xmove, image.window, PROC2CALL_NAME='caviar_satModel_move', $
						PARENT=wSMPARAMSbases[2], FRAME=0
	caviar_edgrad_gui, PARENT=wLDMsbases[0]
	caviar_edcanny_gui, PARENT=wLDMsbases[1]
	WIDGET_CONTROL, wMAINbase, UPDATE=1
	WIDGET_CONTROL, wLDsbase2, SCR_YSIZE=wLDMbaseYsize
	
	caviar_satLimbFit_gui_init
	
	wSLFbaseYsize = wSMPbaseYsize + wLDMbaseYsize + wSLFcplmtYsize
	
	;***************************************************************************
	; Set 'state' structure with some parameters and copy it in 'extra' widget uvalue:
	;***************************************************************************
	state = {wSMdlist:wSMdlist, wSMPARAMSbttn:wSMPARAMSbttn, $
			 wSMPbase:wSMPbase, wSMPARAMSbases:wSMPARAMSbases, WSMPBASEYSIZE:wSMPbaseYsize, $
			 wLDdlist:wLDdlist, wLDMbttn:wLDMbttn, $
			 wLDMbase:wLDsbase2, WLDMBASEYSIZE:wLDMbaseYsize, wLDMsbases:wLDMsbases, $
			 WSLFCPLMTYSIZE:wSLFcplmtYsize, $
			 SATMODELNPTS:satModelNpts, SMAUTOUPDT:smAutoUpdt, SMCPVALS:smcpVals, $
			 FITTHRESHOLD:fitThresholdList[fitThresholdList_dfltID]}
	WIDGET_CONTROL, extra, SET_UVALUE=PTR_NEW(state, /NO_COPY)
	
	XMANAGER, 'caviar_satLimbFit_gui', wMAINbase, /JUST_REG
END
