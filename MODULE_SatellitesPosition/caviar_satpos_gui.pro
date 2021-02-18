@caviar_satlimbfit_gui.pro
@caviar_findsatcentroid.pro
@caviar_satpos_pkg.pro

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;	List of procedure of this file:
;		- PRO		caviar_satPos_gui_table
;		- PRO		caviar_satPos_gui_init
;		- PRO 		caviar_satPos_gui_event
;		- PRO 		caviar_satPos_gui
;-------------------------------------------------------------------------------

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_SATPOS_GUI_TABLE
; PURPOSE: Set/Update table values with visible satellites data and/or 
;	foreground & background color with selected line.
; INPUTS:
;	wTblID: ID of the table widget to set/update the values
; KEYWORDS:
;	UPDATE_VALUES: To update table values
;	UPDATE_SELECT: To update the color in function of the selected planet
;-------------------------------------------------------------------------------
PRO caviar_satPos_gui_table, UPDATE_VALUES=updtValues, UPDATE_SELECT=updtSelect
	
	COMMON CAVIAR_DATA, image, stars, imgStars, planets
	COMMON CAVIAR_SATPOS, selSatIndex, wSatTblID, wSaveLbl, wSLFbaseYsize 
	
	IF KEYWORD_SET(updtValues) THEN BEGIN
		tblVali = {name:'', id:'', RA:'', dec:'', xcoord:'', ycoord:'', PIXSIZE:'', $
			method: '', RA_cent: '', dec_cent: '', xcent: '', ycent: '', $
			xresidual: '', yresidual: '', sigma: '', xoffset: '', yoffset: ''}
	
		tblValList = LIST()
		FOR i=0, N_ELEMENTS(planets)-1 DO BEGIN
			IF NOT (planets[i]).visible THEN CONTINUE
			sati = planets[i]
			tblVali.name	 = haveTag(sati, 'NAME') EQ 0     ? '' : sati.name
			tblVali.id		 = haveTag(sati, 'ID') EQ 0 	  ? '' : STRCOMPRESS(STRING(sati.id), /REMOVE_ALL)
			tblVali.RA		 = haveTag(sati, 'RA') EQ 0		  ? '' : STRING( sati.RA*180.0D/!DPI, '(D12.7, " deg")' )
			tblVali.dec	  	 = haveTag(sati, 'DEC') EQ 0	  ? '' : STRING( sati.dec*180.0D/!DPI, '(D12.7, " deg")' )
			tblVali.xcoord   = haveTag(sati, 'Xcoord') EQ 0	  ? '' : STRING( sati.xcoord, FORMAT='(D11.4, " px")' )
			tblVali.ycoord   = haveTag(sati, 'Ycoord') EQ 0	  ? '' : STRING( sati.ycoord, FORMAT='(D11.4, " px")' )
			tblVali.PIXSIZE  = haveTag(sati, 'PIXSIZE') EQ 0  ? '' : STRING( sati.PIXSIZE, FORMAT='(D11.2, " km/px")' )
			tblVali.method	 = haveTag(sati, 'METHOD') EQ 0	  ? '' : sati.method
			tblVali.RA_cent  = haveTag(sati, 'RA_cent') EQ 0  ? '' : STRING( sati.ra_cent*180.0D/!DPI, '(D12.7, " deg")')
			tblVali.dec_cent = haveTag(sati, 'DEC_cent') EQ 0 ? '' : STRING( sati.dec_cent*180.0D/!DPI, '(D12.7, " deg")')
			tblVali.xcent	 = haveTag(sati, 'Xcent') EQ 0	  ? '' : STRING( sati.xcent, FORMAT='(D11.4, " px")' )
			tblVali.ycent	 = haveTag(sati, 'Ycent') EQ 0	  ? '' : STRING( sati.ycent, FORMAT='(D11.4, " px")' )
			
			IF TOTAL(haveTag(sati, ['XCENT', 'XCOORD'])) NE 2 $
			THEN tblVali.xresidual='' $ 
			ELSE tblVali.xresidual=STRING(sati.xcent-sati.xcoord, '(D11.4, " px")')
			
			IF TOTAL(haveTag(sati, ['YCENT', 'YCOORD'])) NE 2 $
			THEN tblVali.yresidual='' $ 
			ELSE tblVali.yresidual=STRING(sati.ycent-sati.ycoord, '(D11.4, " px")')
			
			tblVali.sigma    = haveTag(sati, 'SIGMA') EQ 0    ? '' : STRING( sati.sigma, FORMAT='(D11.4, " px")' )
			tblVali.xoffset  = haveTag(sati, 'XOFFSET') EQ 0  ? '' : STRING( sati.xoffset, FORMAT='(D11.4, " px")' )
			tblVali.yoffset  = haveTag(sati, 'YOFFSET') EQ 0  ? '' : STRING( sati.yoffset, FORMAT='(D11.4, " px")' )
				
			tblValList.add, tblVali
		ENDFOR
		
		
		CASE N_ELEMENTS(tblValList) OF
			0: tblVal = tblVali
			1: tblVal = ( tblValList.toArray() )[0]	;'[0]' needed since IDL 8.2
			ELSE: tblVal = tblValList.toArray()
		ENDCASE
		
		WIDGET_CONTROL, wSatTblID, SET_VALUE=tblVal
	ENDIF
	
	
	IF KEYWORD_SET(updtSelect) THEN BEGIN
		; Reset default color for the table
		WIDGET_CONTROL, wSatTblID, BACKGROUND_COLOR=[200,200,200], FOREGROUND_COLOR=[0,0,0]

		; Set specific color for the selected line
		WIDGET_CONTROL, wSatTblID, GET_VALUE=tblValue
		selCells = WIDGET_INFO(wSatTblID, /TABLE_SELECT)

		selCol = selCells[0]
		nLines = N_TAGS(tblValue[selCol])
		WIDGET_CONTROL, wSatTblID, USE_TABLE_SELECT=[selCol, 0, selCol, nLines-1], $
						BACKGROUND_COLOR=[51,102,255], FOREGROUND_COLOR=[255,255,255]
	ENDIF
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_SATPOS_GUI_INIT
; PURPOSE: Set default values/labels/parameters/etc. of the GUI.
;-------------------------------------------------------------------------------
PRO caviar_satPos_gui_init

	COMMON CAVIAR_GUI, wCAVIARtlb, wFITSATLIMBbase, wRINGSLOADbase, wCATSTARSLOADbase, wSATPOSbase
	COMMON CAVIAR_SATPOS, selSatIndex, wSatTblID, wSaveLbl, wSLFbaseYsize
	
	WIDGET_CONTROL, WIDGET_INFO(wSATPOSbase, /CHILD), GET_UVALUE=pstate
	
	caviar_satLimbFit_gui_init
	
	; Set 'visible' status of the planets:
	vSatsIndexList = caviar_satellites_getVisible(/UPDATE)
	selSatIndex = ISA(vSatsIndexList) ? vSatsIndexList[0] : !NULL
	
	; Update planets table:
	tblXsize = N_ELEMENTS(vSatsIndexList) > 1
	WIDGET_CONTROL, wSatTblID, SET_UVALUE=vSatsIndexList, ALIGNMENT=1, $
						TABLE_XSIZE=tblXsize, COLUMN_WIDTHS=105+INTARR(tblXsize)
	WIDGET_CONTROL, wSatTblID, SET_TABLE_SELECT=[0, 0, 0, 0]
	caviar_satPos_gui_table, /UPDATE_VALUES, /UPDATE_SELECT
	
	IF selSatIndex NE !NULL THEN BEGIN
		WIDGET_CONTROL, (*pstate).wCOFbase, MAP=1
		WIDGET_CONTROL, (*pstate).wSAVEbase, MAP=1
	ENDIF
	
	WIDGET_CONTROL, wSaveLbl, SET_VALUE=" "
	caviar_display
END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;	                  Manage events of the widget 		     				   
;-------------------------------------------------------------------------------
PRO caviar_satPos_gui_event, event
	
	
	COMMON CAVIAR_DATA, image, stars, imgStars, planets
	COMMON CAVIAR_SATPOS, selSatIndex, wSatTblID, wSaveLbl, wSLFbaseYsize 
	
	CATCH, Error_status
	IF Error_status NE 0 THEN BEGIN
		res = DIALOG_MESSAGE(!ERROR_STATE.MSG, DIALOG_PARENT=event.HANDLER, /CENTER)
		RETURN
	ENDIF


	; Get state from the first child of the compound widget root:
	child = WIDGET_INFO(event.HANDLER, /CHILD)
	WIDGET_CONTROL, child, GET_UVALUE=pstate
	
	WIDGET_CONTROL, /HOURGLASS
		
	;***************************************************************************
	uname = WIDGET_INFO(event.id, /UNAME)
	CASE uname OF
		
		"TOP_BASE": BEGIN
			IF ISA(event, 'WIDGET_BASE') THEN $
				WIDGET_CONTROL, event.HANDLER, SCR_XSIZE=event.X, SCR_YSIZE=event.Y
		END
		
		;***********************************************************************
		"TABLE": BEGIN
			dispCBname = 'caviar_satPos_display'
			
			IF event.type EQ 4 THEN BEGIN
				IF event.SEL_LEFT EQ event.SEL_RIGHT && event.SEL_LEFT NE -1 THEN BEGIN
					IF selSatIndex EQ !NULL THEN BEGIN
						; Hide center of figure computation widgets:
						WIDGET_CONTROL, (*pstate).wCOFbase, MAP=0
						WIDGET_CONTROL, (*pstate).wSAVEbase, MAP=0
						BREAK
					ENDIF
					
					; Get selected vSats planets index
					WIDGET_CONTROL, event.ID, GET_UVALUE=vSatsIndexList
					selSatIndex = vSatsIndexList[event.SEL_LEFT]
					
					; Show center of figure computation widgets:
					WIDGET_CONTROL, (*pstate).wCOFbase, MAP=1
					WIDGET_CONTROL, (*pstate).wSAVEbase, MAP=1
					
					; Update satDraw.x/y_cent values and display:
					caviar_display
										
					; Update table color to highlight the change of the selected satellite:
					caviar_satPos_gui_table, /UPDATE_SELECT
				ENDIF 	
			ENDIF
		END
		
		"TABLE UPDATE": caviar_satPos_gui_init

		;***********************************************************************
		"CENTROIDING": BEGIN
			WIDGET_CONTROL, (*pstate).wSLFbase, SCR_YSIZE=1
			
			IF NOT ISA(image,'STRUCT') THEN $
				MESSAGE, 'Please load an image first'
			IF ~haveTag(image, 'CMAT') || N_ELEMENTS(image.CMAT) NE 9 THEN $
				MESSAGE, 'Make sure pointing information has been loaded'
			IF selSatIndex EQ !NULL THEN $
				MESSAGE, 'No satellite has been selected to compute centroid'
					
			; Compute satellite centroid (xcent, ycent)
			sat = planets[selSatIndex]
			imgpixfov = image.fovpix*image.binning
			rslt = caviar_findsatcentroid(image.RAW, sat.ID, MAX(sat.RADII), $
					sat.XCOORD, sat.YCOORD, image.ET, imgpixfov, image.SPC.ID, image.CMAT)
						
			; Set planet results and update display:
			caviar_satPos_setResults, "CENTROIDING", [rslt.xcent, rslt.ycent], rslt.sigma, rslt.xoffset, rslt.yoffset
			
			; Update 'satPos' visible satellite table values:
			caviar_satPos_gui_table, /UPDATE_VALUES
			
			caviar_display
		END
		
		;***********************************************************************
		"LIMB-FIT": BEGIN
			WIDGET_CONTROL, (*pstate).wSLFbase, SCR_YSIZE=wSLFbaseYsize
		END
	
		;***********************************************************************
		"SAVE FORMAT": (*pstate).saveFormat = event.STR
		"SAVE": caviar_satPos_save, (*pstate).saveFormat
		
		ELSE:
	ENDCASE
		
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_CALSATPOS_GUI
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
;		2012, december	Written by L-E.	MEUNIER (IMCCE/OBSPM)
;		2013, april			L-E.	MEUNIER (IMCCE/OBSPM)
;			- Change UI
;-------------------------------------------------------------------------------
PRO caviar_satPos_gui, PARENT=parent, XOFFSET=xoffset, YOFFSET=yoffset
	
	COMMON CAVIAR_GUI, wCAVIARtlb, wFITSATLIMBbase, wRINGSLOADbase, wCATSTARSLOADbase, mainBase
	COMMON CAVIAR_SATPOS, selSatIndex, wSatTblID, wSaveLbl, wSLFbaseYsize
	
	; Test if widget window is already launched
	IF(XRegistered('caviar_satPos_gui') NE 0) THEN RETURN
		
	;***************************************************************************
	; Set Parameters 
	;***************************************************************************
	; Set satellites' table parameters
	satTbl_Labels = ['Name', 'ID', 'RA_calc', 'DEC_calc', 'X_calc', 'Y_calc', 'Resolution', $
					 'Method', 'RA_obs', 'DEC_obs', 'X_obs', 'Y_obs', $
					 'Xresidual', 'Yresidual', 'Sigma', 'Xoffset', 'Yoffset']
			
	; Save results parameters
	saveFormatList = ['QMPF']
	saveFormatList_dfltID = 0
	saveFormat = saveFormatList[saveFormatList_dfltID]
	
	; Tooltips	
	lfitBttnTltp = "For resolved satellite -> Fit a shape model to the detected limb"
	centBttnTltp = "For unresolved satellite -> Search for the center-of-light and correct for phase angle"


	;*************************************************************************** 
	; Define the widget base and subwidgets tree 
	;***************************************************************************
	IF KEYWORD_SET(parent) THEN BEGIN
		mainBase = parent[0]
		IF NOT WIDGET_INFO(mainBase, /VALID_ID) $
		THEN res=DIALOG_MESSAGE('Invalid widget identifier.',/ERROR)
	ENDIF ELSE BEGIN
		mainBase = WIDGET_BASE(TITLE="Measure satellite's center-of-figure", $
			/TLB_SIZE_EVENTS, UNAME='TOP_BASE', XOFFSET=xoffset, YOFFSET=yoffset, $
			/SCROLL, X_SCROLL_SIZE=315, Y_SCROLL_SIZE=600)   ; create a dummy parent base
	ENDELSE
	
	extra = WIDGET_BASE(mainBase, /COLUMN, /BASE_ALIGN_LEFT);, /BASE_ALIGN_LEFT)
	
		
	;********** Satellites table:
	wSELSATlbl = WIDGET_LABEL(extra, VALUE=" 1. Select satellite to be measured:")
	wSatTblID = WIDGET_TABLE(extra, /COLUMN_MAJOR, /NO_COLUMN_HEADERS, /ALL_EVENTS, $
		ALIGNMENT=1, UNAME = "TABLE", SCR_XSIZE=302, $
		ROW_LABELS=satTbl_Labels, YSIZE=N_ELEMENTS(satTbl_Labels), $
		BACKGROUND_COLOR=[200,200,200], FOREGROUND_COLOR=[0,0,0], $
		X_SCROLL_SIZE=2, Y_SCROLL_SIZE=N_ELEMENTS(satTbl_Labels))
	wOBJTBLUPDTbttn = WIDGET_BUTTON(extra, VALUE="Update table", UNAME="TABLE UPDATE")
	
	space = WIDGET_LABEL(extra, VALUE='')
	;********** Satellite center of figure calculation:
	wCOFbase = WIDGET_BASE(extra, /COLUMN, XPAD=0, YPAD=0, MAP=0);, XSIZE=305
		wLbl = WIDGET_LABEL(wCOFbase, VALUE=" 2. Select a method to find the center-of-figure:", /ALIGN_LEFT)

;njc             wMethodBase = WIDGET_BASE(wCOFbase, /ALIGN_CENTER, /ROW, XPAD=0, YPAD=0, SPACE=5), /EXCLUSIVE)
		wMethodBase = WIDGET_BASE(wCOFbase, /ALIGN_CENTER, /ROW, XPAD=0, YPAD=0, SPACE=5) ; , /EXCLUSIVE)
			wLFbttn = WIDGET_BUTTON(wMethodBase, UNAME="LIMB-FIT", $
					VALUE="Limb Fitting", YSIZE=30, TOOLTIP=lfitBttnTltp)
			wCentBttn = WIDGET_BUTTON(wMethodBase, UNAME="CENTROIDING", $
					VALUE="Centroiding", YSIZE=30, TOOLTIP=centBttnTltp)
	
		wSLFbase = WIDGET_BASE(wCOFbase, /COL, XPAD=0, YPAD=0, /FRAME)
	
	
	;********** Saving results:
	wSAVEbase = WIDGET_BASE(extra, /ROW, /BASE_ALIGN_CENTER, YPAD=0, MAP=0)
		wSaveBttn = WIDGET_BUTTON(wSaveBase, UNAME="SAVE", VALUE="Save", XSIZE=80, YSIZE=32)
		wSaveFormatLbl = WIDGET_LABEL(wSaveBase, VALUE="Format:")
		wSaveFormatCbox = WIDGET_COMBOBOX(wSaveBase, UNAME="SAVE FORMAT", $
								VALUE=saveFormatList, XSIZE=60)
		wSaveLbl = WIDGET_LABEL(wSaveBase, VALUE=" ", XSIZE=50)
	
	
	WIDGET_CONTROL, mainBase, /REALIZE, UPDATE=0
	caviar_satlimbfit_gui, PARENT=wSLFbase, XPAD=0
	WIDGET_CONTROL, mainBase, UPDATE=1
	
	WIDGET_CONTROL, wSLFbase, SCR_YSIZE=1
	
	; Set default results file format:
	WIDGET_CONTROL, wSaveFormatCbox, SET_COMBOBOX_SELECT=saveFormatList_dfltID
	
	;***************************************************************************
	; Define 'state' structure and copy it in 'extra' widget uvalue
	state = {wCOFbase:wCOFbase, wSLFBase:wSLFBase, $
			 wSAVEbase:wSAVEbase, saveFormat: saveFormat}
	WIDGET_CONTROL, extra, SET_UVALUE=PTR_NEW(state, /NO_COPY)
	
	; Initialize others gui parameters:
	caviar_satPos_gui_init
	
	XMANAGER, 'caviar_satPos_gui', mainBase, /JUST_REG
END
