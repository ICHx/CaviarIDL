@caviar_zoom.pro


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;	List of procedure of this file:
;		- FUNC caviar_gui_getZoomFactorList
;		- PRO  caviar_gui_changeZoomFactor
;		- PRO  caviar_gui_event  
;		- PRO  caviar_gui
;-------------------------------------------------------------------------------

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_GUI_GETZOOMFACTORLIST
; PURPOSE: Get zoom factors list from min to max, compatible with window dimensions
;-------------------------------------------------------------------------------
FUNCTION caviar_gui_getZoomFactorList, min, max, ns, nl
	
	; Get divisors of ns and nl dimension
	dns = getDivisors(ns, MAX=1.0D/min)
	dnl = getDivisors(nl, MAX=1.0D/min)
	
	; Remove irrational factors
	dns = dns[WHERE(ns*1.0D/dns-FLOOR(ns*1.0D/dns) EQ 0)]
	dnl = dnl[WHERE(nl*1.0D/dnl-FLOOR(nl*1.0D/dnl) EQ 0)]
	
	; Keep only common divisors from dns & dnl 
	dcom = dns[WHERE(dns EQ dnl)]
	
	RETURN, [REVERSE(1.0D/dcom), DINDGEN(max-1)+2]
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_GUI_CHANGEZOOMFACTOR
; PURPOSE: 
;-------------------------------------------------------------------------------
PRO caviar_gui_changeZoomFactor, zfNew, xcursor, ycursor
	
	COMMON CAVIAR_DATA, image, catstars, imgStars, planets
	COMMON CAVIAR_PARAMS, dispParams, imgDraw, dispProName, starsCatalogs, find_imgStars_params
	COMMON CAVIAR_GUI, wCAVIARtlb
	
	WIDGET_CONTROL, WIDGET_INFO(wCAVIARtlb, /CHILD), GET_UVALUE=pstate
	wZOOMbttn = (*pstate).wZOOMbttn
	wIMGdraw  = (*pstate).wIMGdraw
	
	; If cursor position is not defined, take the viewport center:
	IF xcursor EQ !NULL THEN xcursor = 0.5*imgDraw.VSIZE[0]
	IF ycursor EQ !NULL THEN ycursor = 0.5*imgDraw.VSIZE[1]

	; Get previous zoom factor:
	zfOld = imgDraw.ZFACTOR
	
	; Update zoom factor value:
	zfNew = zfNew[0]
	imgDraw.ZFACTOR = zfNew

	; Update zoom factor button value:
	WIDGET_CONTROL, wZOOMbttn, SET_VALUE="Zoom x"+STRING(zfNew, '(G0)')
	
	; Update image draw offset:			
	r = zfNew / zfOld
	WIDGET_CONTROL, wIMGdraw, GET_DRAW_VIEW=offset_ini
	center_ini = offset_ini + 0.5*imgDraw.VSIZE
	center_new = r*center_ini + (r-1)*([xcursor, ycursor]-0.5*imgDraw.VSIZE)
;	offset = center_new - 0.5*imgDraw.VSIZE
;njc Fix problem on large screen when zoom < 1:
        offset = [0.0,0.0]

	; Constraint offset to keep viewport fully filled by the image: 
	offset = offset < (zfNew*[image.ns,image.nl]-imgDraw.VSIZE) > [0,0]
	imgDraw.OFFSET = ROUND(offset);[xOffset, yOffset])

	; Update window dimensions and display
	WIDGET_CONTROL, wIMGdraw, DRAW_XSIZE=image.ns*zfNew, DRAW_YSIZE=image.nl*zfNew
	WIDGET_CONTROL, wIMGdraw, SET_DRAW_VIEW=imgDraw.OFFSET
	
	caviar_display
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_GUI_INIT
; PURPOSE: Set default values/labels/parameters/etc. of the GUI.
;-------------------------------------------------------------------------------
PRO caviar_gui_init
	
	COMMON CAVIAR_DATA, image, catstars, imgStars, planets
	COMMON CAVIAR_PARAMS, dispParams, imgDraw, dispProName, starsCatalogs, find_imgStars_params
	COMMON CAVIAR_GUI, wCAVIARtlb

	WIDGET_CONTROL, WIDGET_INFO(wCAVIARtlb, /CHILD), GET_UVALUE=pstate
	wZOOMbttn = (*pstate).wZOOMbttn
	wTOOLStab = (*pstate).wTOOLStab
	wIMGdraw  = (*pstate).wIMGdraw
	
	ns = image.ns
	nl = image.nl
	WIDGET_CONTROL, wImgDraw, GET_VALUE=imgWinID
	image.window = imgWinID
	;caviarSettings.active_imgWinID = image.window

	; Set CaVIaR main GUI title:
	WIDGET_CONTROL, wCAVIARtlb, TLB_SET_TITLE="Caviar - "+image.NAME
	
	; Set tabs view to the first one, with labels' table:
	WIDGET_CONTROL, wTOOLStab, SET_TAB_CURRENT=0
		
	; Get new zoom factors and zoom factor list:
	zfList = caviar_gui_getZoomFactorList(0.1, 10, ns, nl)
	imgDraw.ZFACTOR = (zf = zfList[WHERE(zfList EQ image.binning)])
	
	; Destroy previous zoom factor menu sub-widgets:
	wZoomSubBttns = WIDGET_INFO(wZOOMbttn, /ALL_CHILDREN)
	nbttn = WIDGET_INFO(wZOOMbttn, /N_CHILDREN)
	FOR i=0, nbttn-1 DO WIDGET_CONTROL, wZoomSubBttns[i], /DESTROY
	
	; Create new zoom factor menu sub-widgets:
	FOR i=0, N_ELEMENTS(zfList)-1 $
	DO wBttn = WIDGET_BUTTON(wZOOMbttn, VALUE="x"+STRING(zfList[i], '(G0)'), $
							 UNAME="DISP ZOOM FACTOR", UVALUE=zfList[i])
	WIDGET_CONTROL, wZOOMbttn, /REALIZE, SET_UVALUE=zfList
	WIDGET_CONTROL, wZOOMbttn, SET_VALUE="Zoom x"+STRING(zf, '(G0)')

	; Set image draw widget size and view:
	xOffset = (zf*ns - imgDraw.VSIZE[0]) > 0
	yOffset = (zf*nl - imgDraw.VSIZE[1]) > 0

	imgDraw.OFFSET = ROUND(0.5*[xOffset, yOffset])

	WIDGET_CONTROL, wIMGdraw, DRAW_XSIZE=zf*ns, DRAW_YSIZE=zf*nl

	WIDGET_CONTROL, wIMGdraw, SET_DRAW_VIEW=imgDraw.OFFSET

	; Display image:
	dispProName='caviar_repoint_display'
	caviar_display
END

;###############################################################################
;#	                        Manage events of the widget 					   #
;###############################################################################
PRO caviar_gui_event, event
	
	COMMON CAVIAR_DATA, image, catstars, imgStars, planets
	COMMON CAVIAR_PARAMS, dispParams, imgDraw, dispProName, starsCatalogs, find_imgStars_params
	COMMON CAVIAR_GUI, wCAVIARtlb
	COMMON CAVIAR_FINDSTARS, wFindStarsTxt

	; Get state from the first child of the widget root:
	child = WIDGET_INFO(event.HANDLER, /CHILD)
	WIDGET_CONTROL, child, GET_UVALUE=pState
	wTOOLStab = (*pstate).wTOOLStab
	wIMGdraw  = (*pstate).wIMGdraw
	
	parent = WIDGET_INFO(event.ID, /PARENT)
	WIDGET_CONTROL, event.HANDLER, TLB_GET_OFFSET=tlbOffset

	CATCH, Error_status
	IF Error_status NE 0 THEN BEGIN
		res = DIALOG_MESSAGE(!ERROR_STATE.MSG, DIALOG_PARENT=event.HANDLER, /CENTER)
		RETURN
	ENDIF
	
	uname = WIDGET_INFO(event.id, /UNAME)
	CASE uname OF
		"TOP_BASE": BEGIN
;			IF ISA(event, 'WIDGET_BASE') THEN BEGIN ; Resize top base
;njc Determine if scrollbars are needed based on monitor information:
                        oInfo = OBJ_NEW('IDLsysMonitorInfo')
                        rects = oInfo->GetRectangles(/EXCLUDE_TASKBAR)
                        OBJ_DESTROY, oInfo
                        maxHeight = MAX(rects[3,*], monIndex)
                        ypadspace = 8
                        IF rects[3,monIndex] lt image.NL*image.binning+ypadspace THEN BEGIN
;njc Then we need scroll bars:
				WIDGET_CONTROL, /HOURGLASS
;				imgDraw.VSIZE = (s = [event.X-(*pState).lpbXsize-12, (event.Y > 450)-12])
;njc Prevents negative values and warning:
                                imgDraw.VSIZE = (s = [(event.X-(*pState).lpbXsize-12)>0, (event.Y > 450)-12])

				WIDGET_CONTROL, wIMGdraw, SCR_XSIZE=s[0], SCR_YSIZE=s[1]
				WIDGET_CONTROL, wIMGdraw, SET_DRAW_VIEW=imgDraw.OFFSET

				; Resize TOOLS TABS base and sub-bases:
				ttbYsize = (event.Y > 450)-(*pState).tbDYsize+13
				WIDGET_CONTROL, (*pState).wTOOLSbase, YSIZE=ttbYsize+24
				children = WIDGET_INFO(wTOOLStab, /ALL_CHILDREN)
				FOR i=0, N_ELEMENTS(children)-1 $
				DO WIDGET_CONTROL, children[i], SCR_YSIZE=ttbYsize
			
				caviar_display

			ENDIF
		END
		
		;***********************************************************************
		"IMAGE DRAW": BEGIN
			;help, event
			CASE event.TYPE OF
				;>>>>>>>>>> Mouse button has been pressed
				0: BEGIN
					CURSOR, xA, yA, /NOWAIT, /DATA
					ptA = [xA, yA]
					
					; Mouse right button is pressed and inside
					; so print information about the cursor position
					IF !MOUSE.BUTTON EQ 4 THEN BEGIN
						infos = caviar_zoom_getInfo(xA*!D.X_VSIZE, yA*!D.Y_VSIZE)
						FOR i=0, N_ELEMENTS(infos)-1 DO PRINT, infos[i]
						PRINT, ""
					ENDIF
					
					; Mouse left button is pressed and inside image
					WHILE !MOUSE.BUTTON EQ 1 DO BEGIN ; Left button is pressed $
						DEVICE, CURSOR_STANDARD=52
						
						CURSOR, xB, yB, /NOWAIT, /DATA
						ptB = [xB, yB]
						
						IF TOTAL(ptB GT [1./!D.X_VSIZE,1./!D.Y_VSIZE]) NE 2 $
						OR TOTAL(ptB LT 1-[1./!D.X_VSIZE,1./!D.Y_VSIZE]) NE 2 $
						THEN BREAK 

                                                minoff = [0.0,0.0]
						maxoff = imgDraw.ZFACTOR * [image.ns,image.nl] - imgDraw.VSIZE
						maxoff = maxoff > [0.0, 0.0]
						offset = imgDraw.OFFSET - (ptB-ptA)*[!D.X_VSIZE,!D.Y_VSIZE]
						imgDraw.OFFSET = minoff > offset < maxoff
						WIDGET_CONTROL, wIMGdraw, SET_DRAW_VIEW=imgDraw.OFFSET

						caviar_display
						
						; Mouse position B become mouse position A
						ptA = ptB
					ENDWHILE
					
					DEVICE, CURSOR_STANDARD=34
				END

				;>>>>>>>>>> Mouse has been moved...
				2: BEGIN
					WIDGET_CONTROL, HOURGLASS=0
			
					QZBgeo = WIDGET_INFO((*pState).wQZOOMbase, /GEOMETRY)
					
					IF QZBgeo.ysize EQ 1 THEN BEGIN
						; The mouse is entering the image viewport, so we "expand" the QZoom widget:
						geoTTBase = WIDGET_INFO((*pState).wTOOLSbase, /GEOMETRY)
						WIDGET_CONTROL, (*pState).wTOOLSbase, SET_UVALUE=geoTTBase
						WIDGET_CONTROL, (*pState).wTOOLSbase, YSIZE=(geoTTBase.ysize-369)
						
						WIDGET_CONTROL, event.ID, TLB_GET_SIZE=TLBsize
						ttbYsize = (TLBsize[1] > 450)-(*pState).tbDYsize+13
						
						children = WIDGET_INFO((*pState).wTOOLStab, /ALL_CHILDREN)
						FOR i=0, N_ELEMENTS(children)-1 $
						DO WIDGET_CONTROL, children[i], SCR_YSIZE=ttbYsize
				
						WIDGET_CONTROL, (*pState).wQZOOMbase, MAP=1, YSIZE=370
					ENDIF
					
					
					WSET, image.window
					CURSOR, xcur, ycur, /NOWAIT, /DATA

					IF (xcur GT 1./!D.X_VSIZE AND xcur LT 1-1./!D.X_VSIZE) $
					AND (ycur GT 1./!D.Y_VSIZE AND ycur LT 1-1./!D.Y_VSIZE) $
					THEN BEGIN

						; The mouse is inside the image viewport, so we update the QZoom view:
						WIDGET_CONTROL, (*pState).wQZOOMdraw, GET_VALUE=QZwin
						QZgeo = WIDGET_INFO((*pState).wQZOOMdraw, /GEOMETRY)
						QZfactor = 4
						caviar_zoom_display, xcur*!D.X_VSIZE, ycur*!D.Y_VSIZE, $
							QZwin, QZgeo.DRAW_XSIZE, QZgeo.DRAW_YSIZE, QZfactor, $
							INTERP=0, FRAME=0
					END ELSE BEGIN
; The mouse is leaving the image viewport, so we "minimize" the QZoom widget
; Now using the REFRESH button to remove the zoom widget instead (see below).
;						WIDGET_CONTROL, (*pState).wQZOOMbase, YSIZE=1, MAP=0
;						WIDGET_CONTROL, (*pState).wTOOLSbase, GET_UVALUE=geoTTBase
;						WIDGET_CONTROL, (*pState).wTOOLSbase, YSIZE=geoTTBase.ysize
					ENDELSE
				END

				;>>>>>>>>>> Scrollbars have been moved...
				3: BEGIN
					imgDraw.OFFSET = [event.X, event.Y]
					caviar_display
				END

				;>>>>>>>>>> Mouse wheel has been scrolled...
				7: BEGIN
					WIDGET_CONTROL, (*pstate).wZOOMbttn, GET_UVALUE=zfList
					zfNew = zfList[WHERE(zfList EQ imgDraw.ZFACTOR) + event.CLICKS]
					caviar_gui_changeZoomFactor, zfNew, event.X, event.Y
				END
				ELSE:
			ENDCASE
		END
		
				
		;***********************************************************************
		"LOAD_IMAGE": BEGIN
;Load new image using drop-down from main gui:
			; Save old caviar data before reset, in case of error loading the new ones
			caviar_data_save

			; Load new image:
			caviar_image_load_gui, GROUP_LEADER=wCAVIARtlb

			; Initialize GUIs with new data:
			IF ISA(image, 'STRUCT') THEN caviar_gui_init
			children = WIDGET_INFO(wTOOLStab, /ALL_CHILDREN)
			IF WIDGET_INFO(children[0], /CHILD) NE 0 THEN BEGIN
				caviar_catstars_gui_init
				caviar_imgstars_gui_init
				caviar_repoint_gui_init
			ENDIF
			IF WIDGET_INFO(children[1], /CHILD) NE 0 THEN caviar_satPos_gui_init

		END
		
		"LOAD_POINTING_FILE": BEGIN
;Load new pointing file using drop-down from main gui:
			filters = [['*.qmpf;*.QMPF', '*.*'],['QMPF','All files']]
			pntgFile = DIALOG_PICKFILE(/READ, /MUST_EXIST, DIALOG_PARENT=wCAVIARtlb, $
					PATH=image.path, FILTER=filters)
			IF ISA(pntgFile,'STRING') && pntgFile NE '' $
			THEN cmat = caviar_file2cmat(pntgFile)
			IF N_ELEMENTS(cmat) NE 9 THEN BREAK
			
			image.cmat = cmat
			image.cmat_ini = cmat
			image.cmat_saved = cmat
			caviar_updtCoords, catstars, image.vobs_stars
			caviar_updtCoords, planets
			caviar_updtCoords, rings
			caviar_display
		END

		"LOAD_SATELLITES": caviar_satellites_gui, GROUP_LEADER=WIDGET_INFO(parent, /PARENT)
		"LOAD_RINGS":      caviar_rings_gui,      GROUP_LEADER=WIDGET_INFO(parent, /PARENT)
		
		;***********************************************************************
		"INFO_IMGLBL": caviar_xvarinfo, image.header, TITLE="Image header"
		
		;***********************************************************************
		"DISP ZOOM FACTOR": BEGIN
			WIDGET_CONTROL, /HOURGLASS
			WIDGET_CONTROL, event.ID, GET_UVALUE=zfNew
			caviar_gui_changeZoomFactor, zfNew
		END 
		
		"DISP REFRESH": BEGIN
;njc Added zoom removal here. So now, clicking the refresh button will also remove the zoom window.
                        WIDGET_CONTROL, (*pState).wQZOOMbase, YSIZE=1, MAP=0
                        WIDGET_CONTROL, (*pState).wTOOLSbase, GET_UVALUE=geoTTBase
                        WIDGET_CONTROL, (*pState).wTOOLSbase, YSIZE=geoTTBase.ysize

			caviar_updtCoords, catstars, image.vobs_stars
			caviar_updtCoords, planets
			caviar_updtCoords, rings
;njc Version of caviar_display for refresh option (excludes the 'imaged stars' i.e from FIND):
 			caviar_redisplay
		END
		"DISP CT": xloadct3, UPDATECALLBACK='caviar_display', GROUP_LEADER=parent
		
		
		;***********************************************************************
		"TOOLS TABS": BEGIN
			WIDGET_CONTROL, /HOURGLASS
			
			children = WIDGET_INFO(event.ID, /ALL_CHILDREN)
			childBase = WIDGET_INFO(children[event.TAB], /CHILD)
			CASE event.TAB OF
				0: BEGIN
					IF childBase EQ 0 THEN caviar_repoint_gui, PARENT=children[0]
					dispProName = 'caviar_repoint_display'
				END
				1: BEGIN
					IF childBase EQ 0 $
					THEN caviar_satPos_gui, PARENT=children[1] $
					ELSE caviar_satPos_gui_init
					dispProName = 'caviar_satPos_display'
				END
			ENDCASE
			caviar_display
		END
		
		ELSE:
	ENDCASE
	
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;       DISPLAY_GUI
; PURPOSE:
;		      
;		
; CATEGORY:
;       Widgets
;
; CALLING SEQUENCE:
;       display_gui
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
;		2012, july			MEUNIER L-E		OBSPM>IMCCE
;			- Written
;-------------------------------------------------------------------------------

PRO caviar_gui, TITLE=title, AUTOLOADSATS=autoloadsats, AUTOLOADSTARS=autoloadstars
	
	COMMON CAVIAR_PARAMS, dispParams, imgDraw, dispProName, starsCatalogs, find_imgStars_params
	COMMON CAVIAR_DATA, image, catstars, imgstars, planets, rings
	COMMON CAVIAR_GUI, wCAVIARtlb
	
	; Test if widget window is already launched
	IF XRegistered('caviar_gui') NE 0 THEN RETURN
	
	
	IF haveTag(image, 'CMAT') && N_ELEMENTS(image.CMAT) EQ 9 THEN BEGIN
		;***********************************************************************
		; Get planet and satellites corresponding to the image target:
		;***********************************************************************
		IF KEYWORD_SET(autoloadsats) && haveTag(image.TARGET, 'ID') THEN BEGIN
			CATCH, error
			IF error NE 0 THEN res=DIALOG_MESSAGE(!ERROR_STATE.MSG, /CENTER) $
			ELSE BEGIN
;Converts the target name (possibly a satellite) into a planet name:
				satsparentid = image.TARGET.ID/100*100+99
				caviar_satellites_load, satsparentid
				IF N_ELEMENTS(planets) EQ 0 $
				THEN MESSAGE, "Target not found: unable to load satellites and parent planet."
			ENDELSE 
			CATCH, /CANCEL
		ENDIF ELSE MESSAGE, "Image target not found in header: unable to load satellites and parent planet."

		;***********************************************************************
		; Get stars from catalogs:
		;***********************************************************************
		IF KEYWORD_SET(autoloadstars) THEN BEGIN
			CATCH, error
			IF error NE 0 THEN res=DIALOG_MESSAGE(!ERROR_STATE.MSG, /CENTER) $
			ELSE BEGIN
				caviar_catstars_load, starsCatalogs, image.FOVIMG, THRESHOLD=image.FOVPIX
				IF N_ELEMENTS(catstars) EQ 0 $
				THEN MESSAGE, "Unable to load stars from catalogues."
			ENDELSE
			CATCH, /CANCEL
		ENDIF
	ENDIF
	
	IF haveTag(image,'NAME') THEN imgName = image.NAME ELSE imgName=''
	IF ~KEYWORD_SET(title) THEN title='CaVIaR - '+imgName
	
	;***************************************************************************
	; Get screen size and define widgets bases dimensions
	;***************************************************************************
	lpbXsize = 360
	xpadspace = 11
	ypadspace = 8
	
	; Get Monitor information
	oInfo = OBJ_NEW('IDLsysMonitorInfo')
	rects = oInfo->GetRectangles(/EXCLUDE_TASKBAR)
	primaryIndex = oInfo->GetPrimaryMonitorIndex()
	OBJ_DESTROY, oInfo

	maxHeight = MAX(rects[3,*], monIndex)
	;monIndex = primaryIndex
	tlbXsize =  640 > (image.NS*image.binning+xpadspace+lpbXsize) < rects[2,monIndex]
	tlbYsize =  480 > (image.NL*image.binning+ypadspace) 		  < rects[3,monIndex]

	imgDraw = {IMGDRAW, ZFACTOR:1.0, INTERP:0, OFFSET:[0.0D,0.0D], $
					VSIZE:[tlbXsize-xpadspace-lpbXsize, tlbYsize-ypadspace]}
	tbDYsize = 102
	
	qzDrawXsize = 350
	qzDrawYsize = 350
	
	; Text box
	infoTxt = "Press right button to print cursor position information"
	infoTxtXsize = FLOOR(FLOAT(qzDrawXsize-10)/!D.X_CH_SIZE)
	infoTxtNline = 5
	
	;***************************************************************************
	; Icons
	;***************************************************************************
	ico_path = GETENV('CAVIAR_ICONS_PATH')
	ico_caviar = ico_path+'favicon.ico'
	ico_updt   = ico_path+'refresh_blue2.bmp'
	ico_zoom   = ico_path+'zoom_blue.bmp'
	ico_ct     = ico_path+'colortable_blue2.bmp'
	
	;***************************************************************************
	; Tooltips
	;***************************************************************************
	ttp_updt = "Refresh image"
	ttp_zoom = "Quick Zoom"
	ttp_ct = "Color table and levels"
	
	
			 
	;***************************************************************************
	; Create CaVIaR main widget
	;***************************************************************************
	wCAVIARtlb = WIDGET_BASE(TITLE=title, UNAME="TOP_BASE", /ROW, $
		/TLB_SIZE_EVENTS, BITMAP=ico_caviar, $
		XOFFSET=rects[0,monIndex]+32, YOFFSET=rects[1,monIndex]+22)
	
	
	extraBase = WIDGET_BASE(wCAVIARtlb, /ROW, XPAD=0, YPAD=0, SPACE=0 )

	;*** Create IMAGE DRAW widget:
	wIMGdraw = WIDGET_DRAW(extraBase, UNAME="IMAGE DRAW", FRAME=1, /APP_SCROLL, $
								/WHEEL_EVENTS, /MOTION_EVENTS, /BUTTON_EVENTS, $
								X_SCROLL_SIZE=imgDraw.VSIZE[0], $
								Y_SCROLL_SIZE=imgDraw.VSIZE[1])
	
	
	;*** Create LEFT PANE widgets:
	wLPbase = WIDGET_BASE(extraBase, /COL, /BASE_ALIGN_CENTER, $
									SCR_XSIZE=lpbXsize, XPAD=3, YPAD=3, SPACE=3)
		
		; Set MENUS and BUTTONS:
		wBTTNSbase = WIDGET_BASE(wLPbase, /ROW, YPAD=0, SPACE=10, /ALIGN_CENTER, /BASE_ALIGN_CENTER)
;			values = ["Image","Pointing file","Satellites","Rings","Stars from Catalogues"]
;			unames = ["LOAD_IMAGE","LOAD_POINTING_FILE","LOAD_SATELLITES","LOAD_RINGS","LOAD_CATSTARS"]
                        values = ["Image","Pointing file","Satellites","Rings"]
                        unames = ["LOAD_IMAGE","LOAD_POINTING_FILE","LOAD_SATELLITES","LOAD_RINGS"]
			wLOADbttn = WIDGET_BUTTON(wBTTNSbase, VALUE="Load", MENU=2)
			FOR i=0,N_ELEMENTS(values)-1 DO $
				wMENUbttn = WIDGET_BUTTON(wLOADbttn, UNAME=unames[i], VALUE=values[i])
	
	
			wInfoBttn = WIDGET_BUTTON(wBTTNSbase, VALUE="Info", MENU=2)
				wImgLblBttn = WIDGET_BUTTON(wInfoBttn, UNAME="INFO_IMGLBL", $
					VALUE="Image header")
	
	
			wZOOMbttn = WIDGET_BUTTON(wBTTNSbase, MENU=2, XSIZE=80, VALUE="Zoom")
	
	
			wDispBase = WIDGET_BASE(wBTTNSbase, /ROW, /BASE_ALIGN_CENTER)
				wUpdBttn   = WIDGET_BUTTON(wDispBase, VALUE=ico_updt, /BITMAP, $
								TOOLTIP=ttp_updt, UNAME="DISP REFRESH")
				wCTBttn    = WIDGET_BUTTON(wDispBase, VALUE=ico_ct, /BITMAP, $
								TOOLTIP=ttp_ct, UNAME="DISP CT")	
		
		; Set TOOLS TABS:
		wTOOLSbase = WIDGET_BASE(wLPbase)
			wTOOLStab = WIDGET_TAB(wTOOLSbase, /ALIGN_CENTER, UNAME='TOOLS TABS')
			
			wTOOLSbases = LONARR(2)
			tabsTitle = ["Pointing Correction","Satellite Astrometry"]
			FOR i=0,1 DO wTOOLSbases[i] = WIDGET_BASE(wTOOLStab, TITLE=tabsTitle[i], $
					/COLUMN, /ALIGN_CENTER, /BASE_ALIGN_CENTER, /SCROLL, $
					X_SCROLL_SIZE=lpbXsize-40, Y_SCROLL_SIZE=tlbYsize-tbDYsize)
		
		; Set QUICK ZOOM:
		wQZOOMbase = WIDGET_BASE(wLPbase, /COL, YSIZE=1, XPAD=0, YPAD=0, MAP=0)
			wQZOOMdraw = WIDGET_DRAW(wQZOOMbase, XSIZE=qzDrawXsize, YSIZE=qzDrawYsize)
			wInfoLbl   = WIDGET_LABEL(wQZOOMbase, VALUE=infoTxt)
	
	;___________________________________________________________________________
	
	
	WIDGET_CONTROL, wCAVIARtlb, /REALIZE, /HOURGLASS, UPDATE=0
	caviar_repoint_gui, PARENT=wTOOLSbases[0]
	WIDGET_CONTROL, wCAVIARtlb, UPDATE=1
	
	
	WIDGET_CONTROL, wCAVIARtlb, TLB_GET_SIZE=mainBaseSize
	;imgDraw.VSIZE = mainBaseSize - [xpadspace+lpbXsize, ypadspace]
	
	
	
	state = {wTOOLSbase:wTOOLSbase, $
			 wQZOOMbase:wQZOOMbase, $
			 wIMGdraw:wIMGdraw, $
			 wQZOOMdraw:wQZOOMdraw, $
			 wZOOMbttn:wZOOMbttn, $
			 wTOOLStab:wTOOLStab, $
			 lpbXsize:lpbXsize, tbDYsize:tbDYsize}
	WIDGET_CONTROL, extraBase, SET_UVALUE=PTR_NEW(state, /NO_COPY)
	
	
	
	caviar_gui_init
	
	
	
	WIDGET_CONTROL, wCAVIARtlb, UPDATE=0
	caviar_satPos_gui, PARENT=wTOOLSbases[1]
	WIDGET_CONTROL, wCAVIARtlb, UPDATE=1
	
	
	XMANAGER, 'caviar_gui', wCAVIARtlb, /JUST_REG
END
