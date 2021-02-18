;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;  List of functions and procedures of this file:
;		- xloadct3_alert_caller		
;		- xloadct3_showCT
;		- xloadct3_setGAMMA
;		- xloadct3_resetCTparams
;		- xloadct3_drawCPS
;		- xloadct3_transferFUNCTION
;		- xloadct3_Tfun_psave
;		- xloadct3_interpCPS
;		- xloadct3_event
;		- xloadct3		(main program)
;
;-------------------------------------------------------------------------------

;###############################################################################
;#						CAVIAR_XLOADCT_ALERT_CALLER	                           #
;# Call a procedure define by updt_callback and pass arguments contained in    #
;# p_updt_cb_data with the keyword 'DATA'. This procedure could be used to     #
;# re-display the image after changing colors                                  #
;###############################################################################
pro xloadct3_alert_caller
	COMPILE_OPT HIDDEN

	COMMON XLOADCT3_COM, updt_callback, p_updt_cb_data, updt_cb_str
	
	ErrorStatus = 0
    CATCH, ErrorStatus
    IF (ErrorStatus NE 0) THEN BEGIN
        CATCH, /CANCEL
        v = DIALOG_MESSAGE(['Unexpected error in caviar_xloadct:', !ERROR_STATE.msg], /ERROR)
       RETURN
    ENDIF
    
     IF (STRLEN(updt_callback) GT 0) THEN BEGIN
        IF (PTR_VALID(p_updt_cb_data)) THEN CALL_PROCEDURE, updt_callback, DATA=*(p_updt_cb_data) $
        							   ELSE CALL_PROCEDURE, updt_callback
    ENDIF
 
END

;###################################################################################################
;#	            			  		CAVIAR_XLOADCT_SHOWCT										   #
;#  Redraw the color table ramp 																   #
;###################################################################################################
PRO xloadct3_showCT, ctDraw_xsize, ctDraw_ysize, ctDraw_ID
	COMPILE_OPT HIDDEN
	
	COMMON CT_PARAMS, cbot, filename, ncolors, bot, top, gamma, chop, lock, use_values
	
    save_ctDraw = !D.WINDOW					;Save current window system variable
    WSET, ctDraw_ID							;Set window system variable to those where to draw color table ramp
    DEVICE, GET_DECOMPOSED=savedDecomposed	;Save color values interpretation mode
    DEVICE, Decomposed=0					; 0 = Color values are interpreted to indices (1=decomposed)
    TV, BYTE((FLOAT(ncolors)*FINDGEN(ctDraw_xsize)/FLOAT(ctDraw_xsize-1)) # REPLICATE(1, ctDraw_ysize))
    DEVICE, DECOMPOSED=savedDecomposed		;Restore color values interpretation mode
    WSET, save_ctDraw							;Restore saved window system variable

    ; Let the caller of caviar_xloadct know that the color table was modified
    xloadct3_alert_caller
END		
					
;###################################################################################################
;#	               Set gamma and colors of the modified color table								   #
;###################################################################################################
PRO xloadct3_setGAMMA
	COMPILE_OPT HIDDEN
	
	COMMON COLORS, R_orig, G_orig, B_orig, R_curr, G_curr, B_curr
	COMMON CT_PARAMS, cbot, filename, ncolors, bot, top, gamma, chop, lock, use_values
	COMMON CT_RAMP_PARAMS, ct_draw_xsize, ct_draw_ysize, ct_draw_win
						
	nc = use_values ? 255 : ncolors-1
	
	; Convert top and bot parameters into their real color table value
	s = bot NE top ? 1.0/(top - bot) : 1.0
	ct_indices = ( ( s*(FINDGEN(ncolors)-bot) > 0.0 )^gamma )*nc
	
	; If chop option then outside bottom-top range color values are put to zero
	IF chop NE 0 THEN BEGIN
		too_high = WHERE(ct_indices ge nc, n)
		IF n GT 0 THEN ct_indices[too_high] = 0L
	ENDIF
	
	IF use_values THEN BEGIN	
		ct_indices <= 255L
		;Subscripts of origin color table starting at cbot witch may be different than 0 if bottom keyword was set.
		l = LINDGEN(ncolors) + cbot	
		R_curr[cbot] = (r = ct_indices[R_orig[l]])
		G_curr[cbot] = (g = ct_indices[G_orig[l]])
		B_curr[cbot] = (b = ct_indices[B_orig[l]])
	ENDIF ELSE BEGIN
		ct_indices += cbot
		R_curr[cbot] = (r = R_orig[ct_indices])
		G_curr[cbot] = (g = G_orig[ct_indices])
		B_curr[cbot] = (b = B_orig[ct_indices])
 	ENDELSE
		
	; Load the display color translation tables, starting at cbot and with r,g,b intensity
	TVLCT, r, g, b, cbot
	xloadct3_showCT, ct_draw_xsize, ct_draw_ysize, ct_draw_win
END

;###################################################################################################
;#	   			   		Reset color table parameters (bottom, top, gamma)				    	   #
;###################################################################################################
PRO xloadct3_resetCTparams
	COMPILE_OPT HIDDEN
	
	COMMON CT_PARAMS, cbot, filename, ncolors, bot, top, gamma, chop, lock, use_values
	COMMON LEVELS_WIDGETS_ID, wLevelsSGroup
	
	top = 255
	bot = 0
	gamma = 1.0
	WIDGET_CONTROL, wLevelsSGroup, SET_VALUE = [top,bot,gamma]
	
	xloadct3_setGAMMA
END

;###################################################################################################
;#					      			CAVIAR_XLOADCT_DRAWCPS										   #
;#	Overplot control point on transfer function window											   #
;#	cps_i: index of the control point to display (set to -1 to redraw all control points)		   #
;###################################################################################################
PRO xloadct3_drawCPS, cpsi, cps_color
	COMPILE_OPT HIDDEN
	
	COMMON TFUN_PARAMS, color, cps_x, cps_y, tfun_psave, pnt, tfun_draw_win
	
	
	IF N_ELEMENTS(cps_color) GT 0 $
	THEN BEGIN
		tc = cps_color
		IF cps_color NE 0 THEN color=cps_color
	ENDIF $
	ELSE tc = color
	IF cpsi[0] EQ -1 THEN j=INDGEN(N_ELEMENTS(cps_x)) ELSE j=cpsi
	PLOTS, cps_x[j], cps_y[j], COLOR=tc, /NOCLIP
	PLOTS, cps_x[j], cps_y[j], COLOR=tc, /NOCLIP, PSYM=6
END

;###################################################################################################
;#                           CAVIAR_XLOADCT_TRANSFERFUNCTION									   #
;#	Set output color levels following the transfert function define by the control points	       #
;#	making linear interpolation beetween them.													   #
;###################################################################################################
PRO xloadct3_transferFUNCTION, UPDATE_CT=UPDATE_CT
	COMPILE_OPT HIDDEN
	
	COMMON COLORS, R_orig, G_orig, B_orig, R_curr, G_curr, B_curr
	COMMON CT_PARAMS, cbot, filename, ncolors, bot, top, gamma, chop, lock, use_values
	COMMON CT_RAMP_PARAMS, ct_draw_xsize, ct_draw_ysize, ct_draw_win
	COMMON TFUN_PARAMS, color, cps_x, cps_y, tfun_psave, pnt, tfun_draw_win
	
	ol = INTARR(ncolors)		;Initialize output color levels array
	FOR i=0, N_ELEMENTS(cps_x)-2 DO BEGIN
		dx = cps_x[i+1]-cps_x[i]
		dy = cps_y[i+1]-cps_y[i]
		ol[cps_x[i]:cps_x[i+1]-1] = dy/FLOAT(dx) * FINDGEN(dx) + (cps_y[i] + cbot)		
	ENDFOR
	ol[ncolors-1] = cps_y[N_ELEMENTS(cps_y)-1]		;Last point
	
	IF use_values THEN BEGIN
		R_curr[cbot] = (r = ol[R_orig])
		G_curr[cbot] = (g = ol[G_orig])
		B_curr[cbot] = (b = ol[B_orig])
	ENDIF ELSE BEGIN
		R_curr[cbot] = (r = R_orig[ol])
		G_curr[cbot] = (g = G_orig[ol])
		B_curr[cbot] = (b = B_orig[ol])
	ENDELSE
	TVLCT, r, g, b, cbot
	IF KEYWORD_SET(UPDATE_CT) THEN xloadct3_showCT, ct_draw_xsize, ct_draw_ysize, ct_draw_win
END

;###################################################################################################
;#								   CAVIAR_XLOADCT_TFUN_PSAVE									   #
;#  Save/Restore our plotting state																   #
;#	Swaps our transfer function plotting state with the current state each time its called		   #
;###################################################################################################
PRO xloadct3_Tfun_psave
	COMPILE_OPT hidden
	
	COMMON TFUN_PARAMS, color, cps_x, cps_y, tfun_psave, pnt, tfun_draw_win
							
	tmp = { xloadct_Tfun_psave, win:!D.WINDOW, x:!X.S, y:!Y.S, xtype:!X.TYPE, ytype:!Y.TYPE, clip:!P.CLIP }
	WSET, Tfun_psave.win
	
	; X,Y axis type (0=linear, 1=logarithmic)
	!X.TYPE = Tfun_psave.xtype		
	!Y.TYPE = Tfun_psave.ytype		
	
	; X,Y scaling factor between data and normalized coordinates (2-elements arrays)
	!X.S = Tfun_psave.x				
	!Y.S = Tfun_psave.y				
	
	; Device coordinates of the clipping window specifying two opposite corners of the volume 
	; (6-elements array)
	!P.CLIP = Tfun_psave.clip		
	
	Tfun_psave = tmp
END

;###################################################################################################
;#	   			   						XLOADCT_INTERPCPS  								     	   #
;#  1- Draw the new control points state														   #
;#  2- Update colors with the new transfer function											   #
;#  3-  
;###################################################################################################
PRO xloadct3_interpCPS, RESET_ALL=RESET_ALL
	COMPILE_OPT HIDDEN
	
	xloadct3_drawCPS, -1  				;Redraw with actualized control points
	xloadct3_transferFUNCTION, /UPDATE	;Update colors with actualized control points
	xloadct3_Tfun_psave					;Restore old points
	IF N_ELEMENTS(RESET_ALL) THEN xloadct3_resetCTparams
END	

;###################################################################################################
;#	                            	Manage events of the widget 								   #
;###################################################################################################
PRO xloadct3_event, event
	COMPILE_OPT HIDDEN
	
	COMMON COLORS, R_orig, G_orig, B_orig, R_curr, G_curr, B_curr
	COMMON SAVED_COLORS, R_save, G_save, B_save
	COMMON CT_PARAMS, cbot, filename, ncolors, bot, top, gamma, chop, lock, use_values
	COMMON CT_RAMP_PARAMS, ct_draw_xsize, ct_draw_ysize, ct_draw_win
	COMMON TFUN_PARAMS, color, cps_x, cps_y, tfun_psave, pnt, tfun_draw_win
	COMMON XLOADCT3_COM, updt_callback, p_updt_cb_data
	
	COMMON LEVELS_WIDGETS_ID, wLevelsSGroup
		
	
	
	
	uname = WIDGET_INFO(event.id, /UNAME)
	CASE uname OF
		
		;***********************************************************************	
		; Make active or inactive color table levels and transfer function widgets
		;***********************************************************************
		'TABS': BEGIN 
			IF event.tab EQ 2 THEN BEGIN 
				old_win = !D.WINDOW
				WSET, tfun_draw_win
				xloadct3_Tfun_psave			;Save old state
				PLOT, /NODATA, [0, ncolors-1], [0, ncolors-1], $
					XSTYLE=3, YSTYLE=3, XMARGIN=[1,1], YMARGIN=[1,1], $
					TICKLEN=-0.03, XTICKNAME=REPLICATE(' ', 10), YTICKNAME=REPLICATE(' ', 10)
				xloadct3_interpCPS, /RESET_ALL
				WSET, old_win
			ENDIF
		END
	
	
		;***********************************************************************
		; COLOR TABLE: list & options
		;***********************************************************************
		'LIST': BEGIN
			LOADCT, event.index, FILE=filename, NCOLORS=ncolors, BOTTOM=cbot, /SILENT
			TVLCT, R_orig, G_orig, B_orig, /GET
			R_save = (R_curr = R_orig)
			G_save = (G_curr = G_orig)
			B_save = (B_curr = B_orig)
			xloadct3_setGAMMA
			RETURN
		END
		'REVERSE': BEGIN                  ;Reverse the table
			l = LINDGEN(ncolors) + cbot
			R_orig[cbot] = REVERSE(R_orig[l])
			G_orig[cbot] = REVERSE(G_orig[l])
			B_orig[cbot] = REVERSE(B_orig[l])
			xloadct3_setGAMMA                
			RETURN
		END
		'REPLACE': BEGIN                  ;overwrite original tables
			R_save = (R_orig = R_curr)
			G_save = (G_orig = G_curr)
			B_save = (B_orig = B_curr)
			xloadct3_resetCTparams
			RETURN
		END
		'RESTORE': BEGIN
			; Restore original color table
			R_curr = (R_orig = R_save)
			G_curr = (G_orig = G_save)
			B_curr = (B_orig = B_save)
			TVLCT, R_curr, G_curr, B_curr
			xloadct3_resetCTparams
			
			; Reset transfer function
			xloadct3_Tfun_psave			;Save old state
			xloadct3_drawCPS, -1, 0		;Erase all control point from window
			cps_y = cps_x				;Linear ramp
			xloadct3_interpCPS
			RETURN
		END
		
				
		;***********************************************************************
		; LEVELS: sliders & options
		;***********************************************************************
		'LEVELS SLIDERS': BEGIN
			top = event.value[0]
			bot = event.value[1]
			gamma = 10^(event.value[2]-1)
			
			IF lock NE 0 THEN BEGIN
				bot = (top - lock) > 0 < 256
				top = (bot + lock) > 0 < 256
			ENDIF
						
			WIDGET_CONTROL, wLevelsSGroup, SET_VALUE=[top, bot, event.value[2]]	
			xloadct3_setGAMMA
		END
		
		'BIND_SLIDERS':	lock = event.select ? top-bot : 0
			

		'TOP_METHOD': BEGIN			;active option and redraw color table
			chop = event.value
			xloadct3_setGAMMA
		END
				 
		'STRETCH': use_values = event.value
		
		
		
		;***********************************************************************
		; TRANSFER FUNCTION
		;***********************************************************************
		'TFUN_DRAW': BEGIN
			;########## Pressed button? 
			IF event.press NE 0 THEN BEGIN		
				xloadct3_Tfun_psave	;Remove old
				p = CONVERT_COORD(event.x, event.y, /TO_DATA, /DEVICE)
				xloadct3_Tfun_psave	;Restore old
				dmin = 1.0e8				;Distance threshold to find closest control point
				FOR i=0, N_ELEMENTS(cps_x)-1 DO BEGIN
					d = (p[0]-cps_x[i])^2 + (p[1]-cps_y[i])^2  ; dist ^ 2
					IF d LT dmin THEN BEGIN
						dmin = d
						pnt = i
					ENDIF
				ENDFOR
				x = FIX(p[0])
				y = FIX(p[1])
				RETURN
			ENDIF
			
			;########## Released button?
			IF event.release NE 0 THEN BEGIN
				pnt = -1
				xloadct3_transferFUNCTION, /UPDATE
				RETURN
			ENDIF
			
			;########## If buttons are not pressed (pnt=-1) then do nothing...
			IF pnt LT 0 THEN RETURN			;Don't care here...
			
			;########## Otherwise, do this...
			xloadct3_Tfun_psave		;Remove old
			
			; For visuals with static colormaps, erase plot before drawing new
			IF (COLORMAP_APPLICABLE(redrawRequired) GT 0) && (redrawRequired GT 0) THEN ERASE, color=0
			
			;# Get the selected point coords into data space
			p = CONVERT_COORD(event.x, event.y, /TO_DATA, /DEVICE)	;Coord of mouse
			
			;# Set value for cps_x and cps_y from p coords with 
			;#		...cps_x first and last value fixed to 0 and ncolors-1 respectively,
			;#		...cps_x other value fixed to the neighbor point value and
			;#		...cps_y value fixed to the range between 0 and ncolors-1
			m = N_ELEMENTS(cps_x)-1
			IF pnt EQ 0 THEN cps_x[pnt] = 0 		ELSE $
			IF pnt EQ m THEN cps_x[pnt] = ncolors-1 ELSE $
							 cps_x[pnt] = FIX(p[0]) > (cps_x[pnt-1]+1) < (cps_x[pnt+1]-1)
			cps_y[pnt] = FIX(p[1]) > 0 < (ncolors-1)
			
			;# Update colors with new transfer function
			xloadct3_transferFUNCTION
			
			
			b = 0.3*R_curr + 0.586*G_curr + 0.114*B_curr 	;NTSC colors
			c = MAX(ABS(b-b[cbot]), pos)  					; *** pos is color index furthest from 0
			
			;# Erase previous control points and segments close to the selected one and 
			;# then draw new ones
			CASE pnt OF
				0: BEGIN
					xloadct3_drawCPS, [0, 1], 0		;Erase segment with black color (0)
					xloadct3_drawCPS, [0, 1], pos 	;Draw segment with max color (pos)	
				END
				m: BEGIN
					xloadct3_drawCPS, [m-1, m], 0
					xloadct3_drawCPS, [m-1, m], pos 	
				END
				ELSE: BEGIN
					xloadct3_drawCPS, [pnt-1, pnt, pnt+1], 0
					xloadct3_drawCPS, [pnt-1, pnt, pnt+1], pos
				END
			ENDCASE
			xloadct3_Tfun_psave		;Remove old
			RETURN
		END
		
		'CP_RMV': BEGIN		; Remove control point
			n = N_ELEMENTS(cps_x)
			IF n GT 2 THEN BEGIN
				xloadct3_Tfun_psave		;Save old state
				xloadct3_drawCPS, -1, 0	;Erase all control point from window
				igap = 0
				FOR i=0, n-2 DO BEGIN
					IF (cps_x[i+1]-cps_x[i]) LT (cps_x[igap+1]-cps_x[igap]) THEN igap=i
				ENDFOR
				keep = WHERE(INDGEN(n) NE (igap>1))
				cps_x = cps_x[keep]
				cps_y = cps_y[keep]
				xloadct3_interpCPS
			ENDIF
		END
		
		'CP_ADD': BEGIN		; Add control point
			xloadct3_Tfun_psave				;Save old state
			xloadct3_drawCPS, -1, 0			;Erase all control point from window
			
			igap = 0			;Find largest gap
			FOR i=0, N_ELEMENTS(cps_x)-2 DO BEGIN
				IF (cps_x[i+1]-cps_x[i]) GT (cps_x[igap+1]-cps_x[igap]) THEN igap = i
			ENDFOR
			cps_x = [ cps_x[0:igap], (cps_x[igap]+cps_x[igap+1])/2, cps_x[igap+1:*]]
			cps_y = [ cps_y[0:igap], (cps_y[igap]+cps_y[igap+1])/2, cps_y[igap+1:*]]
			xloadct3_interpCPS
		END
		
			
			

		ELSE:
	ENDCASE

END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;       xloadct3 (changed from Xloadct)
; PURPOSE:
;       A graphical interface to the LOADCT user library procedure.
;       XLOADCT displays the current color map and provides list of available predefined color table.
;       Using the mouse to press these buttons causes the corresponding color map to be loaded.   
; CATEGORY:
;       Widgets
; CALLING SEQUENCE:
;       xloadct3 [, PARENT=parent_widget_id][, UPDATECALLBACK=procedure_name]
;				 [, CT_FILE=color_table_file]
; INPUTS:
;       None.
; KEYWORDS:
;   PARENT: Set this keyword to the parent widget id in witch you want to place this compound widget.
;	CT_FILE: If this keyword is set, the file by the given name is used instead of the file
;		colors1.tbl in the IDL directory. This allows multiple IDL users to have their own
;		color table file.
;	NCOLORS: Number of colors to use. Use color indices from BOTTOM to the smaller of
;		!D.TABLE_SIZE-1 and NCOLORS-1.
;		Default = !D.TABLE_SIZE = all available colors.
;	BOTTOM: First color index to use. Use color indices from BOTTOM to BOTTOM+NCOLORS-1. Default = 0.
;	USE_CURRENT: Set this keyword to use the current color tables, regardless of the contents of the
;		COLORS common block.
;	BLOCK: Set this keyword to have XMANAGER block when this application is registered.
;		By default the Xmanager keyword NO_BLOCK is set to 1 to provide access to the command line 
;       if active command line processing is available.
;       Note that setting BLOCK for this application will cause all widget applications to block,
;		not only this application. For more information see the NO_BLOCK keyword to XMANAGER.
;	GROUP: Set this keyword to the widget ID of the widget that calls caviar_xloadct. 
;       When this ID is specified, a death of the caller results in a death of caviar_xloadct.
;   MODAL: If set, then XLOADCT runs in "modal" mode, meaning that all other widgets are blocked
;       until the user quits XLOADCT. A group leader must be specified (via the GROUP keyword)
;       for the MODAL keyword to have any effect.
;	UPDATECALLBACK: Set this keyword to a string containing the name of a user-supplied procedure
;       that will be called when the color table is updated by XLOADCT. The procedure may optionally
;       accept a keyword called DATA, which will be automatically set to the value specified by the 
;       optional UPDATECBDATA keyword.
;   UPDATECBDATA: Set this keyword to a value of any type. It will be passed via the DATA keyword
;       to the user-supplied procedure specified via the UPDATECALLBACK keyword, if any. If the
;       UPDATECBDATA keyword is not set the value accepted by the DATA keyword to the procedure
;       specified by UPDATECALLBACK will be undefined.
;
; OUTPUTS:
;       None.
; COMMON BLOCKS:
;       COLORS, 				R_orig, G_orig, B_orig, R_curr, G_curr, B_curr
;		SAVED_COLORS, 			R_save, G_save, B_save
;		CT_PARAMS, 				cbot, filename, ncolors, bot, top, gamma, chop, lock, use_values
;		CT_RAMP_PARAMS, 		ct_draw_xsize, ct_draw_ysize, ct_draw_win
;		TFUN_PARAMS, 			color, cps_x, cps_y, tfun_psave, pnt
;		CAVIAR_XLOADCT_COM, 	updt_callback, p_updt_cb_data
;		LEVELS_WIDGETS_ID, 		bot_slider_ID, top_slider_ID, g_slider_ID
;		XLOADCT_BASES_AND_TABS, base1, base2, tab1, tab2
;
; SIDE EFFECTS:
;       One of the predefined color maps may be loaded.
; RESTRICTIONS:
;       This routine uses the LOADCT user library procedure to do the actual work.
; MODIFICATION HISTORY:
;   24, August, 1990, Written by AB, RSI.
;   March 1, 1992  Mark Rivers
;		- Added Reverse Table to options menu.
;	7/92, DMS
;		- Added new color tables (allows more than 16).
;	9/92, ACY
;		- Added FILE keyword.
;	10/1/96, AB
;		- Removed the PICK_ONE keyword. It was broken for 4 years without anyone noticing, and the 
;		idea doesn't really fit XLOADCT anymore.
;   1/10/97, DJC 
;		- Fixed color bar display bug, and added "MODAL" keyword.
;	1/13/96, AB
;		- Improved the saving and restoring of the current graphics window to prevent other applications 
;		from drawing on this applications windows.
;   1/17/97, DJC
;		- Moved group_leader keyword from "XManager" to "WIDGET_BASE".
;   	- Added check to ignore "MODAL" keyword if a group leader is not specified.
;   8/20/98, ACY
;		- Added UPDATECALLBACK and UPDATECBDATA keywords.
;	2012, January, 30		L-E MEUNIER - IMCCE (OBSPM)
;		- Remove SILENT keyword. Now, XLOADCT is always silent.
;		- Cosmetic changes
;	2012, October, 30		L-E MEUNIER - IMCCE (OBSPM)
;		x Make sliders vertical
;		- Change levels options label
;	2013, October, 24		L-E MEUNIER - IMCCE (OBSPM)
;		- Change widget behavior when closing/reopening the widget:
;		  Now, xloadct starts using the previous color table parameters.
;---------------------------------------------------------------------------------------------------

PRO xloadct3, PARENT=base, BLOCK=block, GROUP_LEADER=group, MODAL=modal, $
			  CT_FILE=ct_file, $
			  NCOLORS=nc, $
			  BOTTOM=bottom, $
			  USE_CURRENT=use_current, $
			  UPDATECALLBACK=updt_cb_name, UPDATECBDATA=updt_cb_data
					
	COMMON COLORS, R_orig, G_orig, B_orig, R_curr, G_curr, B_curr
	COMMON SAVED_COLORS, R_save, G_save, B_save
	COMMON CT_PARAMS, cbot, filename, ncolors, bot, top, gamma, chop, lock, use_values
	COMMON CT_RAMP_PARAMS, ctDraw_xsize, ctDraw_ysize, ctDraw_win
	COMMON TFUN_PARAMS, color, cps_x, cps_y, tfun_psave, pnt, tfun_draw_win
	COMMON XLOADCT3_COM, updt_callback, p_updt_cb_data, updt_cb_str
	
	COMMON LEVELS_WIDGETS_ID, wLevelsSGroup
	
		
	cur_win = !D.WINDOW						;Get index of the current window
	
	; Test if the widget have already be launched
	IF XRegistered('xloadct3') NE 0 THEN RETURN
	
	; Test on the keyword to set specific procedure to call after loading a new color table 
	; (ex: re-display the image) and pass to this procedure the optionnal data set in updt_cb_data.
	updt_callback = KEYWORD_SET(updt_cb_name) ? updt_cb_name : ''
	p_updt_cb_data = KEYWORD_SET(updt_cb_data) ? PTR_NEW(updt_cb_data) : PTR_NEW()
    updt_cb_str = KEYWORD_SET(updt_cb_string) ? updt_cb_string : ''
    
	; Test on personal color table file keyword ('CT_FILE')
	ct_filename = KEYWORD_SET(ct_file) ? ct_file : ''
								 
	
	;___________________________________________________________________________                             
	;###########################################################################
	; Initialisation of default parameters
	IF top EQ !NULL        THEN top = 255			;Default value of the color level curve high threshold 
	IF bot EQ !NULL        THEN bot = 0				;Default value of the color level curve low threshold 
	IF gamma EQ !NULL      THEN gamma = 1.0			;Default value of the gamma (contrast factor)
	IF lock EQ !NULL       THEN lock = 0			;Define if sliders are independant (lock=0) or gang (lock=1)
	IF chop EQ !NULL       THEN chop = 0			;Define if values higher than vtop are saturated (chop=0) or put to 0 (chop=1)
	IF use_values EQ !NULL THEN use_values = 0
	;IF NOT KEYWORD_SET(nc) THEN nc = top+1
	
	lvlParams={VALUES: [top,bot,gamma], STEP: [1,1,0.1], $
			 MIN: [0,0,0.0], MAX: [255,255,2.0], $
			 TEXT: ["Top = ", "Bottom = ", "Gamma = "], $
			 FORMAT: ['(I-3)', '(I-3)', '(G0)'] $
			}
	
	ctDraw_xsize = 256          ;Width of the color table ramp (pixel)
	ctDraw_ysize = 25          	;Height of the color table ramp (pixel)
	ctList_ysize = 17			;Height of the color table list (character)
	lvlSliders_size = 200
	
	
	ct_names = 0							;Variables containing color table names
	LOADCT, GET_NAMES=ct_names, FILE=file	;Get table names
	;___________________________________________________________________________
	;###########################################################################
	;# Define the widget base and subwidgets tree
	IF ~KEYWORD_SET(base) THEN BEGIN
		title = "Color table & Levels"
		IF N_ELEMENTS(group) GT 0 $
		THEN base = WIDGET_BASE(TITLE=title, /COLUMN, GROUP_LEADER=group, /FLOATING, MODAL=KEYWORD_SET(modal)) $
		ELSE base = WIDGET_BASE(TITLE=title, /COLUMN)
	ENDIF
	
	; Setting the managed attribute indicates our intention to put this app under the control of
	; XMANAGER, and prevents our draw widgets from becoming candidates for becoming the default
	; window on WSET, -1. XMANAGER sets this, but doing it here prevents our own WSETs at startup
	; from having that problem.
	WIDGET_CONTROL, /MANAGED, base
		
	extraBase = WIDGET_BASE(base, /ROW, XPAD=0, YPAD=0, SPACE=0)
	
	
	;wCTList = WIDGET_LIST(extraBase, UNAME="LIST", VALUE=ct_names, YSIZE=ctList_ysize)
	
	wBase = WIDGET_BASE(extraBase, /ALIGN_CENTER, /COLUMN)
		;wCTdlist = WIDGET_DROPLIST(wBase, UNAME="DLIST", VALUE=ct_names);, YSIZE=ctList_ysize)
		wCTDraw = WIDGET_DRAW(wBase, XSIZE=ctDraw_xsize, YSIZE=ctDraw_ysize, /FRAME, RETAIN=2)
		wCTOptBase = WIDGET_BASE(wBase, /ROW, /ALIGN_CENTER, XPAD=0, YPAD=0, SPACE=0)
			wCTReverseBttn = WIDGET_BUTTON(wCTOptBase, UNAME="REVERSE", $
							VALUE='Reverse', /NO_RELEASE, $
							TOOLTIP="Reverse Current Table")
			wCTReplaceBttn = WIDGET_BUTTON(wCTOptBase, UNAME="REPLACE", $
							VALUE='Save As Original Table', /NO_RELEASE, $
							TOOLTIP="Replace Original Table")
			wCTRestoreBttn = WIDGET_BUTTON(wCTOptBase, UNAME="RESTORE", $
							VALUE='Restore', /NO_RELEASE, $
							TOOLTIP="Restore Original Table")
								
										
		wTabs = WIDGET_TAB(wBase, UNAME='TABS')
			
			wCTbase = WIDGET_BASE(wTabs, TITLE='Color Table', /COLUMN)
				wCTList = WIDGET_LIST(wCTbase, UNAME="LIST", VALUE=ct_names, YSIZE=ctList_ysize)
			
			;----------
			wLevelsBase = WIDGET_BASE(wTabs, TITLE='Levels', /COLUMN)
			
				wLevelsSGroup = CW_SGROUP(wLevelsBase, 3, UNAME="LEVELS SLIDERS", $
						TEXT=lvlParams.TEXT, FORMAT=lvlParams.FORMAT, $
						VAL=lvlParams.VALUES, $
						MIN=lvlParams.MIN, MAX=lvlParams.MAX, STEP=lvlParams.STEP, $
						X_SLIDER_SIZE=220, X_LABEL_SIZE=150, Y_LABEL_SIZE=16, $
						/LABEL_TOP, XPAD=0, YPAD=0, SPACE=0)
			
				;wLbl = WIDGET_LABEL(wLevelsBase, VALUE=' ')								
				wLevelsOptLbl = WIDGET_LABEL(wLevelsBase, VALUE="Options")
				wLevelsOptBase = WIDGET_BASE(wLevelsBase, /COLUMN, /FRAME, /ALIGN_CENTER, SPACE=0, YPAD=0)
					; Option to "link" top and bottom sliders movement
					wSlidersOptBGroup = CW_BGROUP(wLevelsOptBase, UNAME='BIND_SLIDERS', $
							"Bind Top/Bottom sliders", SET_VALUE=lock, /NONEXCLUSIVE)
				
					; Option to chose how values over "top" threshold are considered
					; "Clip": values == top ; "Chop": values == 0
					wTOPCUTOPTbgroup = CW_BGROUP(wLevelsOptBase, UNAME='TOP_METHOD', $
							LABEL_LEFT="Top cut:", ['Clip', 'Chop'], $
							SET_VALUE=chop, /ROW, /EXCLUSIVE, /NO_RELEASE)
			
					;
					wStretchdOptBGroup = CW_BGROUP(wLevelsOptBase, UNAME='STRETCH', $
							LABEL_LEFT="Stretch:", ['Indices', 'Intensity'], $
							SET_VALUE=use_values, /ROW, /EXCLUSIVE, /NO_RELEASE)
								
		
			;----------
			wTFunBase = WIDGET_BASE(wTabs, TITLE='Transfer function', /COLUMN, /BASE_ALIGN_CENTER)
				Tfun_draw_ID = WIDGET_DRAW(wTFunBase, UNAME="TFUN_DRAW", $
							XSIZE=246, YSIZE=246, /BUTTON_EVENTS, /MOTION_EVENTS)
				wTfunOptBase1 = WIDGET_BASE(wTFunBase, /ROW)
					wTfunOptLbl = WIDGET_LABEL(wTfunOptBase1, VALUE="Control Points:")
					wTFunOpt1 = WIDGET_BUTTON(wTfunOptBase1, UNAME='CP_ADD', $
										XSIZE=70, VALUE='Add')
					wTFunOpt2 = WIDGET_BUTTON(wTfunOptBase1, UNAME='CP_RMV', $
										XSIZE=70, VALUE='Remove')
				
	
	
	; Create the widget (i.e. display it)		
	WIDGET_CONTROL, base, /REALIZE
	
	;___________________________________________________________________________
	;###########################################################################
	; Definition of the number of colors used
	cbot = KEYWORD_SET(bottom) ? bottom : 0
	IF cbot LT 0 || cbot GT 255 THEN MESSAGE, "Input keyword 'BOTTOM' must be in range 0-255"
	
	ncolors = KEYWORD_SET(nc) ? !D.TABLE_SIZE-cbot < ROUND(nc) : !D.TABLE_SIZE-cbot
	IF ncolors LE 0 THEN MESSAGE, "Input keyword 'COLORS' must be in a positive integer"
	
	; Set color table if R_orig, G_orig, B_orig have not been define:
	IF R_orig EQ !NULL || G_orig EQ !NULL || B_orig EQ !NULL THEN BEGIN
		LOADCT, 0, FILE=filename, NCOLORS=ncolors, BOTTOM=cbot, /SILENT
		TVLCT, R_orig, G_orig, B_orig, /GET
		R_save=R_orig & G_save=G_orig & B_save=B_orig
	ENDIF
	
	; Display color table in the corresponding widget
	WIDGET_CONTROL, wCTDraw, GET_VALUE=ctDraw_win
	xloadct3_setGAMMA
	;___________________________________________________________________________
	;###########################################################################
	; Set Tfun parameters:
	color = ncolors + cbot -1
	cps_x = [0, ncolors-1]
	cps_y = cps_x				;Create a linear ramp (i.e. transfer function)
	pnt = -1
	
	; Get Tfun_draw window ID
	WIDGET_CONTROL, Tfun_draw_ID, GET_VALUE=Tfun_draw_win	
	
	; Initialize the structure to save Tfun_draw plot properties (scaling & window)
	Tfun_psave = { TFUN_PSAVE, win:Tfun_draw_win, x:!X.S, y:!Y.S , xtype:!X.TYPE, ytype:!Y.TYPE, clip:!P.CLIP }
	
	; Active Tfun_draw window
	WSET, tfun_draw_win			;Initial graph
	
	; Save original Tfun_draw plot properties (scaling & window)
	xloadct3_Tfun_psave
	
	; Initialize new plot (axes, ...)
	PLOT, /NODATA, [0, ncolors-1], [0, ncolors-1], XSTYLE=3, YSTYLE=3, XMARGIN=[1,1], YMARGIN=[1,1], TICKLEN=-0.03
	
	; Restore original scaling & window
	xloadct3_Tfun_psave		
	;___________________________________________________________________________

	; Restore window active at the beggining of this procedure
	WSET, cur_win
	
	; Let XMANAGER takes control of widgets event processing
	XMANAGER, 'xloadct3', base, NO_BLOCK=(~KEYWORD_SET(BLOCK)), /JUST_REG
END
