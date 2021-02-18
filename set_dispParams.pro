; OPTIONNAL KEYWORDS:
;
; 	OBJ_STATE: set "0" to get object to not be displayed (default is "1" = displayed)
;
; 	OBJ_PSYM: 	3=Dot
;		  		4=Diamond 
;				6=square
;				8=UserDefine (use "USERSYM" procedure to set your own symbol - default is a circle)


PRO set_dispParams, dispParams, SET_DEFAULT=SET_DEFAULT, $
	CATSTARS_STATE=cs_state, CATSTARS_PSYM=cs_psym, CATSTARS_SYMSIZE=cs_symsize, CATSTARS_COLOR=cs_color, $
	IMGSTARS_STATE=is_state, IMGSTARS_PSYM=is_psym, IMGSTARS_SYMSIZE=is_symsize, IMGSTARS_COLOR=is_color, $
	LOCSTARS_STATE=ls_state, LOCSTARS_PSYM=ls_psym, LOCSTARS_SYMSIZE=ls_symsize, LOCSTARS_COLOR=ls_color, $
	PLANETS_STATE=p_state,   PLANETS_PSYM=p_psym,   PLANETS_SYMSIZE=p_symsize,   PLANETS_COLOR=p_color, $
	RINGS_STATE=r_state,     RINGS_PSYM=r_psym,     RINGS_SYMSIZE=r_symsize,     RINGS_COLOR=r_color, $
	ZOOMFACTOR=zoomFactor, INTERPOLATE=interp
	
	
	IF KEYWORD_SET(SET_DEFAULT) THEN BEGIN
		
		A = FINDGEN(49) * (!PI*2/48.)
		USERSYM, COS(A), SIN(A)			;create circle symbol
		
		;# Stars display parameters:
		s_state	= 1
		s_psym 	= 4 					;user define symbol
		s_symsize 	= 3
		s_color	= 'LIGHT BLUE' 
		s_selColor = 'RED'				;-> then lightSkyBlue after fine repointing
				
		;# Image stars display parameters:
		is_state	= 1
		is_psym		= 4 				;diamond
		is_symsize  = 2			
		is_color 	= 'DARK VIOLET'
		
					
		
		;# Planets display parameters:
		p_state		= 1
		p_psym 		= 6					;square symbol
		p_symsize 	= 2
		p_color 	= 'DARK YELLOW 2'
		p_selColor	= 'RED'
		p_calColor	= 'GREEN'
						
		
		
		;# Rings display parameters:
		r_state		= 1
		r_psym 		= 3 				;point symbol
		r_symsize	= 0.5
		r_color 	= 'MAGENTA'
		
		
		;# Zoom parameters
		zoomFactor  = 1
		interpolate = 0
		
	ENDIF
	
	catstars_dp = {state: s_state, psym: s_psym, symsize: s_symsize, $
				   color: s_color, sel_color: s_selColor}
	imgstars_dp = {state: is_state, psym: is_psym, symsize: is_symsize, $
				   color: is_color}
		
	planets_dp	= {state: p_state, psym:  p_psym, symsize: p_symsize, $
				   color: p_color, sel_color: p_selColor, calc_color: p_calColor}
		
	rings_dp 	= {state: r_state, psym:  r_psym, symsize: r_symsize, $
				   color: r_color}
	
	
	dispParams = {catstars:	catstars_dp, $
				  imgstars:	imgstars_dp, $
				  planets: 	planets_dp, $
				  rings: 	rings_dp $
				 }
END
