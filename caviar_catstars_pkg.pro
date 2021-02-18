;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;  List of functions and procedures of this file:
;		PRO			caviar_catstars_display
;		PRO			caviar_catstars_rmvDoubles
;		FUNCTION	caviar_catstars_ctlgaccess
;		PRO			caviar_catstars_load
;		PRO			caviar_catstars_gui_updtCatalogs
;		FUNCTION	caviar_catstars_gui_updtTable
;		PRO			caviar_catstars_gui_event
;		PRO			caviar_catstars_gui
;-------------------------------------------------------------------------------

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_CATSTARS_DISPLAY
; PURPOSE: 
;-------------------------------------------------------------------------------
PRO caviar_catstars_display, catstars, dispParams, nl, offset, ZOOMFACTOR=zf
	
	; Test inputs
	IF dispParams.state NE 1 THEN RETURN
	
	n = N_ELEMENTS(catstars)
	IF n EQ 0 THEN RETURN
	
	IF ~KEYWORD_SET(zf) THEN zf=1
	
	; Get stars position, display parameters and informations:
	x = DBLARR(n)
	y = DBLARR(n)
	colors = STRARR(n)
	colorsIndex	= color([dispParams.sel_color,dispParams.color], /SILENT)	;Be carefull, color function change the color table!
	names = STRARR(n)
	mags  = STRARR(n)
	FOR i=0, n-1 DO BEGIN
		cati = (catstars[i])
		x[i] = zf * ( cati.xcoord ) - offset[0]
		y[i] = zf * ( (nl-1) - cati.ycoord ) - offset[1]
		IF haveTag(cati, "MATCHED") && cati.MATCHED EQ 1 $
		THEN colors[i] = colorsIndex[0] $
		ELSE colors[i] = colorsIndex[1]
		names[i] = cati.name
		mags[i]	 = STRING(cati.mag, '(D0.2)')
	ENDFOR
	psym    = dispParams.psym
	symsize = dispParams.symsize
	
	PLOTS, x, y, /DEVICE, COLOR=colors, PSYM=psym, SYMSIZE=symsize
	XYOUTS, x+10, y, names, /DEVICE, COLOR=colors
	XYOUTS, x+10, y-10, mags, /DEVICE, COLOR=colors
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_CATSTARS_RMVDOUBLES
;
; PURPOSE:
;	Compare two list of stars to look for doubles. 
;	If any duplicated stars are found, the one from stars1 is kept and the one 
;	from stars2 is remove unless the "S2_MAGRANGE" keyword is set and the  
;	magnitude of the star is inside the stars2 prefered magnitude range. In this
;	case the star of stars2 is kept and the star of stars1 is remove.
;
; CALLING SEQUENCE:
;	caviar_catstars_rmvDoubles, stars1, stars2 $
;							[, THRESHOLD=threshold][, S2_MAGRANGE=s2magrange]
;
; INPUTS:
;	stars1/stars2: List of n1/n2 structure elements with tags "RA", "dec", and "mag". 
;					   
; KEYWORDS:
;	S2_MAGRANGE: 2-Elements array defining the prefered magnitude range for 
;		stars2, while stars1 is considered as default choice.
;	THRESHOLD: Scalar value. Minimum seperation distance of two unique stars.
; OUTPUT:
;	Update the stars1/stars2 input variables.
;
; MODIFICATIONS:
;	2012, February		MEUNIER L-E				OBSPM>IMCCE
;		- Written based uppon old "unique_stars" function of CaVIaR
;	2013, January		MEUNIER L-E				OBSPM>IMCCE
;		- Add 'THRESHOLD' keyword for minimum seperation distance of two unique stars
;	2013, February		MEUNIER L-E				OBSPM>IMCCE
;		- Correct a bug with threshold that generates uneffective of removing doubles.
;		- Change calculation method for seperation angle -> use spherical trigonometry.
;		- Remove FOR-loop to matrix calculation to improve performance.
;-------------------------------------------------------------------------------
PRO caviar_catstars_rmvDoubles, stars1, stars2, THRESHOLD=threshold, $
								   S2_MAGRANGE=s2magrange
	
	IF NOT KEYWORD_SET(threshold) THEN threshold = 6.0D-6 
	IF KEYWORD_SET(s2magrange) THEN BEGIN
		IF N_ELEMENTS(s2magrange) NE 2 THEN BEGIN
			s2magrange=[-99, 99]
			MESSAGE, '"S2_RANGE" is not a 2-elements array. This keyword will be ignore', /CONTINUE
		ENDIF
	ENDIF

	
	n1 = N_ELEMENTS(stars1.name)
	n2 = N_ELEMENTS(stars2.name)
	IF n1 EQ 0 || n2 EQ 0 THEN RETURN
	
	name1 = stars1.name & name2 = stars2.name
	mag1  = stars1.mag  & mag2  = stars2.mag
	ra1   = stars1.ra   & ra2   = stars2.ra
	dec1  = stars1.dec  & dec2  = stars2.dec
	
	;***************************************************************************
	; Compute separation angle between stars from both catalogs:
	;***************************************************************************
	mra1 = ra1 # MAKE_ARRAY(1, n2, VALUE=1D)
	mra2 = MAKE_ARRAY(n1, VALUE=1D) # TRANSPOSE(ra2)
	dra = mra1-mra2
	
	mdec1 = dec1 # MAKE_ARRAY(1, n2, VALUE=1D)
	mdec2 = MAKE_ARRAY(n1, VALUE=1D) # TRANSPOSE(dec2)
	ddec = mdec1-mdec2
	
	sep = ACOS(cos(dra)*cos(ddec))
	
	;***************************************************************************
	; Indentify stars in double:
	;***************************************************************************
	index = WHERE(sep LT threshold, count)
	
	stars1_toRmv = LIST()
	stars2_toRmv = LIST()
		
	IF (SIZE(sep))[0] EQ 1  THEN BEGIN
		IF count GT 0 THEN BEGIN
			i = index
			j = 0
			IF mag2[j] GE s2magrange[0] && mag2[j] LE s2magrange[1] $
			THEN stars1_toRmv.add, i $
			ELSE stars2_toRmv.add, j
		ENDIF
	ENDIF ELSE BEGIN
		IF count GT 0 THEN index = ARRAY_INDICES(sep, index) ELSE RETURN
		FOR k=0, count-1 DO BEGIN
			i = index[0,k]
			j = index[1,k]
			IF mag2[j] GE s2magrange[0] && mag2[j] LE s2magrange[1] $
			THEN stars1_toRmv.add, i $
			ELSE stars2_toRmv.add, j
		ENDFOR
	ENDELSE
	
	;***************************************************************************
	; Remove stars in double:
	;***************************************************************************
	indexTOkeep = MAKE_ARRAY(n1, VALUE=1D)
	indexTOkeep[stars1_toRmv.ToArray()] = 0
	indexTOkeep = WHERE(indexTOkeep EQ 1)
	stars1 = {name:name1[indexTOkeep], mag:mag1[indexTOkeep], $
			  ra:ra1[indexTOkeep], dec:dec1[indexTOkeep]}
	
	indexTOkeep = MAKE_ARRAY(n2, VALUE=1D)
	indexTOkeep[stars2_toRmv.ToArray()] = 0
	indexTOkeep = WHERE(indexTOkeep EQ 1)
	stars2 = {name:name2[indexTOkeep], mag:mag2[indexTOkeep], $
			  ra:ra2[indexTOkeep], dec:dec2[indexTOkeep]}
	RETURN
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_catstars_ctlgaccess
; PURPOSE: Access stars catalogues routines to retrieve stars in the image field
;		of view and create the stars structure variable.
; INPUTS:
;	strcat: Scalar string. Name of the catalogue to access.
;	epoch: Scalar floating point variable. Image mid-time in years.
;	pRA/pDEC: Scalar floating point variables. Camera pointing vector 
;		coordinates in radian.
;	fovsearch: Scalar floating point variable. Search field of view in degree.
;	magmin/magmax: Scalar floating point variables. Magnitude search range.
; OUTPUT:
;	A structure with the 'NAME', 'MAG', 'RA' and 'DEC' fields, which contains
;	an array of type string, double, double and double respectively.
;	RA and DEC are given in radian.
; MODIFICATIONS:
;       2017, May         Nick Cooper                            QMUL 
;               - Added UCAC5 catalogue
;-------------------------------------------------------------------------------
FUNCTION caviar_catstars_ctlgaccess, strcat, epoch, pRA, pDEC, fovsearch, magmin, magmax

;njc Added:
; Convert 0.0 seconds past J2000 to  UTC decimal years:
        cspice_timout, 0.d0, 'YYYY.#########::UTC', 14, J2000y
	
	name = LIST() &	mag = LIST() & ra = LIST() & dec = LIST()
			
	CASE strcat OF
		'TYCHO2': BEGIN
			strFOVR  = STRCOMPRESS(STRING(fovsearch*3600D/2D),/REMOVE_ALL)
			strRA	 = STRCOMPRESS(STRING(pRA*180.0D/!DPI),/REMOVE_ALL)
			strDEC   = STRCOMPRESS(STRING(pDEC*180.0D/!DPI),/REMOVE_ALL)
			strEpoch = STRCOMPRESS(STRING(epoch),/REMOVE_ALL)

;--------------------------------------------------------------------------------------------------------------------------------
;njc TYCHO2 For Vizier web-interface:
;--------------------------------------------------------------------------------------------------------------------------------
                         strMagmin = STRCOMPRESS(STRING(magmin),/REMOVE_ALL)
                         strMagmax = STRCOMPRESS(STRING(magmax),/REMOVE_ALL)
;njc Using -c.rd option so use half fovsearch (i.e.radius):
                         strFOV    = STRCOMPRESS(STRING(0.5*fovsearch),/REMOVE_ALL)

;If the DEC value is positive, it must have a trailing '+' :
                         IF STRMID(strDEC,0,1) ne '-' THEN strDEC='+'+strDEC

;Vizquery provides positions and proper motions at J2000. The proper motion correction must then be applied for the image epoch:
;RA and DEC in ICRF
                         cmd = 'vizquery -mime=text -out=TYC2,_RA,_DE,VTmag,pmRA,pmDE -source=I/259/tyc2 -out.form=mini VTmag="<'+strMagmax+'" -c='+strRA+strDEC+' -c.rd='+strFOV+''

                         SPAWN, cmd, out
                         print,out              ;Output from Vizier search

                         IF n_elements(out) EQ 1 THEN BEGIN
                           PRINT, FORMAT='(/A)', ">>>>> No TYCHO2 stars found. Check internet connection."
                           RETURN, -1
                         ENDIF
                         FOR i=31, N_ELEMENTS(out)-4 DO BEGIN					;Skip header and footer
                              values = STRSPLIT(out[i], ' ', /EXTRACT)
                              IF values[0] ne '-' or values[0] ne '' then BEGIN
                                 magi  = DOUBLE(values[3])
                                 IF (magi GE magmin) && (magi LE magmax) THEN BEGIN
                                        name.add, 'TYCHO2 '+STRING(values[0])
                                        mag.add, magi
                                        ras =DOUBLE(values[1])                                  ;deg
                                        des =DOUBLE(values[2])                                  ;deg
                                        IF (n_elements(values) gt 4) THEN BEGIN
   			                  pmra=DOUBLE(values[4])/(3600.d0*1000.d0)		;convert mas/yr to deg/yr
                                          pmde=DOUBLE(values[5])/(3600.d0*1000.d0)		;convert mas/yr to deg/yr
                                        ENDIF ELSE BEGIN					
                                          pmra = 0.d0						;no proper motions in table
                                          pmde = 0.d0
                                        ENDELSE
;Correct J2000 position to image epoch using proper motions):
	                                dt  = DOUBLE(strepoch) - DOUBLE(J2000y)
                                        ra.add,  (ras + pmra*dt)*!DPI/180.0D         		; Convert from deg to radian
                                        dec.add, (des + pmde*dt)*!DPI/180.0D			; Convert from deg to radian
                                 ENDIF
                              ENDIF
                        ENDFOR

;--------------------------------------------------------------------------------------------------------------------------------
		END
		
;njc Added 03May17 :
                'UCAC5': BEGIN

;njc: Using -c.rd option so use half fovsearch (i.e.radius):
                        strFOV    = STRCOMPRESS(STRING(0.5*fovsearch),/REMOVE_ALL)
                        strRA     = STRCOMPRESS(STRING(pRA*180.0D/!DPI),/REMOVE_ALL)
                        strDEC    = STRCOMPRESS(STRING(pDEC*180.0D/!DPI),/REMOVE_ALL)
                        strEpoch  = STRCOMPRESS(STRING(epoch),/REMOVE_ALL)
                        strMagmin = STRCOMPRESS(STRING(magmin),/REMOVE_ALL)
                        strMagmax = STRCOMPRESS(STRING(magmax),/REMOVE_ALL)
;--------------------------------------------------------------------------------------------------------------------------------
;njc UCAC5 For Vizier web-interface:
;--------------------------------------------------------------------------------------------------------------------------------
;If the DEC value is positive, it must have a trailing '+' :
                         IF STRMID(strDEC,0,1) ne '-' THEN strDEC='+'+strDEC

;Vizquery provides positions and proper motions at J2000. The proper motion correction must then be applied for the image epoch:
                         cmd = 'vizquery -mime=text -source=I/340 f.mag="<'+strMagmax+'" -out.add=_ID=SrcIDgaia -out.form=mini -c='+strRA+strDEC+' -c.rd='+strFOV+''
                         SPAWN, cmd, out
                         print,out		;Output from Vizier search

                         IF n_elements(out) EQ 1 THEN BEGIN
                           PRINT, FORMAT='(/A)', ">>>>> No UCAC5 stars found. Check internet connection."
                           RETURN, -1
                         ENDIF

;njc Skip the Vizier header and footer:
                         FOR i=32, N_ELEMENTS(out)-3 DO BEGIN
                                values = STRSPLIT(out[i], ' ', /EXTRACT)
                                magi  = DOUBLE(values[15])
                                IF (magi GE magmin) && (magi LE magmax) THEN BEGIN
                                        name.add, 'UCAC5 '+STRING(values[2])
                                        mag.add, magi
                                        ras =DOUBLE(values[0])                                  ;deg
                                        des =DOUBLE(values[1])                                  ;deg
                                        pmra=DOUBLE(values[10])/(3600.d0*1000.d0)                ;convert mas/yr to deg/yr
                                        pmde=DOUBLE(values[12])/(3600.d0*1000.d0)                ;convert mas/yr to deg/yr

;Correct J2000 position to image epoch using proper motions):
                                        dt  = DOUBLE(strepoch) - DOUBLE(J2000y)
                                        ra.add,  (ras + pmra*dt)*!DPI/180.0D                    ; Convert from deg to radian
                                        dec.add, (des + pmde*dt)*!DPI/180.0D                    ; Convert from deg to radian
                                ENDIF
                        ENDFOR
;--------------------------------------------------------------------------------------------------------------------------------
                END
	
		ELSE: MESSAGE, "Wrong catalogue name."
	ENDCASE
	
	IF N_ELEMENTS(name) EQ 0 || N_ELEMENTS(mag) EQ 0 $
	|| N_ELEMENTS(ra) EQ 0   || N_ELEMENTS(dec) EQ 0 $
	THEN RETURN, {} $
	ELSE RETURN, {NAME:name.ToArray(), MAG:mag.ToArray(), RA:ra.ToArray(), DEC:dec.ToArray()}
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_CATSTARS_LOAD
;
; PROCEDURE CALLS:
;	caviar_catstars_rmvDouble
;	haveTag							in havetag.pro
;		
; MODIFICATIONS:
;	2012, March		written by L-E. MEUNIER	(IMCCE/OBSPM)
;	2012, August		L-E. MEUNIER (IMCCE/OBSPM)
;		- Create 'get_UCAC2_stars' & 'get_TYCHO2_stars' functions
;		- Add tags 'xcoord', 'ycoord', 'located', 'selected' to 'stars' structure
;	2013, february      L-E. MEUNIER (IMCCE/OBSPM)
;		- Improve performance by changing the 'stars' data type and structure:
;		- 'stars' stay a structure of arrays until the end of the routine.
;		- 'stars' are not store in 'catalogs' structure anymore.
;	2014, January		L-E. MEUNIER (IMCCE/OBSPM)
;		- Change name from 'caviar_getCatStart' to 'caviar_catstars_load'
;-------------------------------------------------------------------------------
PRO caviar_catstars_load, catalogs, fovsearch, THRESHOLD=threshold
	
	COMMON CAVIAR_DATA, image, catStars
	
	IF N_ELEMENTS(image) EQ 0 THEN MESSAGE, "Please load an image"

	IF ~haveTag(image, 'CMAT') || ~ISA(image.cmat, /ARRAY) $
	THEN MESSAGE, "Cannot load stars from catalogues without camera pointing matrix (cmat)."
	
	PRINT, FORMAT='(/A)', ">>>>> Start getting stars from catalogs"
;Using -c.rd option so use half fovsearch (i.e.radius):
	PRINT, 'Search field of view: ', STRING(0.5*fovsearch, '(G0)'), ' deg'
	
	;***************************************************************************
	; Convert ephemeris time (TDB seconds past J2000) to epoch (UTC decimal years):
	;***************************************************************************
	cspice_timout, image.ET, 'YYYY.#########::UTC', 14, ctime
	epoch = DOUBLE(ctime)
	
	
	cspice_timout, image.et, 'YYYY-MM-DD HR:MN:SC.#######::UTC',27, t1
	cspice_timout, image.et, 'YYYY-MM-DD HR:MN:SC.#######::TDB',27, t2
	cspice_et2utc, image.et, "ISOC", 9, t3
	print, t1
	print, t2
	print, t3
	;***************************************************************************
	; Get pointing in RA, DEC from cmat:
	;***************************************************************************
	rho_J2000 = image.CMAT#[0.0d0, 0.0d0, 1.0d0]	;rho_J2000 = cmat # rho_CAMERA
	cspice_recrad, rho_J2000, range, pRA, pdec
	IF pRA LT 0 THEN pRA+=2*!dpi
	
	;***************************************************************************
	; Get stars from selected catalogs:
	;***************************************************************************
	stars = LIST()
	FOREACH cat, catalogs DO BEGIN
		IF cat.state EQ 1 THEN BEGIN
			PRINT, "Getting stars from "+cat.name+" catalogue..."
			res = caviar_catstars_ctlgaccess(cat.NAME, epoch, pRA, pDEC, $
									fovsearch, cat.SRCHMAG_MIN, cat.SRCHMAG_MAX)
			IF ISA(res, 'STRUCT') THEN BEGIN
				stars.add, res
				PRINT, "... found "+STRING(N_ELEMENTS(res.name), FORMAT='(I0)')
			ENDIF ELSE PRINT, "... found 0"
		ENDIF		
	ENDFOREACH
	
	
	;***************************************************************************
	; Compare stars found in each catalogs to remove doubles.
	; The selection depends on the preference order and magnitude range.
	;***************************************************************************
	name = !NULL & mag = !NULL & ra = !NULL & dec = !NULL
	format = '("Comparing the ", I0, " stars of ", A, " catalogue ", ' $
			 	  +'"with the ", I0, " stars of ", A, " to remove duplicated stars...")'
	n = N_ELEMENTS(stars)
	FOR i=0, n-1 DO BEGIN
		stars1 = stars[i]
		n1 = N_ELEMENTS(stars1.name)
		IF n1 GT 0 THEN BEGIN
			FOR j=i+1, n-1 DO BEGIN
				stars2 = stars[j]
				n2 = N_ELEMENTS(stars2.name)
				IF n2 GT 0 THEN BEGIN
					PRINT, FORMAT=format, n1, (catalogs[i]).name, n2, (catalogs[j]).name
					caviar_catstars_rmvDoubles, stars1, stars2, THRESHOLD=threshold, $
						S2_MAGRANGE=[(catalogs[j]).prefmag_min, (catalogs[j]).prefmag_max]
				ENDIF
				stars[j] = stars2
			ENDFOR
			name = [name, stars1.name]
			mag  = [mag , stars1.mag ]
			ra   = [ra  , stars1.ra  ]
			dec  = [dec , stars1.dec ]
		ENDIF
	ENDFOR
	nstars = N_ELEMENTS(name)
	PRINT, "Found ", STRING(nstars, FORMAT='(I0)'), " unique stars."
	
	; Convert RA/dec to sample/line image coordinates 
	radec2slcoord, ra, dec, sample, line, CORRECT_STELAB=image.VOBS_STARS
	
	;***************************************************************************
	; Create 'catStars' structures list:
	;***************************************************************************
	catStars = LIST()
	FOR k=0, nstars-1 $
		DO catStars.add, {NAME:name[k], MAG:mag[k], RA:ra[k], DEC:dec[k], $
						XCOORD:sample[k], YCOORD:line[k], MATCHED:0, FITTED:0}
	
	print, catStars
	IF N_ELEMENTS(catStars) GT 0 $
	THEN PRINT, "<<<<< Stars from catalogs have been loaded succesfully."
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_CATSTARS_UPDTCATALOGS
; PRURPOSE:
; 	Update values of catalogs structure list from table values. It is use when 
; 	the table values are modified by the user.
;-------------------------------------------------------------------------------
PRO caviar_catstars_gui_updtCatalogs, tblValues, catalogs

	FOR i=0, N_ELEMENTS(catalogs)-1 DO BEGIN
		cati = catalogs[i]
		index = WHERE(tblValues.name EQ cati.name)
		cati.srchmag_min = DOUBLE( (tblValues[index]).srchmag_min )
		cati.srchmag_max = DOUBLE( (tblValues[index]).srchmag_max )
		cati.prefmag_min = DOUBLE( (tblValues[index]).prefmag_min )
		cati.prefmag_max = DOUBLE( (tblValues[index]).prefmag_max )
		catalogs[i] = cati
	ENDFOR

	RETURN
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_CATSTARS_UPDTTABLE 					        
; PURPOSE:
;	Create/Update the search catalogs table (parameters and values) from    
;	catalogs structure list.                                                 
;-------------------------------------------------------------------------------
FUNCTION caviar_catstars_gui_updtTable, catalogs
	
	; Update table values and editable state
	ncats = N_ELEMENTS(catalogs)
	tbli = {state:'', name: '', srchmag_min: '', srchmag_max: '', prefmag_min: '', prefmag_max: ''}
	values = REPLICATE(tbli, ncats)
	ntags = N_TAGS(tbli)
	editstate = INTARR(ntags, ncats)
	FOR i=0, ncats-1 DO BEGIN
		cati = catalogs[i]
		tbli.state		 = cati.state EQ 1 ? STRING(i+1,'( I0)') : ''
		tbli.name 		 = cati.name
		tbli.srchmag_min = STRING(cati.srchmag_min,'(D0.1)')
		tbli.srchmag_max = STRING(cati.srchmag_max,'(D0.1)')
		tbli.prefmag_min = STRING(cati.prefmag_min,'(D0.1)')
		tbli.prefmag_max = STRING(cati.prefmag_max,'(D0.1)')
		values[i] = tbli
		
		editstate[2:ntags-1,i] = (catalogs[i]).state
	ENDFOR	
	
	bgcolor = (fgcolor = BYTARR(3, ntags, ncats, /NOZERO))
	onesLine = MAKE_ARRAY(ntags, VALUE=1)
	FOR i=0, ncats-1 DO BEGIN
		fgcolor[*,*,i] = onesLine ## !COLOR.black
		IF (catalogs[i]).STATE EQ 1 THEN BEGIN
			bgcolor[*,*,i] = onesLine ## !COLOR.white
			fgcolor[*,0,i] = !COLOR.blue
		ENDIF ELSE BEGIN
			bgcolor[*,*,i] = onesLine ## !COLOR.light_gray
		ENDELSE
		bgcolor[*,0,i] = (bgcolor[*,1,i] = !COLOR.light_gray)
	ENDFOR
		
	RETURN, {values: values, editstate: editstate, bgcolor: bgcolor, fgcolor: fgcolor}
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_CATSTARS_GUI_INIT
; PURPOSE: Set default values/labels/parameters/etc. of the GUI.
;
;njc This routine is called from caviar_gui.pro when a new image is loaded from 
;njc the 'Load' drop-down in the main gui.
;-------------------------------------------------------------------------------
PRO caviar_catstars_gui_init
	COMMON CAVIAR_DATA, image, catstars
        COMMON CAVIAR_GUI, wCAVIARtlb, wFITSATLIMBbase, wRINGSLOADbase, wCATSTARSLOADbase
        COMMON CAVIAR_SF, SFfactor

	WIDGET_CONTROL, WIDGET_INFO(wCATSTARSLOADbase, /CHILD), GET_UVALUE=pstate
	
	nstars = STRING(N_ELEMENTS(catstars), '(I0)')
	WIDGET_CONTROL, (*pstate).wGetStarsLblID, SET_VALUE=" Found: "+nstars
	
	; Set search field of view (in degrees):
	SFfactor = 1
	fovimg = image.FOVIMG
	fovsearch = SFfactor*fovimg
	format = '("x", G-0.3, A)'
        sfovimg = STRING(0.5*fovimg  , '°', FORMAT=format)
	
	WIDGET_CONTROL, (*pstate).wSFField, SET_VALUE=SFfactor
	WIDGET_CONTROL, (*pstate).wSFLabel, SET_VALUE=sfovimg

END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_CATSTARS_GUI_EVENT
; PURPOSE: Manage events from the widget.
;-------------------------------------------------------------------------------
PRO caviar_catstars_gui_event, event
	
	COMMON CAVIAR_DATA, image, catstars
	COMMON CAVIAR_PARAMS, dispParams, imgDraw, dispProName, catalogs, find_imgStars_params
	COMMON CAVIAR_GUI, wCAVIARtlb, wFITSATLIMBbase, wRINGSLOADbase, wCATSTARSLOADbase
;njc added:
        COMMON CAVIAR_SF, SFfactor

	CATCH, Error_status
	IF Error_status NE 0 THEN BEGIN
		res = DIALOG_MESSAGE(!ERROR_STATE.MSG, DIALOG_PARENT=event.HANDLER, /CENTER)
		RETURN	
	ENDIF

	; Get state from the first child of the compound widget root:
	WIDGET_CONTROL, event.HANDLER, GET_UVALUE=pstate

	uname = WIDGET_INFO(event.ID, /UNAME)
	CASE uname OF
		; Change size of search field

		"SFFACTOR": BEGIN
                     (*pstate).fovsearch=event.VALUE*image.fovimg	;event.value corresponds to SFfactor
;njc added: 
                     SFfactor=event.VALUE
                     print,'here s3a',(*pstate).fovsearch,event.VALUE,image.fovimg
                END

		; Change catalogs selected state or table values
		"CATALOGS_PARAMS": BEGIN
			IF event.TYPE EQ 4 THEN BEGIN
				IF event.SEL_TOP EQ event.SEL_BOTTOM && event.SEL_TOP NE -1 $
				&& event.SEL_LEFT EQ event.SEL_RIGHT $
				&& (event.SEL_LEFT EQ 0 || event.SEL_LEFT EQ 1) $
				THEN BEGIN
					cati = catalogs[event.SEL_TOP]
					cati.STATE = 1-cati.STATE
					catalogs[event.SEL_TOP] = cati
					
					; Sort catalogs by state: state={1,0} -> {beginning,end}
					FOR i=0, N_ELEMENTS(catalogs)-1 $
					DO IF (catalogs[i]).STATE EQ 0 THEN catalogs.ADD, catalogs.remove(i)
						
					; Update table values, editable state and background color
					tbl = caviar_catstars_gui_updtTable(catalogs)
					WIDGET_CONTROL, event.ID, SET_VALUE=tbl.VALUES, $
							EDITABLE=tbl.EDITSTATE, BACKGROUND_COLOR=tbl.BGCOLOR, $
							FOREGROUND_COLOR=tbl.FGCOLOR
				ENDIF
			ENDIF ELSE BEGIN
				WIDGET_CONTROL, event.ID, GET_VALUE=tblVals
				caviar_catstars_gui_updtCatalogs, tblVals, catalogs
			ENDELSE
		END
		
		; Start getting stars from catalogs and remove doubles
		"GET_STARS": BEGIN
			WIDGET_CONTROL, /HOURGLASS
			ncat = 0
			FOREACH cat, catalogs DO IF cat.STATE EQ 1 THEN ncat++
			IF ncat EQ 0 THEN MESSAGE, 'At least one catalog must be selected !'

                        if SFfactor ne 1 then begin
			  caviar_catstars_load, catalogs, (*pstate).fovsearch, THRESHOLD=image.fovpix
                        endif else begin  ;new image
                          caviar_catstars_load, catalogs, image.fovimg, THRESHOLD=image.fovpix
                        endelse

			nstars = STRING(N_ELEMENTS(catstars), '(I0)')
			WIDGET_CONTROL, (*pstate).wGetStarsLblID, SET_VALUE=" Found: "+nstars
			
			caviar_updtCoords, catstars, image.vobs_stars
			caviar_display

		END
		
		"CLOSE": BEGIN
			PTR_FREE, pstate
			WIDGET_CONTROL, event.TOP, /DESTROY
		END
		
		ELSE:
	ENDCASE
			
	RETURN
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;	caviar_catstars_gui
;
; PURPOSE:
;	Create a graphical interface for searching stars in catalogs. This interface 
;	allow to change the search field, magnitudes and set magnitudes preference 
;	in case of star found in several catalogs.
;
; CALLING SEQUENCE:
;	caviar_catstars_gui[, PARENT=baseID]
;
; INPUTS: None. 
;			
; OPTIONAL KEYWORDS:
;	PARENT: Set this keyword to the parent widget id in witch you want to place
;		this compound widget.
;
; OUTPUT: None.
;	
; COMMON BLOCKS:
;	CAVIAR_DATA, image, catstars
;	CAVIAR_PARAMS, dispParams, catalogs
;
; MODIFICATIONS:
;	2012, March			MEUNIER L-E				OBSPM>IMCCE
;		- Written
;-------------------------------------------------------------------------------
PRO caviar_catstars_gui, PARENT=wMAINbase, GROUP_LEADER=groupLeader, $
							XOFFSET=xoffset, YOFFSET=yoffset, TITLE=title, $
							FRAME=frame, XPAD=xpad, YPAD=ypad, SPACE=space
				 				  
	COMMON CAVIAR_DATA, image, catstars
	COMMON CAVIAR_PARAMS, dispParams, imgDraw, dispProName, catalogs, findStarsParams
	COMMON CAVIAR_GUI, wCAVIARtlb, wFITSATLIMBbase, wRINGSLOADbase, wCATSTARSLOADbase
        COMMON CAVIAR_SF, SFfactor
	
	; Test if widget window is already launched
	IF(XRegistered('caviar_catstars_gui') NE 0) THEN RETURN
	
	;***************************************************************************
	; Set parameters:
	;***************************************************************************
	; Set search field of view (in degree)
	SFfactor = 1
	fovimg = image.FOVIMG
	fovsearch = SFfactor*fovimg
	format = '("x", G-0.3, A)'
        sfovimg = STRING(0.5*fovimg  , '°', FORMAT=format)
		
	; Set catalogs table values and parameters:
	ncat = N_ELEMENTS(catalogs)
	table = caviar_catstars_gui_updtTable(catalogs)
	 
	loadBttnTooltip = "Load stars and remove doubles"

	;***************************************************************************
	; Define the widget base:
	;***************************************************************************
	IF NOT KEYWORD_SET(title) THEN title = '1. Load stars from catalogues'
	
	IF KEYWORD_SET(wMAINbase) THEN BEGIN
		IF NOT WIDGET_INFO(wMAINbase, /VALID_ID) $
		THEN res=DIALOG_MESSAGE('Invalid parent widget identifier.',/ERROR)
		wCATSTARSLOADbase = wMAINbase
		isTopBase = 0
	ENDIF ELSE BEGIN
		isTopBase = 1
		
		IF NOT KEYWORD_SET(groupLeader) $
		THEN wCATSTARSLOADbase = WIDGET_BASE(TITLE=title, XOFFSET=xoffset, YOFFSET=yoffset) $
		ELSE BEGIN
			IF WIDGET_INFO(groupLeader, /VALID_ID) $
			THEN wCATSTARSLOADbase = WIDGET_BASE(TITLE=title, XOFFSET=xoffset, YOFFSET=yoffset, $
										GROUP_LEADER=groupLeader, /FLOATING) $
			ELSE res=DIALOG_MESSAGE('Invalid group leader widget identifier.',/ERROR)
		ENDELSE
		
	ENDELSE
	
	;***************************************************************************
	; Define subwidgets tree:
	;***************************************************************************
	wExtraBase = WIDGET_BASE(wCATSTARSLOADbase, /COLUMN, XPAD=0, YPAD=0)
		IF NOT isTopBase THEN wLbl = WIDGET_LABEL(wEXTRAbase, VALUE=title, /ALIGN_CENTER)
		wbase = WIDGET_BASE(wExtraBase, /COL, FRAME=frame, XPAD=xpad, YPAD=ypad, SPACE=space)
	
	wbase1 = WIDGET_BASE(wbase, /ROW, /BASE_ALIGN_CENTER, YPAD=0);, SPACE=10)
		wGetStarsBase = WIDGET_BASE(wbase1, /COL, XPAD=0, YPAD=0, SPACE=0)
			wGetStarsBttn = WIDGET_BUTTON(wGetStarsBase, UNAME="GET_STARS", $
				VALUE="Load", TOOLTIP=loadBttnTooltip, YSIZE=28, XSIZE=104)
			wGetStarsLbl  = WIDGET_LABEL (wGetStarsBase, XSIZE=75, /ALIGN_LEFT, $
				VALUE=" Found: "+STRING(N_ELEMENTS(catstars), '(I0)'))
		
		;wSpace = WIDGET_BASE(wbase1, XSIZE=1, YSIZE=25, /FRAME)
		
		wFIELDbase = WIDGET_BASE(wbase1, /ROW, XPAD=0, YPAD=0, SPACE=0)
			wSFField = CW_FIELD(wFIELDbase, /ALL_EVENTS, /FLOATING, $
					TITLE="Search radius:", VALUE=SFfactor, UNAME="SFFACTOR", XSIZE=3)
			wSFLabel = WIDGET_LABEL(wFIELDbase, VALUE=sfovimg, XSIZE=52, /ALIGN_LEFT)

	wPARAMSbase = WIDGET_BASE(wbase, /COL, /ALIGN_CENTER, /BASE_ALIGN_CENTER, SPACE=0)		
		wMagLbl = WIDGET_LABEL(wPARAMSbase, VALUE=" Catalogues    Mag Search   Mag Priority")
		wMagTbl = WIDGET_TABLE(wPARAMSbase, UNAME="CATALOGS_PARAMS", /ALL_EVENTS, $
					/ROW_MAJOR, /NO_ROW_HEADERS, /NO_COLUMN_HEADERS, ALIGNMENT=1, $
					COLUMN_WIDTH=[20,80,42,42,42,42], ROW_HEIGHTS=INTARR(ncat)+18, $
					Y_SCROLL_SIZE=(ncat < 5), $
					VALUE=table.values, EDITABLE=table.editstate, $
					BACKGROUND_COLOR=table.bgcolor, FOREGROUND_COLOR=table.fgcolor)
		WIDGET_CONTROL, wMagTbl, SET_TABLE_SELECT=[2,0,2,0]
		
	IF isTopBase THEN close_buttonID = WIDGET_BUTTON(wExtraBase, VALUE="CLOSE", UNAME="CLOSE")
	
	; Create the widget (i.e. display it)
	WIDGET_CONTROL, wCATSTARSLOADbase, /REALIZE
	
	;***************************************************************************
	; Define 'state' structure and copy it in 'wExtraBase' widget uvalue
	;***************************************************************************
	state = {wGetStarsLblID:wGetStarsLbl, wSFField:wSFField, wSFLabel:wSFLabel, fovsearch:fovsearch}
	WIDGET_CONTROL, wExtraBase, SET_UVALUE=PTR_NEW(state)

	; Let XMANAGER takes control of widgets event processing
	XMANAGER, 'caviar_catstars_gui', wEXTRABase, /JUST_REG, GROUP_LEADER=groupLeader
END
