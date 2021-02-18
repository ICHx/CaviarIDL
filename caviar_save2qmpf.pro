PRO caviar_save2qmpf_printGalileoInfo, exposure, et
	
	etstart = et - 0.5D*exposure
	etstop  = et + 0.5D*exposure
	cspice_sce2s, -77L, etstart, sclkch_start
	cspice_sce2s, -77L, etstop, sclkch_stop

	cspice_timout, etstart, 'YYYY-DOYTHR:MN:SC.###::UTC::RND', 22, cstarttime
	cspice_timout, etstop,  'YYYY-DOYTHR:MN:SC.###::UTC::RND', 22, cstoptime

	sclkch_start_count = STRSPLIT(sclkch_start, '/', /EXTRACT)
	sclkch_stop_count  = STRSPLIT(sclkch_stop,  '/', /EXTRACT)

	PRINTF, lun, 'SPACECRAFT_CLOCK_START_COUNT = '+sclkch_start_count[1]
	PRINTF, lun, 'SPACECRAFT_CLOCK_STOP_COUNT = '+sclkch_stop_count[1]
	PRINTF, lun, 'START_TIME = '+cstarttime+'Z'
	PRINTF, lun, 'STOP_TIME = '+cstoptime+'Z'

END

PRO caviar_save2qmpf, CAM_OFFSET_QUAT=cam_offset_quat, FLAG=flag

	COMMON CAVIAR_GUI, wCAVIARtlb
	COMMON CAVIAR_DATA, image, stars, imgStars, planets
	COMMON CAVIAR_PARAMS, dispParams, imgDraw, dispProName, starsCatalogs, find_imgStars_params
	
	; FLAG is used to inform the caller that the file has been saved!
	flag=0
	
	
	metakernel_file = image.metaKernel
	
	
	
	; Ask where to save the QMPF file. Default is image directory
	imgName = STRMID(image.NAME, 0, STRPOS(image.NAME, '.', /REVERSE_SEARCH))
	qmpf_file = DIALOG_PICKFILE(/WRITE,  /OVERWRITE_PROMPT, $
								FILE=image.path+PATH_SEP()+imgName, $
								TITLE="Select a file for saving results", $
								PATH=image.path, DEFAULT_EXTENSION='QMPF', $
								FILTER=[['*.QMPF;*.qmpf' , '*'],['QMPF', 'All files']], $
								DIALOG_PARENT=wCAVIARtlb)
	IF qmpf_file EQ '' THEN RETURN
	OPENW, lun, qmpf_file, /GET_LUN
	
		
	;###############################################################################################
	SPAWN, 'date', date
	printf, lun, 'IMAGE_DATA_FILE = '+image.NAME
	printf, lun, 'USER    = ', GETENV('USER')
	printf, lun, 'HOST    = ', GETENV('HOSTNAME')
	printf, lun, 'DATE    = '+date
	printf, lun, 'VERSION = ', GETENV('CAVIAR_VERSION_TEXT')
	printf, lun, 'SPACECRAFT = '+image.SPC.NAME
	
;njc Don't need this
;	lblsize = SIZE(image.header)
;	CASE lblsize[0] OF
;		1: FOR i=0, lblsize[2]-1 DO PRINTF, lun, image.header[0,i]+' = '+image.header[1,i]
;		2: FOR i=0, lblsize[1]-1 DO PRINTF, lun, image.header[i]
;	ENDCASE

;njc Added:
        a=strpos(image.header[0,*],'PRODUCT_ID')
        b=where(a eq 0)
        printf,lun,image.header[0,b[0]]+' = '+image.header[1,b[0]]
        a=strpos(image.header[0,*],'OBSERVATION_ID')
        b=where(a eq 0)
        printf,lun,image.header[0,b[0]]+' = '+image.header[1,b[0]]    
        a=strpos(image.header[0,*],'NL')
        b=where(a eq 0)
        printf,lun,image.header[0,b[0]]+' = '+image.header[1,b[0]]    
        a=strpos(image.header[0,*],'SPACECRAFT_CLOCK_CNT_PARTITION')
        b=where(a eq 0)
        printf,lun,image.header[0,b[0]]+' = '+image.header[1,b[0]]    
        a=strpos(image.header[0,*],'SPACECRAFT_CLOCK_START_COUNT')
        b=where(a eq 0)
        printf,lun,image.header[0,b[0]]+' = '+image.header[1,b[0]]    
        a=strpos(image.header[0,*],'SPACECRAFT_CLOCK_STOP_COUNT')
        b=where(a eq 0)
        printf,lun,image.header[0,b[0]]+' = '+image.header[1,b[0]]    
        a=strpos(image.header[0,*],'START_TIME')
        b=where(a eq 0)
        printf,lun,image.header[0,b[0]]+' = '+image.header[1,b[0]]    
        a=strpos(image.header[0,*],'STOP_TIME')
        b=where(a eq 0)
        printf,lun,image.header[0,b[0]]+' = '+image.header[1,b[0]]    
        a=strpos(image.header[0,*],'EXPOSURE_DURATION')
        b=where(a eq 0)
        printf,lun,image.header[0,b[0]]+' = '+image.header[1,b[0]]    

;	IF STRMATCH(image.SPC.NAME, '*GALILEO*', /FOLD_CASE) $
;	THEN caviar_save2qmpf_printGalileoInfo, image.exposure, image.et
	
	;######################################  STARS  ################################################
	n_fitStars = 0
	txresidual = 0D
	tyresidual = 0D
	FOR i=0, N_ELEMENTS(stars)-1 DO BEGIN
		IF (stars[i]).fitted EQ 1 THEN BEGIN
			n_fitStars++
;			PRINTF,lun,format='("STAR = ", A-17, 2(D17.12), 4(D10.4), 2(D10.4), D8.3)', $
;njc For UCAC5/GAIA
                        PRINTF,lun,format='("STAR = ", A-30, 2(D17.12), 4(D10.4), 2(D10.4), D8.3)', $
				(stars[i]).name, $
				(stars[i]).RA*180.0D/!DPI, (stars[i]).dec*180.0D/!DPI, $
				(stars[i]).ycent, (stars[i]).xcent, $
				(stars[i]).ycoord, (stars[i]).xcoord,$
				(stars[i]).yresidual, (stars[i]).xresidual, (stars[i]).mag
			tyresidual += (stars[i]).yresidual^2
			txresidual += (stars[i]).xresidual^2
		ENDIF
	ENDFOR
	IF n_fitStars GE 2 THEN BEGIN
		rms_sRes = SQRT( txresidual/n_fitStars )				;Before, it was: (n_fitStars*(n_fitStars-1))
		rms_lRes = SQRT( tyresidual/n_fitStars )
		rms_tRes = SQRT( (txresidual+tyresidual) / (n_fitStars*(n_fitStars-1)) )	

		PRINTF, lun, 'STAR_RMS_RESIDUAL = '+STRTRIM(STRING(rms_tRes),1)
		PRINTF, lun, 'STAR_RMS_LINE_RESIDUAL = '+STRTRIM(STRING(rms_lRes),1)
		PRINTF, lun, 'STAR_RMS_SAMPLE_RESIDUAL = '+STRTRIM(STRING(rms_sRes),1)
	ENDIF

	;#####################################  PLANETS  ###############################################
	FOR i=0, N_ELEMENTS(planets)-1 DO BEGIN

		slen_satname = STRCOMPRESS( STRING( STRLEN((planets[i]).name) ) , /REMOVE_ALL)
		IF NOT haveTag(planets[i], "METHOD") THEN BEGIN
		;	PRINTF, lun, FORMAT='(A12, A'+slen_satname+')', "SATELLITE = ", (planets[i]).name
			continue
		ENDIF
		
		CASE STRUPCASE((planets[i]).method) OF
			"LIMB-FIT":	 		; Fitted center of figure based on limb-fit
			"CENTROID": 			; Fitted center of light (centroid)
			"ESTIMATED": BEGIN		; Estimated centre (alias method 3)
				col_offset, (planet[i]).id, image.SPC.ID, image.et, image.cmat, image.fovpix, $
							yoffset, xoffset
			END
			ELSE: GOTO, PASS_NEXT		;Pass to next planet
		ENDCASE
												  
		IF (planets[i]).sigma NE -1.0d0 $
		THEN sigma=STRCOMPRESS(STRING((planets[i]).sigma),/remove_all) $
		ELSE sigma=' '

		slen_method = STRING( STRLEN((planets[i]).method)+1, '(I0)' )
		slen_sigma  = STRING( STRLEN(sigma)+1 , '(I0)' )
;njc Changed line and sample order back to match original QMPF (sample then line):
		PRINTF, lun, $
			FORMAT='(A12, A'+slen_satname+', 2(D10.4), 2(D17.12), A'+slen_sigma+', A'+slen_method+', 2(D10.3))', $
			'SATELLITE = ', (planets[i]).name, $
	        (planets[i]).ycent, (planets[i]).xcent, $
	        (planets[i]).RA_cent*180.0D/!DPI, (planets[i]).dec_cent*180.0D/!DPI, $
	        (planets[i]).sigma,  $
                (planets[i]).method, $
	        (planets[i]).yoffset, (planets[i]).xoffset
	        
		PASS_NEXT:
		
		IF i EQ N_ELEMENTS(planets)-1 THEN PRINTF, lun, "* Offset values are included in centroid and residual values."
	ENDFOR
	
	;###############################################################################################
   	cspice_m2eul, image.cmat, 3, 2, 3, twistogll, deco, rao
 	cspice_m2eul, image.cmat, 3, 1, 3, twisto, deco, rao

	rao  = rao-(0.5*!dpi)
	deco = (0.5*!dpi)-deco
              
	rao		  *= !RADEG
	deco	  *= !RADEG
	twisto    *= !RADEG
	twistogll *= !RADEG

	IF rao		 LT 0 THEN rao		 += 360
	IF twisto	 LT 0 THEN twisto	 += 360
	IF twistogll LT 0 THEN twistogll += 360

	PRINTF, lun, FORMAT='(a14,d17.12)', 'POINTING_RA = ',rao
	PRINTF, lun, FORMAT='(a15,d17.12)', 'POINTING_DEC = ',deco
	PRINTF, lun, FORMAT='(a17,d17.12)', 'POINTING_TWIST = ',twisto
	PRINTF, lun, FORMAT='(a22,d17.12)', 'POINTING_TWIST(GLL) = ',twistogll

	cspice_m2q, image.cmat, cmat_quat

	PRINTF, lun, FORMAT='(A22, 4(D17.13))', $
		'POINTING_QUATERNION = ', cmat_quat[0], cmat_quat[1], cmat_quat[2], cmat_quat[3]
	IF KEYWORD_SET(cam_offset_quat) $
	THEN PRINTF, lun, FORMAT='(A27, 4(D17.13))', 'CAMERA_OFFSET_QUATERNION = ', $
					cam_offset_quat[0], cam_offset_quat[1], cam_offset_quat[2], cam_offset_quat[3]

             
	;###############################################################################################
	PRINTF, lun, 'CAM_PARAMS[0] = ', image.focal
	PRINTF, lun, 'CAM_PARAMS[1] = ', image.epsilon[1]
	PRINTF, lun, 'CAM_PARAMS[2] = ', image.epsilon[4]
	PRINTF, lun, 'CAM_PARAMS[3] = ', image.epsilon[5]
	PRINTF, lun, 'CAM_PARAMS[4] = ', image.Kmat[0,0]
	PRINTF, lun, 'CAM_PARAMS[5] = ', image.Kmat[1,0]
	PRINTF, lun, 'CAM_PARAMS[6] = ', image.Kmat[0,1]
	PRINTF, lun, 'CAM_PARAMS[7] = ', image.Kmat[1,1]
	PRINTF, lun, 'CAM_PARAMS[8] = ', image.Kmat[2,0]		;Kxxy
	PRINTF, lun, 'CAM_PARAMS[9] = ', image.Kmat[2,1]		;Kyxy
	PRINTF, lun, 'CAM_PARAMS[10] = ', image.bigomega		;Big Omega
	PRINTF, lun, 'CAM_PARAMS[11] = ', image.fovpix
	
	IF N_ELEMENTS(find_imgStars_params) NE 0 THEN BEGIN
		fs_params = ' fwhm:'+STRING(find_imgStars_params.fwhm,'(G0)')+ $
					' nsig:'+STRING(find_imgStars_params.nsig,'(G0)')+ $
					' roundlim: ['+STRING(find_imgStars_params.roundlim[0],'(G0)')+', ' $
								  +STRING(find_imgStars_params.roundlim[1],'(G0)')+ ']'+ $
					' sharplim: ['+STRING(find_imgStars_params.sharplim[0],'(G0)')+', ' $
								  +STRING(find_imgStars_params.sharplim[1],'(G0)')+ ']'
				
		PRINTF, lun, 'FIND_STAR_PARAMETERS ='+fs_params
	ENDIF
	
	;###############################################################################################
	;# Extract lines from metakernel file to kfb
	kfb = STRARR(FILE_LINES(metakernel_file))
	OPENR, lun_2, metakernel_file, /GET_LUN
	READF, lun_2, kfb
	FREE_LUN, lun_2
	
	pos_beg = WHERE(STRMATCH(kfb, '*begindata*') EQ 1)
	pos_end = WHERE(STRMATCH(kfb, '*begintext*') EQ 1)
	
	FOR i=pos_beg[0], pos_end[0] DO BEGIN
		text = STRCOMPRESS(kfb[i], /REMOVE_ALL)
		PRINTF, lun, 'KERNEL = '+STRMID(text, 0, STRLEN(text))
	ENDFOR
	FREE_LUN, lun
	
	flag=1
	
	RETURN
END
