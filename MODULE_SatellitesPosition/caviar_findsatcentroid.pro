;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_FINDSATCENTROID_COLOFFSET
; PURPOSE: Compute the object's center of light offset to correct for the phase 
;		angle and obtain the center of figure.
; INPUTS:
;	pid: Scalar integer. NAIF integer code of the planet (target body). Ex: 501=Phobos
;
; OUTPUT:
;	offset: 2-Elements array of double. Sample and line offset between the 
;		Center Of Light and the Center Of Figure (in pixel) due to phase  
;		angle between spacecraft, planet (or satellite) and sun.
;-------------------------------------------------------------------------------

FUNCTION caviar_findsatcentroid_coloffset, pid, et, imgpixfov, spcID, cmat
	
;	CATCH, error_status
;	IF error_status NE 0 THEN BEGIN
;		CATCH, /CANCEL
;		pos = STRPOS(!ERROR_STATE.MSG, ']')
;		PRINT, !ERROR_STATE.MSG_PREFIX+STRMID(!ERROR_STATE.MSG, 0, pos+1)
;		s = STRSPLIT(STRMID(!ERROR_STATE.MSG, pos+2), '(  +)', /REGEX, /EXTRACT)
;		s[0] = STRMID(s[0], 0, STRLEN(s[0])-1) 
;		PRINT, STRJOIN(s, ' ')
;		MESSAGE, "Cannot compute COF/COL offset of body "+STRING(pid,'(I3)')
;		RETURN, [0D,0D]
;	ENDIF
		
	; Get position vector of:
	; - the spacecraft (observing body) relative to the planet (target): rho1
	; - the planet (observing body) relative to the sun (target): rho2
	cspice_spkezp, pid, et, 'J2000', 'CN+S', spcID, rho1, ltime
	cspice_spkezp, 10L, et-ltime, 'J2000', 'CN+S', pid, rho2, ltime
	
	; Get seperation angle between spacecraft and sun, viewed from the satellite:
	phi = cspice_vsep(-rho1,rho2)
	
	; Get maximum radius of the satellite on the sensor:
	cspice_bodvar, pid, 'RADII', radii
	radpix = ATAN( MAX(radii),NORM(-rho1) ) / imgpixfov
	
	rho_camframe = cmat ## rho2
	rho_camframe /= NORM(rho_camframe)
	
	corr = -radpix * (3*!DPI*SIN(phi)*(1+COS(phi))) / (16*(SIN(phi)+((!DPI-phi)*COS(phi))))

	offset = rho_camframe * corr
	
	RETURN, offset
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_FINDSATCENTROID
; PURPOSE: Compute center of light and phase angle offset for unresolved satellite.
; NOTE: Use find_stars adapted from 1986 STSDAS version of DAOPHOT.
; INPUTS:
;	img: 2-Dimensionnal array. Image with the satellite centroid to locate.
;	satID: Satellite ID (NAIF integer code).
;	sat[X/Y]pos: [Scalar Integer/Float/Double] Estimated satellite position in img.
;	satRadiusKm: [Scalar Integer/Float/Double] Satellite radius in kilometers.
;		For body with different x/y/z radii, take the max! 
; KEYWORDS:
;	SILENT: If set to 1, no output will be made, otherwise it displays distance, 
;		    radius and fwhm of the satellite.
; PROCEDURES/FUNCTIONS CALLS:
;	CAVIAR_IMGSTARS_FIND in caviar_imgstars_pkg.pro
;-------------------------------------------------------------------------------
FUNCTION caviar_findsatcentroid, img, satID, satRadiusKm, satXpos, satYpos, $
						et, imgpixfov, spcID, cmat, $
						SILENT=SILENT
	
	imgSize = SIZE(img)
	ns = imgSize[1]
	nl = imgSize[2]
	
	;***************************************************************************
	; Get fwhm of the pointed object:
	;***************************************************************************
	cspice_spkezp, satID, et, 'J2000', 'CN+S', spcID, ptarg, ltime
	distance = NORM(ptarg)
	satRadiusPix = ATAN(satRadiusKm,distance) / imgpixfov
	fwhm = (satRadiusPix GT 0.5) ? 2*satRadiusPix : 1
	
	IF NOT KEYWORD_SET(SILENT) THEN BEGIN
		PRINT, FORMAT='("Distance  (km) = ", F-0.3)', distance
		PRINT, FORMAT='("Radius    (km) = ", F-0.3)', satRadiusKm
		PRINT, FORMAT='("Radius (pixel) = ", F-0.3)', satRadiusPix
		PRINT, FORMAT='("FWHM   (pixel) = ", F-0.3)', fwhm
	ENDIF

	;***************************************************************************	
	; Get sub-image around the satellite estimated position 
	; with size of 2 times the object fwhm:
	;***************************************************************************
	xmin = ROUND( satXpos - ROUND(fwhm*2)-1 ) > 0
	ymin = ROUND( satYpos - ROUND(fwhm*2)-1 ) > 0
	xmax = ROUND( satXpos + ROUND(fwhm*2)+1 ) < ns-1
	ymax = ROUND( satYpos + ROUND(fwhm*2)+1 ) < nl-1
	sub_img = img[xmin:xmax,ymin:ymax]
	
	;***************************************************************************	
	; Search object centroid:
	;***************************************************************************
	rslt = caviar_imgstars_find(sub_img, fwhm, 1.0, [-1.0,1.0], [0.2,1.0], $
							CENTROID_METHOD=2, /SILENT)
	
	IF rslt EQ !NULL THEN MESSAGE, "Cannot find centroid."
	max_flux = MAX(rslt.flux, index)
	IF N_ELEMENTS(index) GT 1 $
	THEN MESSAGE, 'More than one local maximum has been found. All maxima have been returned.'
	 
	xcent = xmin + rslt.X[index]
	ycent = ymin + rslt.Y[index]

	IF xcent LT 0 || xcent GT ns-1 || ycent LT 0 || ycent GT nl-1 $
	THEN MESSAGE, 'Error in centroiding this object. Cannot obtain valid location.'
		
	;***************************************************************************
	; Compute phase angle offset:
	;***************************************************************************
	offset = caviar_findsatcentroid_coloffset(satID, et, imgpixfov, spcID, cmat)
	PRINT, "INFORMATION: Output x/y centroid values have been corrected for ", $
		   "phase angle by the x/y offset values, respectively!"
	PRINT, ""
	
	RETURN, {XCENT:xcent+offset[0], YCENT:ycent+offset[1], SIGMA:0.5D, XOFFSET:offset[0], YOFFSET:offset[1]}
END
