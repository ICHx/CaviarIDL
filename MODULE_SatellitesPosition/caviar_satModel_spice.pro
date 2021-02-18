;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
; PURPOSE:
; INPUT:
; OUTPUT:
;---------------------------------------------------------------------------------------------------
FUNCTION caviar_satModel_spice_getPoints, radius, tipm, npts, rho, rhoi
	
	; Get CSPICE ellipsoid as viewed from the spacecraft:
	; For limb, rhoi vector ~ sat->spc
	; For terminator, rhoi vector ~ sun->sat
	; For equator, rhoi vector ~ north->sat
	cspice_edlimb, radius[0], radius[1], radius[2], rhoi, spice_ellipse
	
	; Extract CSPICE ellipsoid parameters: center, semi-major axis and semi-minor axis
	cspice_el2cgv, spice_ellipse, center, smajor, sminor
	
	; Convert ellipsoid parameters from body-fixed (x,y,z)sat to inertial (x,y,z)J2000 coordinates:
	center = tipm#center
	smajor = tipm#smajor
	sminor = tipm#sminor

	; Construct theta vector from -pi to +pi
	theta = LINDGEN(npts) * 2*!DPI/(npts-1) - !DPI
	
	; Compute ellipsoid points position in inertial coordinates
	points = DBLARR(3,npts)
	cos = cos(theta)#smajor
	sin = sin(theta)#sminor
	FOR i=0,2 DO points[i,*] = REPLICATE(center[i], npts) + cos[*,i] + sin[*,i]

	; Compute ellipse point vector
	x = points[0,*]-rho[0]
	y = points[1,*]-rho[1]
	z = points[2,*]-rho[2]
	pvec = [x, y, z]
	
	;***********************************************************************************************
	; Filter ellipse points that are not in the plane normal to pointing vector:
	invNorm_pvec = DBLARR(3,npts)
	FOR i=0L, npts-1 DO invNorm_pvec[*,i] = 1.0D/NORM(pvec[*,i])
	
	points_test = TRANSPOSE(tipm) # (points - 0.05*pvec*invNorm_pvec)
	pvec_test = -(TRANSPOSE(tipm) # pvec)
		
	found = INTARR(npts)
	FOR i=0L, npts-1 DO BEGIN
		cspice_surfpt, points_test[*,i], pvec_test[*,i], radius[0],radius[1],radius[2], fndpnt, fndi
		found[i] = fndi
	ENDFOR
	index = WHERE(found EQ 0, count)
	IF count GT 0 THEN pvec_fnd = pvec[*,index] ELSE RETURN, 0
	
	;***********************************************************************************************
	; Conversion of ellipse points from (x,y,z)J2000 to (RA,dec)J2000 (radians):
	cspice_recrad, pvec_fnd, range, ra, dec
		
	; Conversion of ellipse points from (RA,dec)J2000 to (sample,line):
	radec2slcoord, ra, dec, sample, line
			
	; Return ellipse points coordinates in ra/dec (milli-arcseconds) and sample/line coordinates:
	RETURN, {RA:ra, DEC:dec, SAMPLE:sample, LINE:line}
END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
; PURPOSE:
; INPUT:
; OUTPUT:
;---------------------------------------------------------------------------------------------------
FUNCTION caviar_satModel_spice, satID, et, spcID, n_points, CENTER=center
	
	IF n_points LE 0 THEN MESSAGE, "Variable 'NPTS' must be greater than zero"
	
	;# Get sat->spc position vector
	cspice_spkez, satID, et, 'J2000', 'CN+S', spcID, state, ltime
	rho=-state[0:2]
	rho_center=rho
	IF KEYWORD_SET(center) THEN BEGIN
		slcoord2radec, center[0], center[1], RA, dec, /ITERATE
		rho = - NORM(rho) * [cos(dec)*cos(RA), cos(dec)*sin(RA), sin(dec)]
		rho_center=-rho	
	ENDIF
	
	;# Get sun->sat position vector
	cspice_spkez, 10L, et-ltime, 'J2000', 'CN', satID, state, ltime_sun
	rho_sun=-state[0:2]
	
	
	
	;# Get 3x3 transformation matrix from inertial (J2000) to body-equator-and-prime-meridian coordinates: (tipm)
	cspice_tipbod, 'J2000', satID, et-ltime, tipm
				
	;# Get length of ellipsoid semi-axis lying on the x-axis/y-axis/z-axis
	cspice_bodvar, satID, 'RADII', radii
		
	;# Compute limb/terminator/equator ra/dec & x/y positions
	limb = caviar_satModel_spice_getPoints(radii, tipm, n_points, rho, TRANSPOSE(tipm##rho))
	term = caviar_satModel_spice_getPoints(radii, tipm, n_points, rho, TRANSPOSE(tipm##rho_sun))
	equa = caviar_satModel_spice_getPoints(radii, tipm, n_points, rho, [0.0D, 0.0D, 300000.0D])

	
	;# Compute limb center ra/dec & x/y position from rectangular coordinates (rho_center)
	cspice_recrad, rho_center, range, rac, decc
	radec2slcoord, rac, decc, sc, lc
	
	model = {npts: n_points, slCenter: [[sc],[lc]], rdCenter: [[rac],[decc]], $
			 slLimb:[[limb.sample], [limb.line]], rdLimb:[[limb.ra], [limb.dec]], $
			 slTerm:[[term.sample], [term.line]], rdTerm:[[term.ra], [term.dec]], $
			 slEqua:[[equa.sample], [equa.line]], rdEqua:[[equa.ra], [equa.dec]] $
			}
		
	RETURN, model
END
