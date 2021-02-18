PRO slcoord2radec_iterate, sample, line, ra, dec, NITE=nite, VERBOSE=VERBOSE
	
	COMMON CAVIAR_DATA, image
	IF KEYWORD_SET(VERBOSE) THEN PRINT, "Conversion from SAMPLE/LINE to RA/DEC (with iteration)"
	
	nval = N_ELEMENTS(sample)
		
	grid_pitch = image.fovpix*image.binning
	grid_size = 10
	npts = grid_size*grid_size
	ra_gridVec = DBLARR(npts)
	dec_gridVec = DBLARR(npts)
	
	IF NOT ISA(nite, 'INTEGER') THEN nite=3	
	
	
	FOR i=0, nval-1 DO BEGIN
		
		FOR k=0, nite-1 DO BEGIN
		
			IF KEYWORD_SET(VERBOSE) THEN PRINT, FORMAT='("Iteration #", I-, $)', k+1
			n_ite = 0
			REPEAT BEGIN
				n_ite++
			
				ra_vec  = ra[i]  + INDGEN(grid_size)-grid_size/2 * grid_pitch
				dec_vec = dec[i] + INDGEN(grid_size)-grid_size/2 * grid_pitch
				FOR i=0, grid_size-1 DO ra_gridVec[i*grid_size] = ra_vec
				FOR i=0, grid_size-1 DO dec_gridVec[i*grid_size] = REPLICATE(dec_vec[i], grid_size)
						
				radec2slcoord, ra_gridVec, dec_gridVec, sample_gridVec, line_gridVec
				residuals = (sample-sample_gridVec)*(sample-sample_gridVec) + (line-line_gridVec)*(line-line_gridVec)
				
				min_val = MIN(residuals, index)
				min_pos = ARRAY_INDICES(INTARR(grid_size, grid_size, /NOZERO), index)
				ra[i]  = ra_vec[min_pos[0]]
				dec[i] = dec_vec[min_pos[1]]
			
				IF KEYWORD_SET(VERBOSE) THEN BEGIN
					format = '(" Min residual (pixels): ", G-12.6, " at (", I3, ",", I3, ")")'
					PRINT, FORMAT=format, min_val, min_pos[0], min_pos[1]
				ENDIF
			ENDREP UNTIL min_pos[0] GT 0 || min_pos[0] LT grid_size-1 || $
						 min_pos[1] GT 0 || min_pos[1] LT grid_size-1 || $
						 n_ite GE 10
			IF n_ite EQ 10 THEN break
		
			grid_pitch *= 0.1D
		ENDFOR
		
	ENDFOR
	
	RETURN
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: 
;	slcoord2radec
; PURPOSE:
;	Convert image x/y coords into RA/dec coords.
; INPUTS:
;   xcoord/ycoord: Scalars or arrays containing x/y coordinates in pixel.
; OPTIONNAL_INPUT_KEYWORD:
;	ITERATE: The sample/line to ra/dec transformation is approximative since it doesn't take into 
;			account optical distorsion.
;			Setting this keyword to 1, will call a routine that will make a grid of ra/dec, compute
;			x/y coords with 'radec2slcoord' routine and return the ra/dec that minimize the error
;			between computed and inputs sample/line values.
; OUTPUTS:
;	RA/dec: Scalars or arrays containing RA/dec coordinates (in radian).
; MODIFICATIONS:
;	2012 February		MEUNIER L-E				OBSPM>IMCCE
;		- Add header and comments
;		- Cosmetics, variables name
;	2012 October		MEUNIER L-E				OBSPM>IMCCE
;		- Add 'ITERATE' and 'VERBOSE' keywords
;	2013 January		MEUNIER L-E				OBSPM>IMCCE
;		- Optimize performance by replacing FOR-loops by matrix calculation.
;---------------------------------------------------------------------------------------------------
PRO slcoord2radec, sample, line, ra, dec, ITERATE=nite, VERBOSE=verbose

	COMMON CAVIAR_DATA, image
	
	n = N_ELEMENTS(sample)

	; Set camera center position (s0,l0)
	IF NOT haveTag(image, 'center') $
	THEN center = 0.5D*[image.ns-1, image.nl-1] $
	ELSE center = image.center/image.binning
		

	K = image.Kmat * (DOUBLE(image.focal)/image.binning)
	Kt = TRANSPOSE(K)
	invK = TOTAL(K[2,*]) EQ 0 ? LA_INVERT(K[0:1,*]) : LA_INVERT(Kt##K)##Kt
	xy = invK ## [[sample-center[0]], [line-center[1]]]
	
	rhoCAM = MAKE_ARRAY(n, 3, VALUE=1.0D)
	rhoCAM[*,0:1] = xy[*,0:1]
	
	invNorm_rho_cam = DBLARR(n,3)
	FOR i=0, n-1 DO invNorm_rho_cam[i, *] = 1.0D/NORM(rhoCAM[i,*])

	rhoJ2000 = TRANSPOSE(image.cmat)##(rhoCAM*invNorm_rho_cam)
	
	cspice_recrad, TRANSPOSE(rhoJ2000), range, ra, dec
	
	IF KEYWORD_SET(ITERATE) $
	THEN slcoord2radec_iterate, sample, line, ra, dec, NITE=nite, VERBOSE=verbose
			
	RETURN
END

