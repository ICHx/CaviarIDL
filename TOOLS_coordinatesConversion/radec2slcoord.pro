;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: 
;	radec2slcoords
; PURPOSE:
;	Convert RA/dec coords into image s/l coords.
; INPUTS:
;   RA/dec: Scalars or arrays containing RA/dec coordinates in radians.
; OPTIONAL INPUT KEYWORD:
;	vobs: Relative velocity from the observer to the pointed object, to correct 
;		  for stellar aberration. 
;		  (Using this keyword will multiply computation time by about 4.)
; OUTPUTS:
;	sample/line: Image coordinates [sample, line] (in pixel) of the object(s).
; MODIFICATIONS:
;	2012 February			MEUNIER L-E				OBSPM>IMCCE
;		- Add header and comments
;		- Cosmetics, variables name
;	2012 October			MEUNIER L-E				OBSPM>IMCCE
;		- Use all epsilon parameters.
;		- Can use camera center parameter rather than image center.
;		- Change name from 'radec2xycoord' into 'radec2slcoord'
;		- Add CORRECT_STELAB keyword
;	2013 January			MEUNIER L-E				OBSPM>IMCCE
;		- Improve performance using less the FOR-loop (about 6 time faster)
;-------------------------------------------------------------------------------
PRO radec2slcoord, ra, dec, sample, line, CORRECT_STELAB=vobs
			
	COMMON CAVIAR_DATA, image
	
	IF ~haveTag(image,'CMAT') || TOTAL(image.cmat) EQ 0 $
	THEN MESSAGE, "Cannot compute image coordinates without camera pointing matrix (cmat)."
	
		
	; Set camera center position (s0,l0)
	IF NOT haveTag(image, 'CENTER') $
	THEN center = 0.5D*[image.ns-1, image.nl-1] $
	ELSE center = image.center
	
	; Convert normalized vector given by RA/dec (in radian) to cartesian coordinates (x,y,z)
	; in J2000 reference frame:
	rho_J2000 = [[cos(dec)*cos(ra)], [cos(dec)*sin(ra)], [sin(dec)]]
	
	; Correct the apparent position for stellar aberration: (80% of computation time)
	IF KEYWORD_SET(vobs) && (TOTAL(vobs) NE 0) THEN BEGIN
		FOR i=0, N_ELEMENTS(ra)-1 DO BEGIN
			cspice_stelab, TRANSPOSE(rho_J2000[i,*]), vobs, rhoi_J2000
			rho_J2000[i,*] = rhoi_J2000
		ENDFOR
	ENDIF
	
	; Compute unitary projection vector in sensor reference frame:
	rho_cam = image.cmat##rho_J2000
	
	; Compute focal plane coordinates x/y (in millimeters):
	fpz = (image.focal/rho_cam[*,2])
	x = fpz * rho_cam[*,0]		; x = f/pz * px
	y = fpz * rho_cam[*,1]		; y = f/pz * py
	
	; Correct for electromagnetic & optical distorsion:
	x2 = x*x  &  y2 = y*y  &  xy = x*y
	r = SQRT(x2 + y2)  &  r2 = r*r  &  r3 = r*r2  &  r4 = r2*r2
	x += image.epsilon##[[-y*r], [x*r2], [-y*r3], [x*r4], [xy], [x2]]
	y += image.epsilon##[ [x*r], [y*r2],  [x*r3], [y*r4], [y2], [xy]]
		
	; Conversion from focal plane into sample & line (pixel):
	K = DOUBLE(image.Kmat)
	sl = K ## [[x],[y],[x*y]]
	sample = (sl[*,0] + center[0])/image.binning
	line   = (sl[*,1] + center[1])/image.binning

	RETURN
END
