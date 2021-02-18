;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: 
;	radec2radlon
; PURPOSE:
;	The normalized vector of input RA/dec coordinates is considered pointing on equatorial plane
;	of the input planet (i.e. ring plane for Saturn case). The point on this plane will be named
;	"ring point". This procedure gives the radius and longitude coordinates in planet reference 
;	frame of this point.
;
; INPUTS:
;   RA/dec: Scalars containing RA/dec coordinates (in radian) refering to a point on equatorial 
;			plane of the input planet (eg. ring plane for Saturn case).
;	planet: structure with these tags: 'id', 'polera', 'poledec'
;				'id': NAIF integer code of the planet
;				'polera'/'poledec': RA/dec coordinates of the planet pole in J2000
;	et: Spacecraft time/image start time in seconds past J2000
;	spcID: Spacecraft NAIF integer code, one whose encoded clock values is represented by sclkdp.
;			
; OUTPUTS:
;	radius/longitude: Scalars containing radius/longitude coordinates of the point on equatorial 
;					  plane	of the input planet (eg. ring plane for Saturn case).
;	length: Distance from the spacecraft to the "ring point"
;
; MODIFICATIONS:
;	2012 October		MEUNIER L-E				OBSPM>IMCCE
;		- Add header and comments
;		- Cosmetics, variables name
;		- Matrix#Vector multiplications order
;---------------------------------------------------------------------------------------------------
PRO radec2radlon, RA, dec, planet, et, spcID, radius, longitude, length

	;# Construct rotation matrix from J2000 to planet reference frame
	capN = (planet.polera+90.0D0)  *!DTOR
	capJ = (90.0D0-planet.poledec) *!DTOR
	cos_capN = cos(capN) & sin_capN = sin(capN)
	cos_capJ = cos(capJ) & sin_capJ = sin(capJ)
	rot=[ [ cos_capN, -sin_capN*cos_capJ,  sin_capN*sin_capJ], $
		  [ sin_capN,  cos_capN*cos_capJ, -cos_capN*sin_capJ], $
		  [ 	  0D, 			sin_capJ, 			cos_capJ] ]
	
	;***********************************************************************************************	
	
	;# Convert normalized vector given by RA/dec (in radian) to cartesian coordinates (x,y,z),
	;# first in J2000 reference frame (rho_J2000) and then in planet referential (rho)
        rho_J2000 = [cos(dec)*cos(RA), cos(dec)*sin(RA), sin(dec)]
	rho = rot#rho_J2000
		
	;***********************************************************************************************	
	
	;# Get planet->spacecraft position vector in J2000 reference frame (xyz_J2000) and then convert 
	;# into planet referential (xyz).
	cspice_spkez, planet.id, et, 'J2000', 'NONE', spcID, state, light_time
	xyz_J2000 = -state[0:2]
	xyz = rot#xyz_J2000

	;***********************************************************************************************
	
	;# Verify that spacecraft points to the equatorial plane and is not exactly in this plane
	IF (xyz[2] LT 0 && rho[2] LE 0) || $
	   (xyz[2] GE 0 && rho[2] GE 0) $
	THEN BEGIN
        radius	= -1.0d0
		longitude = -1.0d0
		length	= -1.0d0
		RETURN
	ENDIF

	;***********************************************************************************************	
	
	;# Compute distance from the spacecraft to the "ring point"
	length = ABS(xyz[2]/rho[2])
	
	;# Compute vector from the planet to the "ring point", in planet reference frame (xyz_rp)
	xyz_rp = xyz + length*rho
	
	;***********************************************************************************************	
	
	;# Correction for light time between spacecraft and "ring point".
	;# No iteratations needed since location is fixed w.r.t the planet center.
	;# This bit allows for aberration effects assuming a solid, non-rotating disc, 
	;# lying in the ring  plane centred on the planet.
	light_time = NORM(xyz_rp-xyz)/299792.458d0
	cspice_spkez, planet.id, et-light_time, 'J2000', 'NONE', spcID, state, ltime_dummy
	xyz = rot#(-state[0:2])
        length = ABS(xyz[2]/rho[2])
	xyz_rp = xyz + length*rho

	;***********************************************************************************************
	
	radius = NORM(xyz_rp)
	longitude = atan(xyz_rp[1],xyz_rp[0]) *!RADEG
	IF longitude LT 0 THEN longitude+=360.0D

	RETURN
end

