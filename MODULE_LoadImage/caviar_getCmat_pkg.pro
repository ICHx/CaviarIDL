@caviar_readCamParams
@headerxtract2

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: caviar_file2cmat
; PURPOSE: 
;   Searches for pointing file corresponding to the input image name and returns 
;   the rotation matrix. If the file does not exist or has no rotation matrix,
;   the function returns "0".
;-------------------------------------------------------------------------------
FUNCTION caviar_file2cmat, file
	
	IF NOT FILE_TEST(file, /REGULAR) THEN MESSAGE, "Input file does not exist."
	
	nlines = FILE_LINES(file)
	OPENR, lun, file, /GET_LUN
	READF, lun, ( lines = STRARR(nlines) )
	FREE_LUN, lun
	
	index = WHERE(STREGEX(lines,'POINTING_QUATERNION', /BOOLEAN), count) 
	IF count NE 1 THEN MESSAGE, '"POINTING_QUATERNION" not found in the file or found more than once.'
	lineParts = STRSPLIT(lines[index],'=',/EXTRACT)
	
	CATCH, error
	IF error NE 0 THEN BEGIN
		PRINT, !ERROR_STATE.MSG_PREFIX, !ERROR_STATE.MSG
		MESSAGE, 'Cannot read pointing quaternion from "'+lines[index]+'"'
	ENDIF
	READS, lineParts[1], ( pquat = DBLARR(4) )
	
	cspice_q2m, pquat, cmat
	
	PRINT, "Camera pointing matrix has been loaded from file: ", file
	RETURN, cmat
END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: caviar_spice2cmat
; PURPOSE: Get rotation matrix of the camera (cmat) from cspice kernel
; INPUTS: 
;	spcID: Spacecraft NAIF integer code, one of whose encoded clock values is
;			represented by sclkdp.
;	camID: Camera NAIF integer code
;	camLNAME: Camera long name with spacecraft + camera short name.
;	et: Ephemerides time/image middle time in seconds past J2000.
; OUTPUTS:
;	cmat: Rotation matrix corresponding to a specified unit quaternion (attitude)
;-------------------------------------------------------------------------------
FUNCTION caviar_spice2cmat, spcID, camID, camLNAME, et
	
	cspice_sce2t, spcID, et, sclkdp
	
	CATCH, error
	IF error NE 0 THEN PRINT, !ERROR_STATE.MSG_PREFIX+!ERROR_STATE.MSG $
	ELSE cspice_ckgp, camID, sclkdp, 1000D, 'J2000', cmat, clkout, found
	CATCH, /CANCEL
	
	IF NOT found THEN BEGIN
		; Get angles between spacecraft and camera orientation:
		caviar_readCamParams, camLNAME, (cp = {ANGLES:DBLARR(3)}), NFOUND=nfound
		IF nfound EQ 0 THEN MESSAGE, "Cannot find angles between spacecraft and camera orientation"
	
		; Get cmat (C-matrix) from the spacecraft orientation matrix (pmat)
		CATCH, error
		IF error NE 0 THEN PRINT, !ERROR_STATE.MSG_PREFIX+!ERROR_STATE.MSG $
		ELSE BEGIN
			cspice_ckgp, spcID*1000, sclkdp, 1000.0d0, 'J2000', pmat, clkout, found
			IF found THEN BEGIN
				cspice_eul2m, cp.angles[0]*!DTOR, cp.angles[1]*!DTOR, cp.angles[2]*!DTOR, 1, 2, 3, Rspc2cam
				cspice_eul2m, 0.0d0, 0.0d0, !DPI, 3, 3, 3, Rz
				cmat = Rz ## TRANSPOSE(Rspc2cam) ## pmat
			ENDIF
		ENDELSE
		CATCH, /CANCEL
	ENDIF
	
	IF NOT found THEN $
		MESSAGE, "Cannot load camera pointing matrix (cmat) from SPICE kernels"
	
	PRINT, "Camera pointing matrix (C-matrix) has been loaded from SPICE kernel."
	RETURN, cmat
END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: caviar_lbl2cmat
; PURPOSE: Get pointing matrix of the camera (cmat) from image labels
; INPUT: 
;	header: Image header with cone/phase/twist angles
; OUTPUTS:
;	cmat: Rotation matrix corresponding to a specified unit quaternion (attitude)
;-------------------------------------------------------------------------------
FUNCTION caviar_lbl2cmat, spcID, header, lblFormat
	
	; Search camera orientation (Euler) angles in the header:
	tags = ['CONE_ANGLE', 'PHASE_ANGLE', 'TWIST_ANGLE']
	CASE lblFormat OF
		'VIC': angles = headerXtract(header, tags, 'FLOAT', " ", "")
		'FIT': angles = headerXtract(header, tags, 'FLOAT', "'", "'")
		ELSE:  angles = headerXtract(header, tags, 'FLOAT', "=", "")
	ENDCASE
	FOREACH val, angles DO IF val EQ !NULL $
	THEN MESSAGE, "Cannot retrieve camera orientation angles from image header."
	
	
	; Convert orientation angles into pointing matrix:
	angles = angles.toArray() * !DTOR
	cspice_eul2m, angles[0], angles[1], angles[2], 3, 2, 3, cmat
	
	PRINT, "Camera pointing matrix (C-matrix) has been loaded from image header."
	RETURN, cmat
END
