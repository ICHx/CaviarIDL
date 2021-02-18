;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: restoreCMAT
; PURPOSE: Restore saved or initial pointing/rotation matrix from camera structure tag 'cmat_saved' 
;		   or 'cmat_ini'.
; MODIFICATION:
;	2012, july				L-E. MEUNIER			OBSPM>IMCCE
;		- Written
;---------------------------------------------------------------------------------------------------
PRO restoreCMAT, SAVED=SAVED, INITIAL=INITIAL
	
	COMMON CAVIAR_DATA, image, catstars, imgstars, satellites, rings
		
	IF KEYWORD_SET(SAVED) 	THEN image.cmat = image.cmat_saved
	IF KEYWORD_SET(INITIAL) THEN image.cmat = image.cmat_ini
	
	caviar_updtCoords, catstars, image.vobs_stars
	caviar_updtCoords, satellites
	caviar_updtCoords, rings
	caviar_display
	
END
