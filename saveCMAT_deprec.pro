;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: saveCMAT
; PURPOSE: Save current pointing/rotation matrix into 'cmat_saved' tag of camera structure to 
;		   restore it later, if necessary, with 'restoreCMAT, /SAVED' procedure.
; MODIFICATION:
;	2012, july				L-E. MEUNIER			OBSPM>IMCCE
;		- Written
;---------------------------------------------------------------------------------------------------
PRO saveCMAT
	
	COMMON CAVIAR_DATA, image, camera
	
	IF N_ELEMENTS(camera) GT 0 $
	THEN camera.cmat_saved = camera.cmat $
	ELSE MESSAGE, "There is no C-matrix to save!", /CONTINUE
	
END
