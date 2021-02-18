@caviar_satModel_spice.pro

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_SHAPEMODEL_MOVE
; PURPOSE: Update limb model position considering the shift/angle action
;-------------------------------------------------------------------------------
PRO caviar_satModel_move, TRANSLATE=shift, ROTATE=angle
	
	COMMON CAVIAR_PARAMS, dispParams, imgDraw, dispProName, starsCatalogs, find_imgStars_params
	COMMON CAVIAR_SATLIMBFIT, satModel, satLimb
	
	IF KEYWORD_SET(shift) THEN BEGIN
		shift /= imgDraw.ZFACTOR
		shift[1] = -shift[1]
	ENDIF
	
	IF KEYWORD_SET(angle) THEN BEGIN	
		; Get the rotation center:
		slCenter = haveTag(satModel, 'SLCENTER') ? satModel.slCenter : [[0],[0]]
		
		; Get the rotation matrix:
		angle = -angle
		R = [[ cos(angle), -sin(angle)], $
			 [ sin(angle),  cos(angle)]]
			 
		; Compute image origin rotated from angle relative to the center:
		rOri = ( DIAG_MATRIX([1,1]) - R ) ## slCenter
	ENDIF
	
	
	IF haveTag(satModel, 'SLCENTER') THEN BEGIN
		IF KEYWORD_SET(shift) THEN satModel.slCenter += shift
		
		; Update ra/dec coordinates:
		slcoord2radec, satModel.slCenter[0], satModel.slCenter[1], ra, dec, ITERATE=3
		satModel.rdCenter = [[ra], [dec]]
	ENDIF
	IF haveTag(satModel, 'SLLIMB') THEN BEGIN
		npts = (SIZE(satModel.slLimb))[1]
		
		IF KEYWORD_SET(shift) $
		THEN satModel.slLimb += TRANSPOSE(shift) ## MAKE_ARRAY(npts, VALUE=1.0D)
		
		IF KEYWORD_SET(angle) $
		THEN satModel.slLimb = rOri ## MAKE_ARRAY(npts, VALUE=1.0D) + R ## satModel.slLimb
		
		; Update ra/dec coordinates:
		slcoord2radec, satModel.slLimb[*,0], satModel.slLimb[*,1], ra, dec, ITERATE=1
		satModel.rdLimb = [[ra], [dec]]
	ENDIF
	IF haveTag(satModel, 'SLTERM') THEN BEGIN
		npts = (SIZE(satModel.slTerm))[1]
		
		IF KEYWORD_SET(shift) $
		THEN satModel.slTerm += TRANSPOSE(shift) ## MAKE_ARRAY(npts, VALUE=1.0D)
		
		IF KEYWORD_SET(angle) $
		THEN satModel.slTerm = rOri ## MAKE_ARRAY(npts, VALUE=1.0D) + R ## satModel.slTerm
		
		; Update ra/dec coordinates:
		slcoord2radec, satModel.slTerm[*,0], satModel.slTerm[*,1], ra, dec, ITERATE=1
		satModel.rdterm = [[ra], [dec]]
	ENDIF
	IF haveTag(satModel, 'SLEQUA') THEN BEGIN
		npts = (SIZE(satModel.slEqua))[1]
		
		IF KEYWORD_SET(shift) $
		THEN satModel.slEqua += TRANSPOSE(shift) ## MAKE_ARRAY(npts, VALUE=1.0D)
		
		IF KEYWORD_SET(angle) $
		THEN satModel.slEqua = rOri ## MAKE_ARRAY(npts, VALUE=1.0D) + R ## satModel.slEqua
		
		; Update ra/dec coordinates:
		slcoord2radec, satModel.slEqua[*,0], satModel.slEqua[*,1], ra, dec, ITERATE=1
		satModel.rdEqua = [[ra], [dec]]
	ENDIF
	
	; Update display
	caviar_display
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_SHAPEMODEL_LOAD
; PURPOSE: Load a shape model for the satellite by using one of these methods:
;	- Get an ellipsoid model from SPICE kernels and display it.
; INPUTS:
;	edParams: 4-Elements array. Edge detector parameters: median filter width 
;			  and canny algorithm high and low threshold and sigma.
;-------------------------------------------------------------------------------
PRO caviar_satModel_load, method
	
	COMMON CAVIAR_DATA, image, stars, imgStars, planets
	COMMON CAVIAR_SATPOS, selSatIndex, wSatTblID, wSaveLbl, wSLFbaseYsize
	COMMON CAVIAR_SATLIMBFIT, satModel, satLimb
	COMMON CAVIAR_GUI, wCAVIARtlb, wFITSATLIMBbase
		
	WIDGET_CONTROL, WIDGET_INFO(wFITSATLIMBbase, /CHILD), GET_UVALUE=pstate
	npts = (*pstate).satModelNpts
	vals = (*pstate).smcpVals
	pid = (planets[selSatIndex]).ID
	
	CASE method OF
		0: satModel = caviar_satModel_spice(pid, image.et, image.SPC.ID, npts)
	ENDCASE		
END

