;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_SATLIMBFIT_ITE
; PURPOSE: Fit the position of a satellite limb model using limb edge points and
;	an iterative algorithm. At each iteration, the model is moved considering 
;	the mean offset between satellite model points and edge points.
;
; INPUTS:
;	SATMODEL:
;	SATEDGE:
;
; OPTIONNAL INPUT KEYWORD:
;	TOLERANCE: Distance threshold to exclude edge points from fitting 
;		computation.
;
; OPTIONNAL OUTPUTS:
;	MEAN_SRES: Double precision scalar value. Mean residual in sample direction.
;	MEAN_LRES: Double precision scalar value. Mean residual in line direction.
;	MEAN_DRES: Double precision scalar value. Mean residual in any direction.
;-------------------------------------------------------------------------------
PRO caviar_satLimbFit_ite, satModel, satEdge, $
							TOLERANCE=tol, $
							mean_sres, mean_lres, mean_dres

	IF NOT KEYWORD_SET(tol) THEN tol=0.5D 
	
	npts  = N_ELEMENTS(satEdge[*,0])
	IF haveTag(satModel, 'SLLIMB') THEN nlimb = N_ELEMENTS(satModel.slLimb[*,0])
	IF haveTag(satModel, 'SLTERM') THEN nterm = N_ELEMENTS(satModel.slTerm[*,0])
	IF haveTag(satModel, 'SLEQUA') THEN nequa = N_ELEMENTS(satModel.slEqua[*,0])
	
	onesEdge = MAKE_ARRAY(1,npts, VALUE=1.0D)
 	onesLimb = MAKE_ARRAY(nlimb, VALUE=1.0D)
 	sEdge = TRANSPOSE(satEdge[*,0])##onesLimb
 	lEdge = TRANSPOSE(satEdge[*,1])##onesLimb
 	
 	
	PRINT, "Iteration | Tolerance | Mean residual |      x/y offset"
	PRINT, "          |  (pixel)  |    (pixel)    |        (pixel) "
 	format = '(4X, I2, 4X, "|", 3X, F5.2, 3X, "|", 4X, F7.4, 4X, "|", 3X, F7.4, " / ", F7.4)'
 	 	
 	; Iterations loop
	k = 1
	slOffset = 10
	REPEAT BEGIN
		
		; Compute distance between each points of satEdge and satModel:
		sres = sEdge - onesEdge##(satModel.slLimb)[*,0]
		lres = lEdge - onesEdge##(satModel.slLimb)[*,1]
		min_dres = SQRT( MIN( sres*sres + lres*lres, min_pos, DIMENSION=1) )
		
		fittedPnts = WHERE(min_dres LT tol, count)
		IF count EQ 0 THEN BEGIN
			tol *= 2.0D
			k++
			CONTINUE
		ENDIF
		slRes = [ [sres[min_pos[fittedPnts]]], [lres[min_pos[fittedPnts]]] ]
		dres = min_dres[fittedPnts]
		
		; Offset SPICE data with mean residual
		slOffset = TRANSPOSE( MEAN(slRes, DIMENSION=1) )
		IF haveTag(satModel, 'SLCENTER') THEN satModel.slCenter += slOffset
		IF haveTag(satModel, 'SLLIMB')   THEN satModel.slLimb   += slOffset ## (DBLARR(nlimb) + 1.0D)
		IF haveTag(satModel, 'SLTERM')   THEN satModel.slTerm   += slOffset ## MAKE_ARRAY(nterm, VALUE=1.0D)
		IF haveTag(satModel, 'SLEQUA')   THEN satModel.slEqua   += slOffset ## MAKE_ARRAY(nequa, VALUE=1.0D)
		
		; Print results:
		PRINT, FORMAT=format, k, tol, MEAN(dres), slOffset[0], slOffset[1]
						
		k++
	ENDREP UNTIL NORM(slOffset[0,*]) LT 1.0D-6 || k EQ 100
	
	mean_sres = MEAN(ABS(slRes[*,0]))
	mean_lres = MEAN(ABS(slRes[*,1]))
	mean_dres = MEAN(dres)
	rms_dres  = SQRT(MEAN(dres*dres))
	satEdge = [ [satEdge[fittedPnts,0]], [satEdge[fittedPnts,1]] ]
	
	; Compute RA/dec coordinates from x/y centroid	
	IF haveTag(satModel, 'SLCENTER') THEN BEGIN
		slcoord2radec, satModel.slCenter[0], satModel.slCenter[0], RA, dec, /ITERATE
		setStruct, satModel, 'RDCENTER', [[RA], [dec]]
	ENDIF
	IF haveTag(satModel, 'SLLIMB')   THEN BEGIN
		slcoord2radec, satModel.slLimb[0], satModel.slLimb[0], RA, dec, /ITERATE
		setStruct, satModel, 'RDLIMB', [[RA], [dec]]
	ENDIF
	IF haveTag(satModel, 'SLTERM')   THEN BEGIN
		slcoord2radec, satModel.slTerm[0], satModel.slTerm[0], RA, dec, /ITERATE
		setStruct, satModel, 'RDTERM', [[RA], [dec]]
	ENDIF
	IF haveTag(satModel, 'SLEQUA')   THEN BEGIN
		slcoord2radec, satModel.slEqua[0], satModel.slEqua[0], RA, dec, /ITERATE
		setStruct, satModel, 'RDEQUA', [[RA], [dec]]
	ENDIF
	
	nfitted = N_ELEMENTS(fittedPnts)
	PRINT, FORMAT='("Image limb points: ", I6, " matched, ", I6, " excluded")', nfitted, npts-nfitted
	PRINT, FORMAT='("Mean Residuals:", %"\n", 4(A, " = ", G0, " pixels", %"\n"))', $
		"    Sample  ", mean_sres, "    Line    ", mean_lres, $
		"    Distance", mean_dres, "RMS Residual", rms_dres

	RETURN
END

