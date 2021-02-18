;*******************************************************************************
; NAME: CAVIAR_ITEREPOINT_NPASSALGO
; PURPOSE: Find the fitted pointing vector with 3-pass algorithm. At each pass 
;	find a more precise grid of pointing vector coordinates used to 
;	compute residual between stars' centroid (found in the image) and stars' 
;       coordinates (from catalogs).
;*******************************************************************************
PRO caviar_iteRepoint_nPassAlgo, ras, decs, xcents, ycents, pvec, $
								NPASS=npass, $
								THRESHOLD=threshold, $
								BLOCK_TWIST=BLOCK_TWIST
	
	COMMON CAVIAR_DATA, image
	
	IF NOT KEYWORD_SET(npass) 		THEN npass=10
	; Set the convergeance threshold:
	IF NOT KEYWORD_SET(threshold) 	THEN threshold=1.0E-6
	; Set the number of fitting parameters (ra/dec/twist):
	IF NOT KEYWORD_SET(BLOCK_TWIST) THEN nparams=3 ELSE nparams=2
		
	nstars = N_ELEMENTS(ras)
	
	; Set grid parameters and variables:
	gridSize = 11
	gridVec = INDGEN(gridSize)-FIX(gridSize)/2		;Grid vector [-npt/2 : 1 : npt/2]
	gridStep = [image.fovpix, image.fovpix, 1.0D-3]
	IF nparams EQ 2 THEN gridStep[2] = 0.0D
	
	ndim = N_ELEMENTS(pvec)
	npts = gridSize^3
	pvecArr = DBLARR(ndim, npts)
	
	; Set pvec array indices:
	ind = INDGEN(npts)			

	; Set the absolute coordinates of the edge of the grid:
	pmaxoff = ABS(gridStep * 0.5*(gridSize-1))
	
	; Declare sum square residuals variable:
	ssr = DBLARR(npts)
	
	; Define arbitrary initial RMS residual:
	rms_res = (gridSize+1)*MAX(gridStep)
	
	PRINT, FORMAT='(/,96("*"),/,"Start of n-pass iterative fitting with ",I0," stars:")', nstars 
	pass=1 & again=1 & ite=1
	WHILE again DO BEGIN
		
		PRINT, FORMAT='(A,I-2,2X,A,I-3,2X,A,3(G-0.3,X),$)', $
			"PASS ",pass,"ITE ",ite,"Grid steps: ",gridStep
				
		; Create an array with all pointing vector ra/dec/twist parameters grid:
		FOR i=0, ndim-1 $
		DO pvecArr[i,*] = pvec[i] + gridStep[i] * gridVec[ind/gridSize^i MOD gridSize]
		
		;***********************************************************************		
		; Iterate along grid points to find those that minimize the residuals:
		; 1- Get new pointing matrix from the selected grid point
		; 2- Get sample/line coordinates considering new pointing matrix
		; 3- Compute sum of squared residuals: ssr = SUM( (scoord-scent)^2+(lcoord-lcent)^2 )		
		;***********************************************************************
		FOR i=0, npts-1 DO BEGIN
			cspice_eul2m, pvecArr[2,i], 0.5*!DPI-pvecArr[1,i], 0.5*!DPI+pvecArr[0,i], 3, 1, 3, cmat
			image.cmat = cmat
			radec2slcoord, ras, decs, scoords, lcoords, CORRECT_STELAB=image.vobs_stars
			sres = scoords-xcents  &  lres = lcoords-ycents
			ssr[i] = TOTAL(sres*sres + lres*lres)
		ENDFOR
		
		; Get new pointing vector and compute RMS residual:
		min_ssr = MIN(ssr, min_pos)
		pvec_old = pvec
		pvec = pvecArr[*,min_pos]
		
		rms = SQRT(min_ssr/nstars)
		PRINT, FORMAT='(4X,A,G-0.7)', "RMS residual: ",rms
		
		;***********************************************************************
		; Test if pointing vector is on the grid edge, meaning that the global 
		; minimum of the residual could be outside the grid. In this case, 
		; restart with new pointing vector as grid center. Otherwise, refine
		; the grid with new pointing vector as grid center.
		;***********************************************************************
		poff = ABS(pvec - pvec_old)
		IF TOTAL(poff LT pmaxoff) NE nparams THEN BEGIN
			IF ite LT 100 $
			THEN ite++ $
			ELSE MESSAGE, "Pointing algorithm did not converge. Try fitting the orientation."
		ENDIF ELSE BEGIN
			; Test iteration conditions:
			IF pass GT 1 && (ABS(rms-rms_old) LE threshold || pass GT 10) $
			THEN again=0 $
			ELSE BEGIN
				gridStep *= 1.0D/(gridSize-1)	; Refine grid steps
				rms_old = rms
				pass++
				ite=1
			ENDELSE	
		ENDELSE
		
	ENDWHILE
	
	RETURN
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_ITEREPOINT
; PURPOSE:
;	Perform an iterative algorithm to compute the pointing, by minimizing the  
;	distance between catalogs star positions and the those found in the image.
;
; INPUTS: None
; OUPUTS: None
;
; OPTIONAL INPUT KEYWORDS:
;	NSIG: Scalar value. The factor applied to the residuals' standard
;		deviation to define the threshold for removing stars from iterative
;		algorithm. The best value seems to be nsig=2.5. A value of '3' keeps
;		too many bad stars and a value of '2' can reject good stars. 
;
; OPTIONAL KEYWORDS:
;	BLOCK_TWIST: Set this keyword to block the twist angle in the algorithm.
;
; COMMON BLOCKS:
;	caviar_data
;
; EXTERNAL ROUTINES:
;	radec2slcoord.pro
;	display.pro
;	setStruct.pro
;
; MODIFICATIONS:
;	2012				MEUNIER L-E.			OBSPM/IMCCE
;		- Created based on previous CaVIaR routines.
;		- A lot of modifications and optimisations.
;	2013, Oct			MEUNIER L-E.			OBSPM/IMCCE
;		- Add the "BLOCK_TWIST" keyword & possibility to block the twist angle.
;		- Replace the "ERR_MSG" keyword by the IDL "MESSAGE" procedure.
;		  (The user must use IDL "CATCH" procedure to manage errors)
;	2013, Nov			MEUNIER L-E.			OBSPM/IMCCE
;		- Replace the "TOLERANCE" keyword by "NSIGMA" and change the code to 
;		remove from computation stars with residuals greater than n*sigma.
;	2014, Jan			MEUNIER L-E.			OBSPM/IMCCE
;		- Correct bug when only 1 star is selected for computation.
;-------------------------------------------------------------------------------
PRO caviar_iteRepoint, NSIGMA=nsig, $
					   BLOCK_TWIST=notwist, $
					   RESIDUALS=residuals
					   
	COMMON CAVIAR_DATA, image, stars, imgStars, planets, rings
	
	; n-Sigma value for residuals threshold:
	IF NOT KEYWORD_SET(nsig) THEN nsig=2.5
	
	; Get pointing vector coordinates (radian) from the pointing matrix:
	caviar_m2rdt, image.cmat, rastart, decstart, twiststart
	pini = [rastart, decstart, twiststart]
	
	
	;***************************************************************************
	; Iterate until each fitted star residual is below tolerance.
	; Remove the stars with the maximum residual at each iteration.
	;***************************************************************************
	working=1
	WHILE working DO BEGIN
		
		pvec = pini
		
		;***********************************************************************
		; Extract selected stars ra/dec & xcent/ycent.
		; Put to zero the 'FITTED' tag.
		;***********************************************************************
		selStars = LIST() & selStars_indexList = LIST()
		ras = LIST() & decs = LIST() & xcents = LIST() & ycents = LIST()
		FOR i=0, N_ELEMENTS(stars)-1 DO BEGIN
			stari = stars[i]
			IF stari.MATCHED EQ 1 THEN BEGIN
				ras.add,    (stars[i]).RA
				decs.add,   (stars[i]).DEC
				xcents.add, (stars[i]).XCENT
				ycents.add, (stars[i]).YCENT
				selStars_indexList.add, i
			ENDIF
			stari.FITTED = 0
			stars[i] = stari
		ENDFOR
		
		n_selStars = N_ELEMENTS(selStars_indexList)
		IF n_selStars EQ 0 $
		THEN MESSAGE, "No stars have been matched. Cannot perform iterative pointing algorithm."
	
		selStars = {RA:ras.ToArray(), DEC:decs.ToArray(), $
					XCENT:xcents.ToArray(), YCENT:ycents.ToArray()}
				
		;***********************************************************************
		; Get the pointing vector that minimized residual between star's centroid  
		; (from image detection) and star's coordinates (from catalogs):
		;***********************************************************************
		caviar_iterepoint_nPassAlgo, selStars.ra, selStars.dec, $
									 selStars.xcent, selStars.ycent, pvec, $
									 BLOCK_TWIST=notwist
		
		PRINT, FORMAT='( /A/, 2( 3(A,G-0.12,2X),/ ) )', $
			"Pointing vector (radian):", $
			"Before:  RA = ", pini[0],"Dec = " ,pini[1],"Twist = ", pini[2], $
			"After :  RA = ", pvec[0],"Dec = ", pvec[1],"Twist = ", pvec[2]

		
		;***********************************************************************
		; Update pointing matrix with the pointing vector that minimize residuals:
		;***********************************************************************
		cspice_eul2m, pvec[2], 0.5*!dpi-pvec[1], 0.5*!dpi+pvec[0], 3, 1, 3, cmat
		image.cmat = cmat
		
		
		;***********************************************************************
		; Search maximum residuals to compare with tolerance. If the value is 
		; above tolerance then the star will be removed for next iteration.
		;***********************************************************************
		radec2slcoord, selStars.ra, selStars.dec, scoords, lcoords, CORRECT_STELAB=image.vobs_stars
		sres = selStars.xcent - scoords
		lres = selStars.ycent - lcoords
		dres = SQRT(sres*sres + lres*lres)
		
		ssig = STDDEV(sres) & smed = MEDIAN(sres, /EVEN) & skeep = ABS(sres-smed) LE nsig*ssig 
		lsig = STDDEV(lres) & lmed = MEDIAN(lres, /EVEN) & lkeep = ABS(lres-lmed) LE nsig*lsig
		dsig = STDDEV(dres) & dmed = MEDIAN(dres, /EVEN) & dkeep = ABS(dres-dmed) LE nsig*dsig
		keep = skeep AND lkeep AND dkeep
		star2rmv_index = -1
		IF TOTAL(keep) EQ N_ELEMENTS(keep) || N_ELEMENTS(n_selStars) THEN working=0 $
		ELSE star2rmv_index=selStars_indexList[WHERE(keep EQ 0)]
		
				
		;***********************************************************************
		; Update stars values and the image display and print fitting results:
		;***********************************************************************
		PRINT, FORMAT='(/,6X,"STARS",10X,"MAG",4X,"FLUX",7X,"Centroid",6X,"Position fitted",3X,"Residual (cent-fit)",2X,"Keep")'
		PRINT, FORMAT='(6X, "name",21X,2(4X,"sample",4X,"line"),3X,"sample",2X,"line",3X,"total",2X,"sltf")'
		PRINT, FORMAT='(96("-"))'
		
		FOR i=0, N_ELEMENTS(selStars_indexList)-1 DO BEGIN
			stari = stars[selStars_indexList[i]]
			setStruct, stari, ['XRESIDUAL', 'YRESIDUAL', 'FITTED'], [sres[i], lres[i], 1] 
			
;njc			PRINT, FORMAT='(A-18,X,F6.2,X,F6.1,4(X,F8.2),X,3(X,F6.3),2X,4(I1))', $
;njc For UCAC5/GAIA:
                        PRINT, FORMAT='(A-30,X,F6.2,X,F6.1,4(X,F8.2),X,3(X,F6.3),2X,4(I1))', $
					stari.NAME, stari.MAG, stari.FLUX, $
					stari.XCENT, stari.YCENT, scoords[i], lcoords[i], $
					sres[i], lres[i], dres[i], skeep[i], lkeep[i], dkeep[i], keep[i]
			
			IF NOT keep[i] THEN stari.MATCHED = 0
			stars[selStars_indexList[i]] = stari
		ENDFOR
		tsres_sq = TOTAL(sres*sres)
		tlres_sq = TOTAL(lres*lres)
		residuals = {SAMPLE: SQRT( (tsres_sq)/n_selStars ), $
					 LINE:   SQRT( (tlres_sq)/n_selStars ), $
					 TOTAL:  SQRT( ((tsres_sq) + (tlres_sq))/n_selStars), $
					 THRESHOLD: dmed+nsig*dsig}
					 
		PRINT, FORMAT='( /,A,G0,3( /,4X,A-14,5(X,F6.3,2X) ),/ )', $
			"Residuals (pixel):   RMS  | Median |  Sigma | Threshold - NSIG=",nsig, $
			"Sample:", residuals.SAMPLE, smed, ssig, smed-nsig*ssig, smed+nsig*ssig, $
			"Line:  ", residuals.LINE,   lmed, lsig, lmed-nsig*lsig, lmed+nsig*lsig, $
			"Total: ", residuals.TOTAL,  dmed, dsig, dmed-nsig*dsig, dmed+nsig*dsig
		
		
		caviar_updtCoords, stars, image.vobs_stars
		caviar_updtCoords, planets
		caviar_updtCoords, rings
		caviar_display
	ENDWHILE
		
	RETURN
END
