;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; List of procedures/functions of this file:
;	- PRO			caviar_imgstars_display
;	- FUNCTION		caviar_imgstars_find_centroid
;	- FUNCTION		caviar_imgstars_find_centroid2
;	- FUNCTION		caviar_imgstars_find
;	- FUNCTION		caviar_imgstars_gui_deftblvalues
;	- PRO       	caviar_imgstars_gui_event
;	- PRO			caviar_imgstars_gui
;-------------------------------------------------------------------------------

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_IMGSTARS_DISPLAY
; PURPOSE: 
;-------------------------------------------------------------------------------
PRO caviar_imgstars_display, imgstars, dispParams, nl, offset, ZOOMFACTOR=zf
	
	; Test inputs
	IF dispParams.state NE 1 THEN RETURN
	
	n = N_ELEMENTS(imgstars)
	IF n EQ 0 THEN RETURN
	
	IF ~KEYWORD_SET(zf) THEN zf=1
		
	; Get stars position
	x = DBLARR(n)
	y = DBLARR(n)
	FOR i=0, n-1 DO BEGIN
		x[i] = zf * ( (imgstars[i]).xcent ) - offset[0]
		y[i] = zf * ( (nl-1) - (imgstars[i]).ycent ) - offset[1]
	ENDFOR
	
	; Display data
	color = color(dispParams.color, /SILENT)	;Be careful, color function changes the color table!
	psym    = dispParams.psym
	symsize = dispParams.symsize
	PLOTS, x, y, /DEVICE, COLOR=color, PSYM=psym, SYMSIZE=symsize
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_IMGSTARS_FIND_CENTROID
; PURPOSE: Compute the centroid of Gaussian sources
; NOTE: Original centroid computation routine of CaVIaR, based on DAOPHOT.
;-------------------------------------------------------------------------------
FUNCTION caviar_imgstars_find_centroid, img_box, nbox, middle, ix, iy
	dd = INDGEN(nbox-1)+0.5-middle				
	w = 1.0 - 0.5*(ABS(dd)-0.5) / (middle-0.5)
	sumw = TOTAL(w)
	ir = (middle-1) > 1
	
	;***************************************************************************
	; Compute X centroid
	derivatx = SHIFT(img_box,-1,0)-img_box
	derivatx = TOTAL(derivatx[0:nbox-2, middle-ir:middle+ir], 2)
	sumd 	= TOTAL(w*derivatx)
	sumxd 	= TOTAL(w*derivatx*dd)
	sumxsq 	= TOTAL(w*dd*dd) 
	IF sumxd GE 0 THEN RETURN, -1 			;Cannot compute X centroid
	
	dx = sumxsq*sumd/(sumw*sumxd)		
	IF ABS(dx) GT middle THEN RETURN, -1	;X centroid too far from local X maximum

	;***************************************************************************
	; Compute Y centroid	
	derivaty = SHIFT(img_box,0,-1)-img_box 
	derivaty = TOTAL(derivaty[middle-ir:middle+ir, 0:nbox-2], 1)
	sumd 	= TOTAL(w*derivaty)
	sumxd 	= TOTAL(w*derivaty*dd)
	sumxsq 	= TOTAL(w*dd*dd) 
	IF sumxd GE 0 THEN RETURN, -1 			;Cannot compute Y centroid

	dy = sumxsq*sumd/(sumw*sumxd)
	IF ABS(dy) GT middle THEN RETURN, -1
	
	;***************************************************************************
	; Convert back to big image coordinates and returns:
	RETURN, [ix-dx, iy-dy]	
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_IMGSTARS_FIND_CENTROID2
; PURPOSE: Compute the centroid of Gaussian sources
; NOTE:
;	The centroid computation was modified in March 2008 and now differs from 
;	DAOPHOT which multiplies the correction dx by 1/(1+abs(dx)). 
;	The DAOPHOT method is more robust (e.g. two different sources will not merge)
;	especially in a package where the centroid will be subsequently 
;	redetermined using PSF fitting. However, it is less accurate, and introduces
;	biases in the centroid histogram. The change here is the same made in the 
;	IRAF DAOFIND routine.
;	(see http://iraf.net/article.php?story=7211&query=daofind )
;   Added to CaVIaR durring summer 2013 by MEUNIER L-E. (IMCCE)
;-------------------------------------------------------------------------------
FUNCTION caviar_imgstars_find_centroid2, img_box, nbox, middle, gker, sigsq, ix, iy

	xwt = FLTARR(nbox,nbox)
	wt = middle - ABS(INDGEN(nbox)-middle) + 1
	FOR i=0,nbox-1 DO xwt[0,i] = wt
	ywt = TRANSPOSE(xwt) 
	p = TOTAL(wt)
	
	sgx = TOTAL(gker*xwt,1)
	sgy = TOTAL(gker*ywt,2)
		
	vec = middle - FINDGEN(nbox) 
	dgdx = sgy*vec
	dgdy = sgx*vec
	
	
	;***************************************************************************
	; Find X centroid:
	; Comupte is the height of the best-fitting marginal Gaussian: HX.
	; If this is not positive then the centroid does not make sense!
	sumgx   = TOTAL(wt*sgy)
	sumgsqy = TOTAL(wt*sgy*sgy)
	sdgdx = TOTAL(wt*dgdx) 
	sgdgdx  = TOTAL(wt*sgy*dgdx)
	sdgdxs  = TOTAL(wt*dgdx*dgdx)
	
	sd      = TOTAL(img_box*ywt,2)
	sumgd   = TOTAL(wt*sgy*sd)
	sumd    = TOTAL(wt*sd)
	
	hx = (sumgd - sumgx*sumd/p) / (sumgsqy - sumgx*sumgx/p)
	IF hx LE 0 THEN RETURN, -1
		
	sddgdx = TOTAL(wt*sd*dgdx)
	pskylvl = sumd - hx*sumgx	; = p * sky level
	dx = (sgdgdx - (sddgdx-sdgdx*(hx*sumgx + pskylvl)))/(hx*sdgdxs/sigsq)
	IF ABS(dx) GE middle THEN RETURN, -1 
	
	;***************************************************************************
	; Find Y centroid                 
	sumgy   = TOTAL(wt*sgx)
	sumgsqx = TOTAL(wt*sgx*sgx)
	sdgdy   = TOTAL(wt*dgdy)
	sdgdys  = TOTAL(wt*dgdy*dgdy)
	sgdgdy  = TOTAL(wt*sgx*dgdy)
	
	sd = TOTAL(img_box*xwt,1)
	sumgd = TOTAL(wt*sgx*sd)
	sumd = TOTAL(wt*sd)

	hy = (sumgd - sumgy*sumd/p) / (sumgsqx - sumgy*sumgy/p)
	IF hy LE 0 THEN RETURN, -1
	
	sddgdy = TOTAL(wt*sd*dgdy)
	pskylvl = (sumd - hy*sumgy)	; = p * sky level
	dy = (sgdgdy - (sddgdy-sdgdy*(hy*sumgy + pskylvl)))/(hy*sdgdys/sigsq)
	IF ABS(dy) GE middle THEN RETURN, -1 
	 
	;***************************************************************************
	; Convert back to big image coordinates and returns:
	RETURN, [ix+dx, iy+dy]	
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;	CAVIAR_IMGSTARS_FIND
; PURPOSE:
;	Find positive brightness perturbations (i.e stars) in an image 
; EXPLANATION:
;	Also returns centroids and shape parameters (roundness & sharpness).
;	Adapted from 1986 STSDAS version of DAOPHOT.
;
; CALLING SEQUENCE:
;	FIND_STARS, image [, x] [, y] [, flux] [, sharp] [, round] [, hmin] [, fwhm]
;					 [, roundlim] [, sharplim] [, PRINT2FILE=print] [, /SILENT]
;	
; INPUTS:
;	image: 2 dimensional image array (integer or real) for which one you want to
;		   find gaussian objects like stars.
;
; OPTIONAL INPUTS:
;	FIND_STARS will prompt for these parameters if not supplied.
;
;	hmin: Threshold intensity for a point source. Should generally be 3 or 4 
;		sigma above background.
;	fwhm: FWHM to be used in the convolve filter.
;	sharplim: 2-elements vector giving low and high cutoff for the sharpness statistic.
;		Default: [0.2,1.0]. Change this default only if the stars have 
;			significantly larger or smaller concentration than a Gaussian.
;	roundlim: 2-elements vector giving low and high cutoff for the roundness statistic.
;		Default: [-1.0,1.0]. Change this default only if the stars are 
;			significantly elongated.
; OPTIONAL INPUT KEYWORDS:
;	SILENT: Normally, each star that meets all selection criteria are write out.
;		If the SILENT keyword is set and non-zero, then this printout is suppressed.
;	PRINT2FILE: If set and non-zero, results will be writen into 'FIND.PRT' file.
;		Also one can specify a different output file name by setting print='filename'.
;		
; OUTPUT:
;	struct = {X:FLTARR(nstarsfound), Y:FLTARR(nstarsfound), $
;			  FLUX:FLTARR(nstarsfound), SHARPNESS:FLTARR(nstarsfound), $
;			  ROUNDNESS:FLTARR(nstarsfound)}
;		x/y: Vector containing x/y position of all stars identified by FIND_STARS
;		flux: Vector containing flux of identified stars as determined
;		      by a gaussian fit.  Fluxes are NOT converted to magnitudes.
;		sharp: Vector containing sharpness statistic for identified stars
;		round: Vector containing roundness statistic for identified stars
;
; NOTES:
;	The sharpness statistic compares the central pixel to the mean of the 
;	surrounding pixels. If this difference is greater than the originally 
;	estimated height of the Gaussian or less than 0.2 the height of the Gaussian 
;	(for the default values of SHARPLIM) then the star will be rejected.
;
; REVISION HISTORY:
;	Written 									W. Landsman, STX  	Feb.	1987
;	ROUND now an internal function in V3.1  	W. Landsman			July	1993
;	Change variable name DERIV to DERIVAT   	W. Landsman			Feb.	1996
;	Use /PRINT2FILE keyword instead of TEXTOUT  W. Landsman 		May 	1996
;	Changed loop indices to type LONG       	W. Landsman 		Aug.	1997
;	Converted to IDL V5.0   					W. Landsman 		Sept.	1997
;	2012 January		L-E. Meunier		OBSPM>IMCCE
;		- Replace some 'goto' to 'repeat... until', cosmetics and comments
;	2012 April			L-E. Meunier		OBSPM>IMCCE
;		- Move statistics and centroid computation to seperate subfunctions
;		- Improve print and printf format
;	2013 January		L-E. Meunier		OBSPM>IMCCE
;		- Replace DATATYPE() by IDL function ISA()
;	2013 July			L-E. Meunier		OBSPM>IMCCE
;		- Change main from PROCEDURE to FUNCTION. Now return a structure with
;		  x, y, flux, sharpness and roundness arrays.
;		- Remove the use of 'GETOPT'
;	2013 November		L-E. Meunier		OBSPM>IMCCE
;		- Replace 'Hmin' by 'nsig' keyword. The threshold for the convolved
;		image is know 'nsig*STDDEV(h)'.
;       2015 October            Nick Cooper             IMCCE/QMUL
;               - Reversed the previous change. Not clear why this change was made.
;                 The threshold for the convolved image is now 'nsig', not 'nsig*STDDEV(h)'. 
;                 This gives results closer to the 'f' routine in the QMUL version of 
;                 caviar.
;-------------------------------------------------------------------------------
FUNCTION caviar_imgstars_find, image, fwhm, nsig, roundlim, sharplim, $
						   CENTROID_METHOD=cntrd_meth, $
						   REMOVE_SATURATED=rmvSat, $
                		   PRINT=print, SILENT=silent, $
                		   MONITOR=monitor, STATISTIC=stat
                
   	;***************************************************************************
	; Test inputs and keywords:
	;***************************************************************************
	nparams = N_PARAMS()
	IF nparams EQ 0 THEN BEGIN
		PRINT, 'Syntax! FIND_STARS, image[, nsig][, fwhm][, roundlim][, sharplim]'+ $
			'[, roundlim][, sharplim][, PRINT=doprint][, /SILENT][, /MONITOR]'
		RETURN, {}
	ENDIF
	
	IF NOT KEYWORD_SET(cntrd_meth) THEN cntrd_meth=1
	
	doprint = KEYWORD_SET(print)			
	silent  = KEYWORD_SET(silent)
	monitor = KEYWORD_SET(monitor)
	
	stat = {}
	
	img_size = size(image)
	IF img_size[0] NE 2 THEN MESSAGE, $
		'ERROR! Image array (first parameter) must be 2 dimensional'
		
	nx=img_size[1]
	ny=img_size[2]
	
;	To allow detection of chopped stars (Saturated stars with "black" maxima):	
;	rclose=1
;	elmt = SHIFT(DIST(2*rclose+1), rclose, rclose) LE rclose
;	img_ext = img_extend(image, rclose, /MIRROR)
;	img_ext = MORPH_CLOSE(img_ext, elmt, /GRAY)
;	image = img_ext[rclose:rclose+nx-1, rclose:rclose+ny-1] 
	
;	winIni = !D.WINDOW
;	window, 2, XSIZE=1024, YSIZE=1024
;	TV, REBIN(image, 2*nx, 2*ny, SAMPLE=1), /ORDER
;	wset, !D.window
	
	IF NOT silent THEN BEGIN
		PRINT, "++> START FINDING STARS IN IMAGE..."
		IF NOT SILENT THEN $
			PRINT, 'Input image size is '+STRTRIM(nx, 2) + ' by '+ STRTRIM(ny, 2)
	ENDIF
	
	IF N_ELEMENTS(fwhm) NE 1 THEN BEGIN
		READ, 'Enter approximate FWHM (>0.5 pixel): ', fwhm
		IF fwhm LT 0.5 THEN MESSAGE, 'Supplied FWHM must be at least 0.5 pixels.'
	ENDIF
	
	IF N_ELEMENTS(nsig) NE 1 THEN $
		READ, 'Enter minimum value above background for threshold detection: ',nsig
		
	
	IF N_ELEMENTS(sharplim) NE 2 THEN BEGIN
		PRINT, 'Sharpness statistic cutoffs (range=[0.0, 1.0]):'
		REPEAT BEGIN
			READ, '-> Enter "low high" or press [RETURN] for defaults ("0.2 1.0"):', (ans='')
		  	sharplim = ans EQ '' ? [0.2,1.0] : FLOAT(STRSPLIT(ans,/EXTRACT))
			IF N_ELEMENTS(sharplim) NE 2 THEN PRINT, 'ERROR! Expecting 2 scalar values'
		ENDREP UNTIL N_ELEMENTS(sharplim) EQ 2
	ENDIF
	
	IF N_ELEMENTS(roundlim) NE 2 THEN BEGIN
		PRINT, 'Roundness statistic cutoffs (range=[-1.0, 1.0]):'
		REPEAT BEGIN
			READ, '-> Enter "low high" or press [RETURN] for defaults ("-1.0 1.0"):', (ans='')
		  	roundlim = ans EQ '' ? [-1.0,1.0] : FLOAT(STRSPLIT(ans,/EXTRACT))
			IF N_ELEMENTS(roundlim) NE 2 THEN PRINT, 'ERROR! Expecting 2 scalar values'
		ENDREP UNTIL N_ELEMENTS(roundlim) EQ 2
	ENDIF

	
	;***************************************************************************
	; Construction of the Gaussian convolution kernel 'gker':
	;***************************************************************************
	maxbox = 13 						;Maximum size of convolution box in pixels 
	radius = 0.637*fwhm > 2.001      	;Radius is 3-sigma/2
	middle = FIX(radius) < (maxbox-1)/2 ;Index of the convolution box's central pixel
	nbox = 2*middle + 1					;Number of pixels for convolution box sides
		
	gker = FLTARR(nbox,nbox)
	row2 = (FINDGEN(nbox)-middle)^2
	FOR i=0, middle DO gker[*,middle-i] = (gker[*,middle+i] = row2 + i*i)
	mask = gker LE radius*radius		;Mask identifies valid pixels in convolution box
	good = WHERE(mask, npix)				
	sigsq = ( fwhm/2.35482 )^2
	
	gker = exp(-0.5*gker/sigsq)				;Make gker into a gaussian kernel
	
	c = mask * gker
	meanc = TOTAL(c)/npix					;Expectation of c
	varc = TOTAL(c*c)/npix - meanc*meanc	;Variance of c
	c = mask * (c-meanc)/(npix*varc) 

	IF NOT silent THEN $ 
		PRINT, 'Relative error computed from FWHM: '+STRING( SQRT(TOTAL(c*c)), FORMAT='(F-7.4)')
		
	
	; Make Gaussian kernel for roundness statistics:
	c1 = exp(-0.5*row2/sigsq)
	meanc1 = TOTAL(c1)/nbox							;Expectation of c1
	varc1 = TOTAL(c1*c1)/nbox - meanc1*meanc1		;Variance of c1
	c1 = (c1-meanc1)/(nbox*varc1)
	
;	In original program it was this:
;	varc1 = TOTAL(c1*c1) - meanc1
;	c1 = (c1-meanc)/varc1
;	But seems to remove good detection!
	
	;***************************************************************************
	; Convolution of the image with kernel "c":
	;***************************************************************************
	h = CONVOL(FLOAT(image), c)    				

	; Makes border of the convolved image equal to zero (avoid border effects)
	h[0:middle-1,*]=0 & h[nx-middle:nx-1,*]=0	
	h[*,0:middle-1]=0 & h[*,ny-middle:ny-1]=0

;	h_histo = HISTOGRAM(h, MIN=0, MAX=10, BINSIZE=0.5)
;	plot, INDGEN(N_ELEMENTS(h_histo))/2., h_histo, /YLOG, PSYM=10, $
;		XRANGE=[0,10], YRANGE=[0.1, MAX(h_histo)], XTICKINTERVAL=2, $
;		XTICKLEN=-0.01, YTICKLEN=-0.01, XSTYLE=9, YSTYLE=9, $
;		XMARGIN=[4,1], YMARGIN=[2,2], $
;		BACKGROUND=255, COLOR=0, CHARSIZE=0.9, TITLE="H histogram"	
	
	;***************************************************************************
	; Search for values above standard deviation in the convolved image:
	;***************************************************************************
;	nhsig = nsig*STDDEV(h)
;cnjc This gives results closer to the 'f' routine in the QMUL version of Caviar.
        nhsig = nsig

	index = WHERE(h GE nhsig, nfound)  
		
	IF nfound EQ 0 $	; Found no maxima
	THEN MESSAGE, 'No maxima exceed threshold ('+STRING(nhsig,'(G0)')+')' 
	
	;***************************************************************************
	; Search for local maxima:
	;***************************************************************************
	mask[middle,middle] = 0			; From now on we exclude the central pixel
	npix--      					; so the number of valid pixels is reduced by 1.
	good = WHERE(mask)      		; "good" identifies position of valid pixels.
	xx = (good MOD nbox) - middle	; x and y coordinate of valid pixels relative to the center.
	yy =  FIX(good/nbox) - middle   
	offset = yy*nx + xx
	
	FOR i=0L, npix-1 DO BEGIN                           
		stars = WHERE(h[index] GE h[index+offset[i]], nlocalmaxfound)
		IF nlocalmaxfound LE 0 $	;Do valid local maxima exist?
		THEN MESSAGE, 'No local maxima exceed threshold ('+STRING(nhsig,'(G0)')+')'
		index = index[stars]
	ENDFOR 
	ix = index MOD nx              ;X index of local maxima
	iy = index/nx                  ;Y index of local maxima
	ngood = N_ELEMENTS(index)       
	IF NOT silent THEN PRINT, 'Local maxima located above threshold: '+STRTRIM(ngood,2)
	
	;***************************************************************************	
	; Compute statistics and reject bad sources:
	;***************************************************************************
	;Initialize variables and arrays:
	x         = FLTARR(ngood)
	y         = FLTARR(ngood)
	flux      = FLTARR(ngood)
	sharpness = FLTARR(ngood)
	roundness = FLTARR(ngood)
	nstar     = 0L       	;NSTAR counts all stars meeting selection criteria
	badsharp  = 0L
	badround  = 0L
	badcntrd  = 0L
	saturated = 0L
	
	; Create output file?
	IF doprint THEN BEGIN			
		file = ISA(print, 'STRING') ? 'find.prt' : print+'.prt'
		IF NOT silent THEN PRINT, 'Results will be written to the file '+file
		OPENW, lun, file, /GET_LUN
		PRINTF, lun, FORMAT='(A, %"\n", 2(A,G0,%"\n"), 2(A,G0,A,G0,%"\n"), A,G0)', $
			' Program: CAVIAR_IMGSTARS_FIND '+systime(), $
			' Approximate FWHM:', fwhm, $
			' Threshold above background:', nhsig, $
			' Sharpness Limits: Low', sharplim[0], '  High', sharplim[1], $
			' Roundness Limits: Low', roundlim[0], '  High', roundlim[1], $
			' No of sources above threshold', ngood
	ENDIF   
	
	header_text = "  STAR        X           Y       FLUX    SHARP    ROUND"
	IF doprint THEN PRINTF, lun, header_text
	rslt_format = '(I5, 2F12.2, F9.1, 2F9.2)'
	
		
	; Iterate through sources and compute statistics:
	FOR i = 0L,ngood-1 DO BEGIN   
		;**********
		; Extract image box around local maxima and his pixel intensity:
		;**********
		img_box = FLOAT( image[ix[i]-middle:ix[i]+middle, iy[i]-middle:iy[i]+middle] ) 
		fluxi = h[ix[i],iy[i]]                      
		
		;**********
		; Compute Roundness:
		;**********
		dx = TOTAL( TOTAL(img_box,2)*c1 )   
		dy = TOTAL( TOTAL(img_box,1)*c1 )
		IF dx LE 0 || dy LE 0 THEN BEGIN	
			badround++ & CONTINUE 			;Cannot compute roundness
		ENDIF
		roundi = 2*(dx-dy)/(dx+dy)
		IF roundi LT roundlim[0] || roundi GT roundlim[1] THEN BEGIN
			badround++ & CONTINUE 			;Does not meet roundness criteria
		ENDIF
		
		;**********
		; Compute Sharpness # sharp = (max_flux - mean_flux)/total_flux
		;**********
		sharpi = (img_box[middle,middle] - (TOTAL(mask*img_box))/npix)/fluxi
		IF sharpi LT sharplim[0] || sharpi GT sharplim[1] THEN BEGIN 
			badsharp++ & CONTINUE			;Does not meet sharpness criteria
		ENDIF
		
		;**********
		; Find centroid position:
		;**********
		CASE cntrd_meth OF
			1: centroid = caviar_imgstars_find_centroid(img_box, nbox, middle, ix[i], iy[i])
			2: centroid = caviar_imgstars_find_centroid2(img_box, nbox, middle, gker, sigsq, ix[i], iy[i])
		ENDCASE
		IF centroid[0] EQ -1 THEN BEGIN		;Cannot compute centroid or centroid to far from local maxima
			badcntrd++ & CONTINUE        
		ENDIF
		
		;**********
		; Saturation condition:
		;**********
		IF KEYWORD_SET(rmvSat) && MAX(img_box) EQ 255 THEN BEGIN	;image[ix[i], iy[i]]
			saturated++ & CONTINUE
		ENDIF
		
		
		;**********
		; This star has met all selection criteria. Save results and print out
		;**********
		x[nstar]         = centroid[0]
		y[nstar]         = centroid[1]
		flux[nstar]      = fluxi
		sharpness[nstar] = sharpi
		roundness[nstar] = roundi
		
		IF (NOT silent) AND monitor THEN BEGIN
			IF nstar MOD 80L EQ 0L THEN PRINT, FORMAT='(%"\n%s")', header_text
			PRINT, FORMAT=rslt_format, nstar, x[nstar], y[nstar], fluxi, sharpi, roundi
		END
		IF doprint THEN PRINTF, lun, FORMAT=rslt_format, $
							nstar, x[nstar], y[nstar], fluxi, sharpi, roundi

		nstar++
	ENDFOR
	
	x = x[0:nstar-1]
	y = y[0:nstar-1]
	flux = flux[0:nstar-1]
	sharpness = sharpness[0:nstar-1]
	roundness = roundness[0:nstar-1]
				  
				  	
	txt0 = "Threshold (nsig*stddev(h)): "
	txt1 = "# pixel above threshold: "
	txt2 = "Local maxima found: "
	txt3 = "Sources rejected by SHARPNESS criteria: "
	txt4 = "Sources rejected by ROUNDNESS criteria: "
	txt5 = "Sources rejected by CENTROID  criteria: "
	txt6 = "Sources kept: "
	IF doprint NE 0 THEN BEGIN
		PRINTF, lun, FORMAT='(/, G0.2, /, 6(A, I-, /), 100("_"))', $
				txt0, nhsig, txt1, nfound, txt2, ngood, txt3, badsharp, $
				txt4, badround, txt5, badcntrd, txt6, nstar, $
				
		FREE_LUN, lun
	ENDIF
	IF (NOT silent) AND monitor THEN BEGIN
		PRINT, FORMAT='(/, A, G0.2, /, 6(A, I-, /), 100("_"))', $
				txt0, nhsig, txt1, nfound, txt2, ngood, txt3, badsharp, $
				txt4, badround, txt5, badcntrd, txt6, nstar
				
	ENDIF
	stat = {THRESHOLD:nhsig, NFOUND:nfound, NGOOD:ngood, $
			BADSHARP:badsharp, BADROUND:badround, BADCNTRD:badcntrd, $
			SATURATED:saturated, NSOURCE:nstar}
	
	; Any stars found?
	IF nstar EQ 0 $		
	THEN RETURN, {} $             
	ELSE RETURN, {X:x, Y:y, FLUX:flux, SHARPNESS:sharpness, ROUNDNESS:roundness}                   
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_IMGSTARS_GUI_DEFTBLVALUES
; PURPOSE: Return the find stars parameters table string array, filled with
;	default values.
;-------------------------------------------------------------------------------
FUNCTION caviar_imgstars_deftblvalues
	;		   FWHM  -	   NSIG  -       ROUNDLIM        SHARPLIM
	RETURN, [ ['1.3',''], ['3.0',''], ['-1.0','1.0'], ['0.2','1.0'] ]
END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_IMGSTARS_GUI_INIT
; PURPOSE: Set default values/labels/parameters/etc. of the GUI.
;-------------------------------------------------------------------------------
PRO caviar_imgstars_gui_init
	COMMON CAVIAR_IMGSTARS, wFSRtxt
	WIDGET_CONTROL, wFSRtxt, SET_VALUE=" "
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_IMGSTARS_GUI_EVENT
; PURPOSE: Manage events of the widget
;-------------------------------------------------------------------------------
PRO caviar_imgstars_gui_event, event
	
	COMMON CAVIAR_DATA, image, catstars, imgStars, planets
	COMMON CAVIAR_PARAMS, dispParams, imgDraw, dispProName, starsCatalogs, find_imgStars_params
	
	CATCH, Error_status
	IF Error_status NE 0 THEN BEGIN
		res = DIALOG_MESSAGE(!ERROR_STATE.MSG, DIALOG_PARENT=event.HANDLER, /CENTER, /ERROR)
		RETURN	
	ENDIF

	; Get state from the first child of the compound widget root:
	WIDGET_CONTROL, event.HANDLER, GET_UVALUE=pstate	
	WIDGET_CONTROL, /HOURGLASS
	
	uname=WIDGET_INFO(event.ID, /UNAME)
	
	IF uname EQ "RESTORE" THEN BEGIN
		tblValues = caviar_imgstars_deftblvalues()
		WIDGET_CONTROL, (*pstate).wFSPtbl, SET_VALUE=tblValues
	ENDIF
	
	IF uname EQ "FIND" || uname EQ "PARAMS" || uname EQ "RESTORE" THEN BEGIN
		WIDGET_CONTROL, (*pstate).wFSPtbl, GET_VALUE=tblValues
		params = {FWHM:FLOAT(tblValues[0,0]), NSIG:FLOAT(tblValues[0,1]), $
			 ROUNDLIM:FLOAT(tblValues[*,2]), SHARPLIM:FLOAT(tblValues[*,3])}
		find_imgStars_params = params
		
		WIDGET_CONTROL, (*pstate).wFSObgroup, GET_VALUE=options
		
		fnd = caviar_imgstars_find(image.RAW, params.FWHM, params.NSIG, $
						params.ROUNDLIM, params.SHARPLIM, CENTROID_METHOD=2, $
						REMOVE_SATURATED=options[0], /MONITOR, STATISTIC=stat)

		nstars = fnd EQ {} ? 0 : N_ELEMENTS(fnd.X)
		imgStars = LIST()
		FOR i=0, nstars-1 DO BEGIN	
			star = {XCENT:fnd.X[i], YCENT:fnd.Y[i], FLUX:fnd.FLUX[i], $
					SHARPNESS:fnd.SHARPNESS[i], ROUNDNESS:fnd.ROUNDNESS[i]}
			imgStars.add, star, /EXTRACT
		ENDFOR
		
		
		; Set the find stars' results text widget value:
		IF stat EQ !NULL THEN text='' ELSE $
			text = ["  Threshold = "+STRING(stat.THRESHOLD, '(G0.2)'),$
					"# > Threshold:"+STRING(stat.NFOUND, '(I5)'), $
					"Local maxima :"+STRING(stat.NGOOD, '(I5)'), $;"", $
					"Bad SHARPNESS:"+STRING(stat.BADSHARP, '(I5)'), $
					"Bad ROUNDNESS:"+STRING(stat.BADROUND, '(I5)'), $
					"Bad CENTROID :"+STRING(stat.BADCNTRD, '(I5)'), $
					"Saturated    :"+STRING(stat.SATURATED, '(I5)'), $;"", $
					"Sources found:"+STRING(stat.NSOURCE, '(I5)')]			   
		WIDGET_CONTROL, (*pstate).wFSRtxt, MAP=1, SET_VALUE=text

		; Re-initilize catStars "MATCHED" tag, GUIs and parameters state:
		FOR i=0, N_ELEMENTS(catstars)-1 DO BEGIN 
			stari = catstars[i]
			stari.MATCHED = 0
			catstars[i] = stari
		ENDFOR
		caviar_repoint_gui_init
		caviar_matchStars_gui_init
		dispParams.imgstars.state = 1
		
		; Display imgStars
		caviar_display
	ENDIF
		
	IF uname EQ "CLOSE" THEN BEGIN
		PTR_FREE, pstate
		WIDGET_CONTROL, event.TOP, /DESTROY
	ENDIF
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:	CAVIAR_IMGSTARS_GUI
; PURPOSE: Create a graphical interface to search for stars in the image. 
;		This interface allows FWHM, HMIN, ROUNDNESS and SHARPNESS 
;		parameters to be changed.
;		
; CALLING SEQUENCE:
;		caviar_imgstars_gui [, GROUP_LEADER=widgetID] [, PARENT=widgetID]
;				[, TITLE=string] [, XOFFSET=xoffset] [, YOFFSET=yoffset] 
;				[, FRAME=width] [, SPACE=pixels] [, XPAD=pixels] [, YPAD=pixels]
;
; INPUTS: None.
; OPTIONAL KEYWORDS:
;	PARENT: Set this keyword to the parent widget id in witch you want to place 
;			this compound widget.
; OUTPUT: None.
;	
; COMMON BLOCKS:
;	CAVIAR_DATA, CAVIAR_PARAMS
;   
; PROCEDURE CALLS:
;	caviar_display				in caviar_display.pro
;		
; MODIFICATIONS:
;	2012, March			MEUNIER L-E				OBSPM>IMCCE
;		- Written
;	2013, July			MEUNIER L-E				OBSPM>IMCCE
;		- Remove 'default params' columns and add 'Reset default params' button.
;		- Remove possibility to change the 'print' and 'silent' options that are
;		  now both set to 0.
;	2013, January		MEUNIER L-E				OBSPM>IMCCE
;		- Simplify management of find stars parameters and table widget values.
;		- Change routine names: 'fingStars' becomes 'imgstars'
;-------------------------------------------------------------------------------
PRO caviar_imgstars_gui, PARENT=wMAINbase, GROUP_LEADER=groupLeader, $
			XOFFSET=xoffset, YOFFSET=yoffset, TITLE=title, $
			FRAME=frame, XPAD=xpad, YPAD=ypad, SPACE=space
	
	COMMON CAVIAR_DATA, image, catstars, imgStars, planets
	COMMON CAVIAR_IMGSTARS, wFSRtxt
			
	; Test if widget window is already launched
	IF(XRegistered('caviar_imgstars_gui') NE 0) THEN RETURN
        	
    ;***************************************************************************
	; Initialisation of table values and parameters
    ;***************************************************************************
    tblValues = caviar_imgstars_deftblvalues()
	rowLabels = ['FWHM', 'NSIG', 'Roundness', 'Sharpness']
	editstate = INTARR(2,4)
	editstate[0,*] = 1 & editstate[1,2:3] = 1
    bgColor = BYTARR(3,2,4)
    FOR i=0,2 $
    DO bgColor[i,*,*] = editstate*!COLOR.white[i] + (1-editstate)*!COLOR.light_gray[i]

  	nstars = N_ELEMENTS(imgStars)
	options = 1
	
	;***************************************************************************
	; Define the widget base:
	;***************************************************************************
	IF NOT KEYWORD_SET(title) THEN title = '2. Find image stars'
	
	IF KEYWORD_SET(wMAINbase) THEN BEGIN
		IF NOT WIDGET_INFO(wMAINbase, /VALID_ID) $
		THEN res=DIALOG_MESSAGE('Invalid parent widget identifier.',/ERROR)
		isTopBase = 0
	ENDIF ELSE BEGIN
		isTopBase = 1
		IF NOT KEYWORD_SET(groupLeader) $
		THEN wMAINbase = WIDGET_BASE(TITLE=title, XOFFSET=xoffset, YOFFSET=yoffset) $

		ELSE BEGIN
			IF WIDGET_INFO(groupLeader, /VALID_ID) $
			THEN wMAINbase = WIDGET_BASE(TITLE=title, XOFFSET=xoffset, YOFFSET=yoffset, $
										GROUP_LEADER=groupLeader, /FLOATING) $
			ELSE res=DIALOG_MESSAGE('Invalid group leader widget identifier.',/ERROR)
		ENDELSE
	ENDELSE
	
	;***************************************************************************
	; Define subwidgets tree:
	;***************************************************************************
	wEXTRAbase = WIDGET_BASE(wMAINbase, /COL, XPAD=xpad, YPAD=ypad)

	IF NOT isTopBase THEN wLbl = WIDGET_LABEL(wEXTRAbase, VALUE=title, /ALIGN_CENTER)
	
	wbase = WIDGET_BASE(wEXTRAbase, /ROW, FRAME=frame, XPAD=0, YPAD=2, SPACE=space)
		base1 = WIDGET_BASE(wbase, /COLUMN, SPACE=0)
		base2 = WIDGET_BASE(wbase, /COLUMN, /ALIGN_BOTTOM, SPACE=2)
	
	wFSbttn = WIDGET_BUTTON(base1, VALUE="Find", UNAME="FIND", YSIZE=28);, XSIZE=70
	wFSRtxt = WIDGET_TEXT(base1, VALUE="      Results", XSIZE=19, YSIZE=8)
	
	wFSObgroup = CW_BGROUP(base2, "Remove saturated", UNAME="OPTIONS", YPAD=1, $
								SET_VALUE=options, /NONEXCLUSIVE)
	wFSPtbl = WIDGET_TABLE(base2, ALIGNMENT=1, /NO_COLUMN_HEADERS, UNAME="PARAMS", $
								ROW_LABELS=rowLabels, VALUE=tblValues, $
								EDITABLE=editstate, BACKGROUND_COLOR=bgColor, $
								COLUMN_WIDTH=[35,35], ROW_HEIGHTS=18*[1,1,1,1])			
	wRSTbttn = WIDGET_BUTTON(base2, UNAME='RESTORE', VALUE="Restore defaults")
	
	
								
	IF isTopBase THEN wCLOSEbttn = WIDGET_BUTTON(wEXTRAbase, VALUE="CLOSE", UNAME="CLOSE")
		
	; Create the widget (i.e. display it)
	WIDGET_CONTROL, wMAINbase, /REALIZE
	
	state = {wFSPtbl:wFSPtbl, wFSObgroup:wFSObgroup, wFSRtxt:wFSRtxt}
	WIDGET_CONTROL, wEXTRAbase, SET_UVALUE=PTR_NEW(state, /NO_COPY)
			
	; Let XMANAGER takes control of widgets event processing
	XMANAGER, 'caviar_imgstars_gui', wEXTRAbase, /JUST_REG, GROUP_LEADER=groupLeader
END
