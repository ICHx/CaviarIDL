;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;       sigma_clip
;
; PURPOSE:
;       Compute the standard deviation of the noise in an image
;       
; CATEGORY:
;       Signal processing
;
; CALLING SEQUENCE:
;       sigma = sigma_clip(cube)
;
; INPUTS:
;       image : an images
;
; PROCEDURE:
;       3 sigma clipping method
;
; MODIFICATION HISTORY:
;       Written by:     E.pantin - CEA		04/05/02
;       
;-------------------------------------------------------------------------------
FUNCTION sigma_clip, image_i, $
					 background, $
					 NSIG=nsig, NOMEDIAN=nomedian, $
					 INDEX=ind_noise, INDEX_REJ=index_rej, $
					 VERBOSE=verbose

	niter = 5
	IF NOT KEYWORD_SET(nsig) THEN nsig=3.2

	; For a cube, we concatenate the cubes in a bigger image:
	imgSize = SIZE(image_i)
	IF imgSize[0] EQ 3 THEN BEGIN  
		img = FLTARR(imgSize[1]*imgSize[3],imgSize[2])
		FOR i=0, imgSize[3]-1 DO img[i*imgSize[1]:(i+1)*imgSize[1]-1,*] = image_i(*,*,i)
	ENDIF ELSE img=image_i

	; Remove dimensions of size 1:
	img = REFORM(img)

	; Remove infinite and nan values:
	ind_finite = WHERE(FINITE(img))
	img = img[ind_finite]

	IF NOT KEYWORD_SET(nomedian) THEN img = img-MEDIAN(img,3)

	sigma = STDDEV(img)
	background = MEAN(img)
	;print, sigma, background

	FOR i=0, niter-1 DO BEGIN
	  ind_noise = WHERE(ABS(img-background) LE nsig*sigma, nc)
	  IF nc GE 1 THEN BEGIN
	  	sigma = STDDEV(img[ind_noise])
	  	background = MEAN(img[ind_noise])
	  	IF KEYWORD_SET(verbose) THEN PRINT, FORMAT='("New sigma =",G0,2X,"New mean = ",G0)', sigma, background
	  ENDIF
	ENDFOR

	index_rej = WHERE(ABS(img-background) GT nsig*sigma, nrej)

	background = background+median(image_i)

	IF NOT KEYWORD_SET(nomedian) THEN sigma/=0.94	;to correct for median filtering

	RETURN, sigma

END
