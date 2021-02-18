;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: IMG_EXTEND
; PURPOSE: Enlarge an image (ie. 2D-array) by adding surrounding borders.
;		By default, the borders are black ('0' value) but setting the 'MIRROR'
;		keywords make the border to be the reflection of the image edges.
; INPUT:
;	img: 2D-Array representing an image.
;	w: Scalar interger representing the width of the surrounding borders.
; KEYWORD:
;	MIRROR: Allow to use for surrounding borders, the reflect of the image edges
;		in place of the default '0' value.
; OUTPUT:
;	img_ext: 2D-Array of the same type as 'img' but of dimensions: 
;			 imgXsize+2w, imgYsize+2w
; MODIFICATIONS:
;	2013 August, 2		LE. Meunier (IMCCE/OBSPM)
;		- Created
;-------------------------------------------------------------------------------
FUNCTION img_extend, img, w, MIRROR=MIRROR

	dimx = (SIZE(img))[1]
	dimy = (SIZE(img))[2]
	type = (SIZE(img))[3]
	
	IF w GT dimx || w GT dimy $
	THEN MESSAGE, "Borders width must be smaller than the image dimensions!"
	
	img_ext = MAKE_ARRAY(dimx+2*w, dimy+2*w, TYPE=type)
	img_ext[w, w] = img

	IF KEYWORD_SET(MIRROR) THEN BEGIN
		
		; Do edges:
		left   = img[0:w-1,*]
		right  = img[dimx-w-1:*,*]
		top    = img[*,0:w-1]
		bottom = img[*,dimy-w-1:*]
		
		img_ext[0:w-1, w:w+dimy-1]      = w EQ 1 ? left    : REVERSE(left, 1)
		img_ext[w+dimx-1:*, w:w+dimy-1] = w EQ 1 ? right   : REVERSE(right, 1)
		img_ext[w:w+dimx-1, 0:w-1]      = w EQ 1 ? top     : REVERSE(top, 2)
		img_ext[w:w+dimx-1, w+dimy-1:*] = w EQ 1 ? bottom  : REVERSE(bottom, 2)
		
		; Do corners:
		topleft  = img[0:w-1     ,0:w-1]
		topright = img[dimx-w-1:*,0:w-1]
		botleft  = img[0:w-1     ,dimy-w-1:*]
		botright = img[dimx-w-1:*,dimy-w-1:*]
				
		img_ext[0:w-1     , 0:w-1]      = w EQ 1 ? topleft  : ROTATE(topleft, 2)
		img_ext[w+dimx-1:*, 0:w-1]      = w EQ 1 ? topright : ROTATE(topright, 2)
		img_ext[0:w-1     , w+dimy-1:*] = w EQ 1 ? botleft  : ROTATE(botleft, 2)
		img_ext[w+dimx-1:*, w+dimy-1:*] = w EQ 1 ? botright : ROTATE(botright, 2)
	ENDIF
	
	RETURN, img_ext
END
