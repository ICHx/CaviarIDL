;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;	SUBIMGXTRACT
; PURPOSE:
;	Extract a sub-image from the input image 'img'.
;	Center of the sub-image in 'img' is given by input variable 'center'.
;	Size of the sub-image is given by 'xsize' and 'ysize' input variables.
;	If the center and the sub-image size make it overflow the input image, black 
;	border are used to fill the sub-image covering its external area.
;
; CALLING SEQUENCE:
;	subimgxtract, img, center, xsize, ysize[, GET_FRAME=frame]
; INPUTS:
;	img: 2 or 3-dimensionnal image in which to extract the sub-image. For 3-d 
;		image, the first dimension correspond to the 3 channel R,V,B.
;	center: 2-Elements array. Coordinate [xcoord, ycoord] of the sub-image 
;		center in input image.
;	xsize/ysize: Scalar integer. Size of the output sub-image.
; OUTPUTS:
;	subimg: Sub-image of the input image with the dimensions 'xsize' & 'ysize'.
; OPTIONNAL OUTPUT KEYWORDS:
;	GET_FRAME: Structure with tags: 'xmin', 'ymin', 'xmax', ymax', corresponding 
;		to the lower-left and upper-right point of the sub-image area extract 
;		from the input image. Can be used to display the extracted area in input 
;		image window.
;
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; EXTERNAL CALL:
;	None.
; MODIFICATION HISTORY:
;	2012, october			L-E. MEUNIER		OBSPM>IMCCE
;		- Created base on 'get_img2zoom' function
;		- Compatible with 3-channel image in first dimension. Usefull with TVRD 
;		function!
;		- Add 'GET_FRAME' keyword to get sub-image frame on input image
;	2014, september			L-E. MEUNIER		OBSPM>IMCCE
;		- Manage possible issue if input arguments type are incorrect
;-------------------------------------------------------------------------------
FUNCTION subimgxtract, img, center, xsize, ysize, GET_FRAME=frame
;FUNCTION extract_csubimg, img, center, xsize, ysize, GET_FRAME=frame
	
	IF ISA(img, /ARRAY, /NUMBER) EQ 0 $
	THEN MESSAGE, "Incorrect type for argument 'img'. Must be 2 or 3-dimensionnal array of float, long, integer or byte."
	
	IF ISA(center, /ARRAY, 'INT') EQ 0 AND ISA(center, /ARRAY, 'LONG') EQ 0 $
	THEN MESSAGE, "Incorrect type for argument 'center'. Must be a 2-elements array of type long or integer."
	
	IF ISA(xsize, /SCALAR, 'INT') EQ 0 AND ISA(xsize, /SCALAR, 'LONG') EQ 0 $
	THEN MESSAGE, "Incorrect type for argument 'xsize'. Must be a scalar of type long or integer."
	
	IF ISA(ysize, /SCALAR, 'INT') EQ 0 AND ISA(ysize, /SCALAR, 'LONG') EQ 0 $
	THEN MESSAGE, "Incorrect type for argument 'ysize'. Must be a scalar of type long or integer."
	
	
	; Get input image dimension and initialize the sub-image:
	imgInfo = SIZE(img, /STRUCTURE)
	
	IF imgInfo.N_DIMENSIONS NE 2 AND imgInfo.N_DIMENSIONS NE 3 $
	THEN MESSAGE, "Incorrect dimensions for 'img' argument. Must be a 2 or 3-dimensionnal array"
	
	;nc = img.DIMENSIONS[imgInfo.N_DIMENSIONS-3]	; Number of channel
	ns = imgInfo.DIMENSIONS[imgInfo.N_DIMENSIONS-2]		; Number of sample
	nl = imgInfo.DIMENSIONS[imgInfo.N_DIMENSIONS-1]		; Number of line

	; Create a sub-image array of the same type as the image and with given size:
	subimg = MAKE_ARRAY(xsize, ysize, TYPE=imgInfo.TYPE)
	
	; Get virtual coordinates of the lower-left & upper-right corners of the
	; sub-image in the full-image array, using its center's position and its size:
	xmin_index = center[0] - FLOOR((xsize-1)/2)
	ymin_index = center[1] - FLOOR((ysize-1)/2)
	xmax_index = xmin_index + xsize-1
	ymax_index = ymin_index + ysize-1
	
	; As sub-image's corners virtual coordinates can exceed full-image dimensions 
	; we must constrain these to the part that can be extracted from the full-image:
	xmin = xmin_index > 0L
	ymin = ymin_index > 0L
	xmax = xmax_index < (ns-1)
	ymax = ymax_index < (nl-1)
	frame = {xmin:xmin, ymin:ymin, xmax:xmax, ymax:ymax}
	
	; Lower-left (xstart,ystart) & upper-right (xstop,ystop) coordinates of 
	; sub-image in which will be copied full-image's visible part.
	xstart = 0L > (-xmin_index)
	ystart = 0L > (-ymin_index)
	xstop = (xsize-1) < (xsize-1 - (xmax_index-(ns-1)) )
	ystop = (ysize-1) < (ysize-1 - (ymax_index-(nl-1)) )
	
	; Put visible part of the full-image in the sub-image:
	IF imgInfo.N_DIMENSIONS EQ 3 $
	THEN subimg[*, xstart:xstop,ystart:ystop] = img[*, xmin:xmax, ymin:ymax] $
	ELSE subimg[xstart:xstop,ystart:ystop] = img[xmin:xmax, ymin:ymax]


	RETURN, subimg
END
