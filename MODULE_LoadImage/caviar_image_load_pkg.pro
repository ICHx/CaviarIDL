;print, "Compiling 'caviar_image_pkg.pro' routines and dependencies:"
@headerxtract2
@dialog_getValue
@caviar_loadspicekernels
@caviar_getParams
@caviar_getCmat_pkg
@caviar_data_routinesWrapper

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: caviar_image_load_spcId
; PURPOSE: 
;	Search in labels for spacecraft name and compare it with spacecraft list
;	compatible with CaVIaR. If it cannot be found in labels, the name or id of
;	the spacecraft will be asked to the user, either with a dialog or a terminal
;	prompt, depending on whether the 'GUI' keyword is set.
;-------------------------------------------------------------------------------
FUNCTION caviar_image_load_spcId, header, GUI=gui, FOUND=found
	
	cspice_boddef, 'MARINER 9', -9
	
	;***************************************************************************
	; Search spacecraft name in the header:
	;***************************************************************************
	vList = LIST()
	vList.ADD, /EXT, headerXtract(header, '^INSTRUMENT_HOST_NAME$', 'STRING')	  ; for Cassini internal header
	vList.ADD, /EXT, headerXtract(header, 'INSTRUMENT_HOST_NAME', 'STRING', '"', '"') ; for Cassini external vicar header
	
	indices = vList.WHERE(!NULL)
	IF indices NE !NULL THEN vList.REMOVE, indices
	vList = vList.toArray()
	
	spcId = 0
	IF N_ELEMENTS(vList) NE 0 THEN BEGIN
		IF TOTAL(STREGEX(vList, 'CASSINI', /BOOL)) GT 0 		 THEN spcId = -82
	ENDIF
	found = (spcId EQ 0) ? 0 : 1
		
	;***************************************************************************
	; Spacecraft ID is not found, warn the user and ask him which one to choose:
	;***************************************************************************
	WHILE NOT found DO BEGIN
		spcList = ['CASSINI # -82']
		lbl = "Cannot find spacecraft name in the header. Select one from the list:"
		IF KEYWORD_SET(gui) THEN BEGIN
			state = dialog_getValue(LABEL=lbl, VALUE=val, LIST_VALUES=spcList)
			val = STRTRIM((STRSPLIT(val, '#', /EXTRACT))[0], 2)
		ENDIF ELSE BEGIN 
			PRINT, lbl
			FOREACH spc, spcList DO PRINT, '    ', spc
			READ, "Enter the spacecraft ID (eg '-82') or press return to quit: ", (val='')
			state = (val EQ '') ? 0 : 1
			IF NOT STREGEX(val, '-', /BOOL) THEN val = '-'+STRTRIM(val, 2)
		ENDELSE
		IF state EQ 0 THEN RETURN, !NULL
		
		; Get spaceraft NAIF ID from user selection:
		cspice_bods2c, val, spcId, found

		IF NOT found THEN BEGIN
			msg = "Invalid spacecraft name or ID!"
			IF KEYWORD_SET(gui) THEN res = DIALOG_MESSAGE(msg, /CENTER) ELSE PRINT, msg
		ENDIF
	ENDWHILE
	
	RETURN, spcId
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_IMAGE_LOAD
; PURPOSE:
;
; INPUTS:
;	IMGFILE: Scalar string with the path to the image file name.
;	IMGTYPE: Scalar string refering to the image type/format.
;                       Ex: "CASSINI"
; OPTIONNAL INPUT KEYWORDS:
;	LABELS_FILE: Scalar string with the path to the label file name.
;	POINTING_FILE: Scalar string with the path to the file containing the 
;		pointing matrix.
;			  If an empty string is given a dialog will ask the user the file. 
; OUTPUTS: 
;	image = Structure variable with following tags:
;		'PATH' = Full path of the image file.
;		'NAME' = Name of the image file with extension.
;		'WINDOW' = Caviar GUI window where is displayed the image, initialized to -1.
;		'HEADER' = 1 or 2-Column(s) array containing the header of the image.
;		'HDRFRMT' = Scalar string representing the header format. 
;			Either 'INTVIC' or 'EXTVIC'
;		'RAWIMG' = 2-Dimensional float array containing the image. 
;		'BYTEIMG' = 2-Dimensional byte array containing the byte scale image.
;		'NS', 'NL' = Scalar long integer. Number of samples/lines of the image.
;		'ET': Scalar double. Image mid-time in ephemeris seconds past J2000.
;		'EXPOSURE' = Exposure time in seconds.
;		'BINNING' = Scalar integer. Image binning.
;		'TARGET' = Structure variable with following tags:
;			'NAME' = Scalar string. Image target name.
;			'ID' = Scalar long integer. Image target ID.
;		'CAM' = Structure variable with following tags:
;			'LNAME' = Camera long name with spacecraft + camera short name.
;			'ID' = Camera NAIF integer code.
;		'SPC' = Structure variable with following tags:
;			'NAME' = Spacecraft name.
;			'ID' = Spacecraft NAIF integer code.
;		'METAKERNEL' = Scalar string. Spacecraft's SPICE metakernel file full path.
;		'FOVPIX' = Scalar double. Field of view of one pixel of the camera (radian).
;		'FOVIMG' = Scalar double. Field of view of the image in the diagonals (degree).
;		'FOCAL' = Scalar double. Focal length of the camera.
;		'CENTER' = 2-Elements array of double. Boresight of the camera.
;		'KMAT': 3 by 2 array of doubles. Camera distortion coefficients matrix.
;		'BIGOMEGA' = Scalar double. Twist misalignment.
;		'EPSILON': 6-Elements array of doubles. Electromagnetic and optical 
;			distortion parameters vector.
;		'CMAT', 'CMAT_INI', 'CMAT_SAVED': 3 by 3 array of doubles. Rotation 
;			matrix corresponding to a specified unit quaternion.
;		'VOBS_STARS': Spacecraft velocity relative to the stars.	
;		
; COMMON BLOCKS:
;	CAVIAR_DATA
;
; MODIFICATIONS: 
;	2014, March		re-written by L-E. MEUNIER (IMCCE/OBSPM)
;-------------------------------------------------------------------------------
FUNCTION caviar_image_load, imgFile, imgType, $
							LABELS_FILE=lblFile, POINTING_FILE=pntgFile, GUI=gui
		
	;***************************************************************************
	; Test inputs:
	;***************************************************************************
	IF ISA(imgFile, 'String') EQ 0 || FILE_TEST(imgFile, /REGULAR) EQ 0 $
	THEN MESSAGE, "'IMGFILE' must be a string representing the image file full path."
	
        goodIMGtypes = ["CASSINI"]
	IF TOTAL(imgType EQ goodIMGtypes) NE 1 $
	THEN MESSAGE, "'IMGTYPE' must be one of these strings: '"+STRJOIN(goodImgTypes, "', '")+"'"
	
	
	
	;***************************************************************************
	; Load image's data:
	;***************************************************************************
	PRINT, FORMAT='(/A)', ">>>>> Start loading "+imgType+" image & header"

	;**********************************************
	; Read image's header from external file:
	IF ISA(lblFile, 'STRING') && FILE_TEST(lblFile, /REGULAR) THEN BEGIN
		PRINT, "Read image's header from file: ", lblFile
		
		; Extract image label from file to an array of the same number of lines:
		OPENR, lun, lblFile, /GET_LUN
		READF, lun, (header = STRARR(FILE_LINES(lblFile)))
		FREE_LUN, lun
		IF STRJOIN(header,/SINGLE) EQ '' $
		THEN MESSAGE, "Cannot extract header from file. File is empty."
		
		print, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxx"
		
		; Determine header's type:
		labels = ["LINE_SAMPLES","LINES","Samples","Lines"]
		types = ['INT','INT','INT','INT']
		values = headerXtract(header, labels, types, "=", '')
		IF values[0] NE !NULL && values[1] NE !NULL THEN BEGIN
			hdrFormat='EXTVIC' & ns = values[0] & nl = values[1]
		ENDIF ELSE IF values[2] NE !NULL && values[3] NE !NULL THEN BEGIN
			hdrFormat='EXTCUB' & ns = values[2] & nl = values[3]
		ENDIF ELSE MESSAGE, "Cannot detect header format."
	ENDIF
	
	;***************************************
	; Extract image array & internal header:
	CASE imgType OF
                "CASSINI" : BEGIN
			; Construct a 'CassImg' object containing image and header with CassImg::Init()
			oImage = OBJ_NEW('CassImg');, DebugFlag=DebugFlag)  
			oImage->ReadVic, imgFile

			; Extract image raw data & convert to 8 bytes array
			rawimg = oImage->Image()

			; Extract image header
			IF header EQ !NULL THEN BEGIN
				header = oImage->LabelArray()
				IF header EQ !NULL || STRJOIN(header, /SINGLE) EQ '' $
                                THEN MESSAGE, "Cannot extract header from CASSINI image file."
				hdrFormat = "INTVIC"
			ENDIF
		END
		
	ENDCASE
	IF N_ELEMENTS(rawimg) LE 1 THEN MESSAGE, "Cannot read "+imgType+" image from file."
		
	;************************
	; Create image structure:
	imgSize = SIZE(rawimg)
	ns = imgSize[1] & nl = imgSize[2]
	image = CREATE_STRUCT('PATH', FILE_DIRNAME(imgFile), $
						  'NAME', FILE_BASENAME(imgFile), 'WINDOW', -1, $
						  'HEADER', header, 'HDRFRMT', hdrFormat, $
						  'RAW',rawimg, 'BYTE',BYTSCL(rawimg), 'NS',ns, 'NL',nl)
	
	PRINT, "<<<<< Image has been loaded succesfully!"
	;if (size(header))[0] EQ 2 THEN print, header ELSE foreach line, header do print, line9
	
	
	;***************************************************************************
	; Load generic SPICE kernels:
	;***************************************************************************
	PRINT, FORMAT='(/A)', ">>>>> Start getting generic SPICE kernels"
        caviar_loadspicekernels, GETENV('SPICEKERNEL_CASSINI'), loaded, TITLE="Select Cassini meta-kernel file:"

	IF loaded THEN PRINT, "<<<<<Cassini kernels have been loaded successfully!" $
	ELSE RETURN, 0
	
	;***************************************************************************
	; Load image information:
	;***************************************************************************
	; Get spacecraft ID from image header or by asking the user
	spcId = caviar_image_load_spcId(header, GUI=gui, FOUND=found)
	IF NOT found THEN BEGIN
		caviar_data_restore
		RETURN, 0
	ENDIF
	
	cspice_bodc2n, spcId, spcName, found
	PRINT, FORMAT='(/A)', ">>>>> Start loading "+spcName+"'s image parameters"
	params = caviar_getParams(spcId, header, hdrFormat, getCmatMethods)

	IF NOT ISA(params, 'STRUCT') THEN BEGIN
		caviar_data_restore
		RETURN, 0
	ENDIF ELSE image = CREATE_STRUCT(image, params)
	PRINT, "<<<<< Image parameters have been loaded successfully!"
	
	
	;***************************************************************************
	; Get image's other parameters from 'cameras_parameters.txt' file:
	;***************************************************************************
;njc 	cp = {FOVPIX:0D, FOCAL:0D, CENTER:DBLARR(2), KMAT:DBLARR(3,2), EPSILON:DBLARR(6)}
        cp = {FOVPIX:0D, FOCAL:0D, CENTER:DBLARR(2), KMAT:DBLARR(3,2), EPSILON:DBLARR(6), BIGOMEGA:0D}
	caviar_readCamParams, params.CAM.LNAME, cp

	fovimg = SQRT(ns*ns+nl*nl) * cp.fovpix * params.binning * !RADEG
	image = CREATE_STRUCT(image, 'FOCAL',cp.FOCAL, 'CENTER',cp.CENTER, 'KMAT',cp.KMAT, $
		'EPSILON',cp.EPSILON, 'FOVPIX',cp.FOVPIX, 'FOVIMG',fovimg, 'BIGOMEGA', cp.BIGOMEGA)
;njc                'EPSILON',cp.EPSILON, 'FOVPIX',cp.FOVPIX, 'FOVIMG',fovimg)

	
	
	;***************************************************************************
	; Get pointing matrix (cmat):
	;***************************************************************************
	cmat = 0
	FOREACH method, getCmatMethods DO BEGIN
		CATCH, error
		IF error THEN BEGIN
			PRINT, !ERROR_STATE.MSG_PREFIX+!ERROR_STATE.MSG
			CONTINUE
		ENDIF
		CASE method OF
			"FILE": IF ISA(pntgFile,'STRING') && pntgFile NE '' $
					THEN cmat = caviar_file2cmat(pntgFile)
			"SPICE": cmat = caviar_spice2cmat(image.SPC.ID, image.CAM.ID, image.CAM.LNAME, image.ET)
			"HEADER": cmat = caviar_lbl2cmat(image.SPC.ID, header, hdrFormat)
			ELSE: cmat = 0
		ENDCASE
		IF N_ELEMENTS(cmat) EQ 9 THEN BEGIN
			image = CREATE_STRUCT(image, 'CMAT',cmat, 'CMAT_INI',cmat, 'CMAT_SAVED',cmat)
			BREAK
		ENDIF
	ENDFOREACH
	IF N_ELEMENTS(cmat) NE 9 THEN BEGIN
		msg = "Cannot find camera pointing matrix."
		PRINT, msg
		res = DIALOG_MESSAGE(msg, /CENTER)
	ENDIF
	
	
	;***************************************************************************
	; Compute spacecraft velocity relative to the Solar System barycenter:
	; (which is equivalent to the velocity relative to the stars)
	;***************************************************************************
	CATCH, error
	IF error NE 0 THEN BEGIN
		pos = STRPOS(!ERROR_STATE.MSG, ']')
		PRINT, !ERROR_STATE.MSG_PREFIX+STRMID(!ERROR_STATE.MSG, 0, pos+1)
		l = STRSPLIT(STRMID(!ERROR_STATE.MSG, pos+2), '(  +)', /REGEX, /EXTRACT)
		l[0] = STRMID(l[0], 0, STRLEN(l[0])-1) 
		PRINT, '  '+STRJOIN(l, ' ')
		PRINT, "Cannot get spacecraft velocity relative to stars. A null velocity will be set!"
		image = CREATE_STRUCT(image, 'VOBS_STARS', [0D,0D,0D])
	ENDIF ELSE BEGIN
		cspice_spkez, image.SPC.ID, image.ET, 'J2000', 'NONE', 0L, state, ltime
		image = CREATE_STRUCT(image, 'VOBS_STARS', state[3:5])
	ENDELSE
	
	RETURN, image
END
