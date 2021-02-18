;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: caviar_data_save
; PURPOSE: Save caviar data. Useful if an error occurs when loading the new one.
;-------------------------------------------------------------------------------
PRO caviar_data_save
	COMMON CAVIAR_DATA, image, catStars, imgStars, planets
	COMMON CAVIAR_OLDDATA, image_old, catStars_old, imgStars_old, planets_old
	
	IF ISA(image) 	 THEN image_old	   = image
	IF ISA(catStars) THEN catStars_old = catStars
	IF ISA(imgStars) THEN imgStars_old = imgStars
	IF ISA(locStars) THEN locStars_old = locStars
	IF ISA(planets)  THEN planets_old  = planets
	IF ISA(rings) 	 THEN rings_old	   = rings
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: caviar_data_reset
; PURPOSE: Reset current caviar data
;-------------------------------------------------------------------------------
PRO caviar_data_reset, ALL=ALL,$
					  RST_IMAGE=RST_IMAGE, $
					  RST_CATSTARS=RST_CATSTARS, $
					  RST_IMGSTARS=RST_IMGSTARS, $
					  RST_PLANETS=RST_PLANETS, $
   					  RST_RINGS=RST_RINGS, $
					  UPDATE_DISPLAY=UPDATE_DISPLAY
					  

	COMMON CAVIAR_DATA, image, catStars, imgStars, planets, rings
	
	IF KEYWORD_SET(ALL) THEN BEGIN
		RST_IMAGE 	 = 1
		RST_CATSTARS = 1
		RST_IMGSTARS = 1
		RST_PLANETS  = 1
		RST_RINGS    = 1
	ENDIF
	
	IF KEYWORD_SET(RST_IMAGE) 	 THEN BEGIN
		image    = {}
		IF KEYWORD_SET(UPDATE_DISPLAY) THEN caviar_display
	ENDIF
	IF KEYWORD_SET(RST_CATSTARS) 	 THEN BEGIN
		catStars    = LIST()
		IF KEYWORD_SET(UPDATE_DISPLAY) THEN caviar_display
	ENDIF
	IF KEYWORD_SET(RST_IMGSTARS) THEN BEGIN
		imgStars = LIST()
		IF KEYWORD_SET(UPDATE_DISPLAY) THEN caviar_display
	ENDIF
	IF KEYWORD_SET(RST_PLANETS)  THEN BEGIN
		planets  = LIST()
		IF KEYWORD_SET(UPDATE_DISPLAY) THEN caviar_display
	ENDIF
        IF KEYWORD_SET(RST_RINGS)  THEN BEGIN
                rings  = LIST()
                IF KEYWORD_SET(UPDATE_DISPLAY) THEN caviar_display
        ENDIF
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: caviar_data_restore
; PURPOSE: Restore previously saved caviar data. Useful if an error occurs when 
;	loading a new image.
;-------------------------------------------------------------------------------
PRO caviar_data_restore
	COMMON CAVIAR_DATA, image, catStars, imgStars, planets
	COMMON CAVIAR_DATA_OLD, image_old, catStars_old, imgStars_old, planets_old
	
	IF ISA(image_old) 	 THEN image	   = image_old
	IF ISA(catStars_old) THEN catStars = catStars_old
	IF ISA(imgStars_old) THEN imgStars = imgStars_old
	IF ISA(planets_old)  THEN planets  = planets_old
	IF ISA(rings_old) 	 THEN rings	   = rings_old
	
	cspice_kclear
        caviar_loadSPICEKernels, GETENV('SPICEKERNEL_CASSINI')
	IF haveTag(image, 'METAKERNEL') THEN caviar_loadSPICEKernels, image.METAKERNEL
END
