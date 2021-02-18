@headerxtract2
@caviar_loadspicekernels

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_GETPARAMS_<Mission Name>
; PURPOSE:
; INPUTS:
;	header:
;	format:
; OUTPUT:
;	params:
;	getCmatMethods:
;-------------------------------------------------------------------------------
FUNCTION caviar_getParams_cassini, header, format, getCmatMethods
	

	;***************************************************************************
	; Get parameters value from header
	CASE format OF
		'INTVIC': BEGIN
                        labels = ['^NS$', '^EXPOSURE_DURATION$', '^IMAGE_TIME$', $
					  '^INSTRUMENT_ID$', 'TARGET_NAME']
			types = ['INT','DOUBLE','STRING','STRING','STRING']
			values = headerXtract(header, labels, types,'',['','','Z','',''])
		END
		'EXTVIC': BEGIN
			labels = ['LINE_SAMPLES', 'EXPOSURE_DURATION', 'IMAGE_MID_TIME', $
					  'INSTRUMENT_ID', 'TARGET_NAME']
			types = ['INT','DOUBLE','STRING','STRING','STRING']
;For LBL files from PDS, the double quotes and Z character are missing from timestrings:
                        values = headerXtract(header, labels, types, ['=','=','=','"','"'], ['','','','"','"'])
;For LBL files from the JPL servers, the timestrings have quotes and Z character:
                        tch=strmid(values[2],0,1)
                        if tch eq '"' then values = headerXtract(header, labels, types, ['=','=','"','"','"'], ['','','Z"','"','"'])
		END
		ELSE: MESSAGE, "Wrong header format."
	ENDCASE

	FOR i=0, N_ELEMENTS(values)-2 DO IF ISA(values[i], /NULL) THEN BEGIN
           INT, "<<<<< Problem reading image header"
           stop
        ENDIF

	binning  = 1024/values[0]
	exposure = values[1]/1000D
	ctime 	 = values[2]
	camNAME	 = values[3]
	tgtName	 = values[4]

	
	IF ISA(tgtName,'STRING') && tgtName NE '' THEN cspice_bodn2c, tgtName, tgtID, found
        IF (DOUBLE(ctime) LT 2001) THEN BEGIN
          tgt = found ? {NAME:tgtName, ID:tgtID} : {NAME:'Jupiter', ID:599}
        ENDIF ELSE BEGIN
          tgt = found ? {NAME:tgtName, ID:tgtID} : {NAME:'Saturn', ID:699}
        ENDELSE

	;***************************************************************************
	; Set spacecraft and camera name and id
	CASE camNAME OF
		'ISSNA': cam = {LNAME: "CASS_ISSNA", ID: -82360L}
		'ISSWA': cam = {LNAME: "CASS_ISSWA", ID: -82361L}
		ELSE: RETURN, -1
	ENDCASE
	spc = {NAME: "CASSINI", ID: -82L}
		
	;***************************************************************************
; All kernels are loaded in caviar_image_load_pkg.pro.
        metaKernel = GETENV('SPICEKERNEL_CASSINI')

	;***************************************************************************
	; Compute the epoch "et" as ephemeris seconds past J2000 (TDB):

	cspice_str2et, ctime, et
;If using INTERNAL header, ctime is IMAGE_TIME (shutter closing time), so subtract 0.5*exposure to get mid-time:  
        if format eq 'INTVIC' then  et -= 0.5*exposure
	
	getCmatMethods = ["FILE","SPICE","HEADER"]
	
	;***************************************************************************

        if tgt EQ !NULL then begin
            params = {}
        endif else begin
            params = CREATE_STRUCT('ET', et, 'EXPOSURE', exposure, 'BINNING', binning, $
            'TARGET', tgt, 'CAM', cam, 'SPC', spc, 'METAKERNEL', metaKernel,'IMGMIDTIME', ctime)  
        endelse

	   					   		   				   					
	RETURN, params
END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: caviar_getParams
; PURPOSE: 
;	- Extract camera informations from image header & 'cameras_parameters.txt'.
;	- Load specific SPICE kernels for the mission
; INPUT: 
;	- 
; OUTPUTS:
;	PARAMS: Structure variable with following tags:
;		ET: Spacecraft time/image middle time in seconds past J2000.
;		EXPOSURE: Exposure time in seconds.
;		BINNING: Scalar integer. Image binning.
;		TARGET: Structure variable with following tags:
;			NAME: Scalar string. Image target name.
;			ID: Scalar long integer. Image target ID.
;		CAM: Structure variable with following tags:
;			LNAME: Camera long name with spacecraft + camera short name.
;			ID: Camera NAIF integer code.
;		SPC: Structure variable with following tags:
;			NAME: Spacecraft name.
;			ID: Spacecraft NAIF integer code.
;		METAKERNEL: Scalar string. Spacecraft's SPICE metakernel file full path.
;-------------------------------------------------------------------------------
FUNCTION caviar_getParams, spcID, header, format, getCmatMethods

	CASE spcID OF
		; Cassini
		-82: params = caviar_getParams_cassini(header, format, getCmatMethods)
		
		ELSE: BEGIN
			cspice_bodc2s, spcId, spcName
			MESSAGE, "Found no function to get "+spcName+"  parameters."
		END
	ENDCASE
	IF ISA(params, 'STRUCT') THEN RETURN, params ELSE RETURN, 0
END
