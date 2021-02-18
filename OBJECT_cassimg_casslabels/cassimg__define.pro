;;	cassimg__define.pro
;;	Package defining CassImg objects and access methods
;;	Kevin Beurle	8th September 1998
;;	Version tag: $Id: cassimg__define.pro,v 1.1 2004/02/16 16:18:44 mwe Exp $

;	A CassImg object contains a Cassini image and related information
;	The ImageP member is a pointer to the image itself
;		(which is a 2D array of pixel values)
;	The OtherP member is a pointer to the "Binary prefix" data
;		(also a 2D array of values, 24 bytes per image line)
;	There are also members describing the image parameters
;		which are set up from the labels read with the image

;	CassImg::Image()
;	Access function returning a copy of the image array
FUNCTION CassImg::Image
;	Bail out if no data available
  imbuff = 0
  IF ((self.NS EQ 0) OR (self.NL EQ 0)) THEN BEGIN
    CISSCAL_Log, 'No valid image size [samples,lines]: [', self.NS, self.NL, ']'
  ENDIF ELSE IF NOT PTR_VALID(self.ImageP) THEN BEGIN
    CISSCAL_Log, 'No valid image pointer'
  ENDIF ELSE BEGIN
    imbuff = INTARR(self.NS, self.NL)
    imbuff = *self.ImageP
  ENDELSE
  RETURN,imbuff
END


;	CassImg::Other()
;	Access function returning a copy of the "binary prefix array"
FUNCTION CassImg::Other

;	Bail out if no data available
  IF ((self.NBB EQ 0) OR (self.NL EQ 0)) THEN RETURN,0
  IF NOT PTR_VALID(self.OtherP) THEN RETURN, 0
;	otherwise return the data
  OtherBuff = INTARR(self.NBB, self.NL)
  OtherBuff = *self.OtherP
  RETURN, OtherBuff
END

;	CassImg::Overclocks()
;	Access function returning a copy of the overclocked pixel values
FUNCTION CassImg::Overclocks

;	Bail out if NL is 0
  IF (self.NL EQ 0) THEN RETURN, 0
;	Return a zero vector if no overclocks available,
;	 otherwise return a copy of the vector of overclocks
  OverClkBuff = INTARR(self.NL)
  IF PTR_VALID(self.OverClkP) THEN OverClkBuff = FIX(*self.OverClkP)
  RETURN, OverClkBuff
END

;	CassImg::Saturated()
;	Access function returning a copy of the "saturated pixels array"
FUNCTION CassImg::Saturated
;	Bail out if no data available
  IF NOT PTR_VALID(self.SaturatedP) THEN RETURN, 0
;	otherwise return the data
  SaturatedBuff = *self.SaturatedP
  RETURN, SaturatedBuff
END

;	CassImg::Missing()
;	Access function returning a copy of the "missing pixels array"
FUNCTION CassImg::Missing
;	Bail out if no data available
  IF NOT PTR_VALID(self.MissingP) THEN RETURN, 0
;	otherwise return the data
  MissingBuff = *self.MissingP
  RETURN, MissingBuff
END

;	CassImg::Name()
;	Access function returning a copy of the image name (image no. as string)
FUNCTION CassImg::name
  RETURN, STRING(self.ImgNo)
END

;	CassImg::BinaryHdr()
;	Access function returning a copy of the "binary header"
FUNCTION CassImg::BinaryHdr
  BinaryHdr = BYTARR(self.Recsize)
  BinaryHdr = *self.BinaryHdrP
  RETURN, BinaryHdr
END



;########## CassImg::LabelArray()
; Access function returning a [[Key, val], [Key, val]...] array
FUNCTION CassImg::LabelArray
	IF OBJ_VALID(self.Labels) EQ 0 THEN BEGIN
		CISSCAL_Log, 'No valid labels object'
		RETURN, !NULL 
	ENDIF ELSE RETURN, self.Labels->LabelArray()
END
;########## CassImg::HaveLabel()
; CassImg interfaces for other CassLabels methods
FUNCTION CassImg::HaveLabel, KeyWord
	RETURN, self.Labels->Have(KeyWord)
END
;########## CassImg::GetLabel()
FUNCTION CassImg::GetLabel, KeyWord
	RETURN, self.Labels->Get(KeyWord)
END
;########## CassImg::SetLabel()
; Intentionally no support for /NEW keyword here
FUNCTION CassImg::SetLabel, KeyWord, Value, Quoted
	RETURN, self.Labels->Set(KeyWord, Value, Quoted)
END
;########## CassImg::ListLabels
; Utility method to log out the label parameter members and the size of the binary header.
; If keyword /BINARY is set, then also log out the contents of the binary header array.
PRO CassImg::ListLabels
	CISSCAL_Log, 'Labelsize is ', self.Labels->Get('LBLSIZE')
	CISSCAL_Log, 'RECSIZE is ', self.RecSize
	CISSCAL_Log, 'INSTRUMENT_ID is ', self.Instrument
	CISSCAL_Log, 'NL is ', self.NL
	CISSCAL_Log, 'NS is ', self.NS
	CISSCAL_Log, 'NLB is ', self.NLB
	CISSCAL_Log, 'NBB is ', self.NBB
	CISSCAL_Log, 'FORMAT is ', self.VicFormat
	CISSCAL_Log, 'INTFMT is ', self.VicIntFmt
	CISSCAL_Log, 'REALFMT is ', self.VicRealFmt
	CISSCAL_Log, 'IMAGE_NUMBER is ', LONG(self.ImgNo)
	CISSCAL_Log, 'EXPOSURE_DURATION is ', self.ExpDur
	CISSCAL_Log, 'OFFSET (precalculated bias) is ', self.Offset
	CISSCAL_Log, 'DETECTOR_TEMPERATURE is ', self.DetT
	CISSCAL_Log, 'OPTICS_TEMPERATURE is ', self.OptT
	CISSCAL_Log, 'INSTRUMENT_MODE_ID is ', self.InstModID
	CISSCAL_Log, 'GAIN_MODE_ID, Gainstate are ', self.GainModID, ',', self.GainState
	CISSCAL_Log, 'ENCODING_TYPE is ', self.EncType
	CISSCAL_Log, 'CONVERSION_TYPE is ', self.ConvType
	CISSCAL_Log, 'LIGHT_FLOOD_STATE_FLAG is ', self.LfFlag
	CISSCAL_Log, 'ANTIBLOOMING_STATE_FLAG is ', self.AbFlag
	CISSCAL_Log, 'CALIB_LAMP_STATE_FLAG is ', self.ClFlag
	CISSCAL_Log, 'TARGET_NAME is ', self.ObsID
	CISSCAL_Log, 'OBSERVATION_ID is ', self.Target
	CISSCAL_Log, 'MISSING_LINES is ', self.MissLns
	CISSCAL_Log, 'Filter 1 is ', self.Filter1
	CISSCAL_Log, 'Filter 2 is ', self.Filter2
	CISSCAL_Log, 'Binary header of', N_ELEMENTS(*self.BinaryHdrP), ' bytes'
	IF KEYWORD_SET(BINARY) THEN CISSCAL_Log, 'Binary hdr:', *self.BinaryHdrP
	RETURN
END

;########## CassImg::MakeRpt()
; Utility method for debugging, etc.
; Returns a string containing a summary of the image parameters
FUNCTION CassImg::MakeRpt
  hk_line = STRING( self.ImgNo, ' ', self.ObsID, ' ', self.InstModID, ' ', $
	LONG(self.ExpDur), ' ', $
	self.GainModID, ' ', 'F1x', ' ', 'F2x', ' ', self.AbFlag, ' ', $
	self.ConvType, ' ', self.EncType, ' ', $
	self.OptT, ' ', self.DetT, ' ', self.Filter1, ' ', self.Filter2, ' ', self.MissLns)
  RETURN, hk_line
END

;########## CassImg::DNRange()
; Utility method to return DN extrema of an image
; Returns a two element array containg [min, max] DN values of image
; if keyword /ARRAY is set otherwise it returns a string
FUNCTION CassImg::DNRange, TEXT=text
	IF KEYWORD_SET(ARRAY) THEN RETURN, [ MIN(*self.ImageP) , MAX(*self.ImageP) ]

	DNRange = '[' + STRING(MIN(*self.ImageP)) + ' ' + STRING(MAX(*self.ImageP)) + ' ]'
	RETURN, DNRange
END




;########## CassImg::Init()
; Runs automatically during execution of OBJ_NEW('CassImg')
; Return TRUE if we're happy, otherwise FALSE
FUNCTION CassImg::Init, Param, DebugFlag=DebugFlag
	Status = 1		; TRUE means we're happy
	IF (N_PARAMS() GT 0) THEN BEGIN
		; Do initialisation based on parameter(s): use it as a filename
		CISSCAL_Log, 'Automatically initialising CassImg object from ' + PARAM
		self->ReadVic,Param
	ENDIF
	IF KEYWORD_SET(DebugFlag) THEN self.ObjDebugLevel = DebugFlag
	RETURN, Status
END

;########## CassImg::Cleanup()
; Runs automatically during OBJ_DESTROY of a CassImg object
PRO CassImg::Cleanup
	; Use this opportunity to release any pointer variables
	; we were using for image/other buffers
	CISSCAL_Log, 'Cleanup for destruction of a CassImg...'
	IF (PTR_VALID(self.BinaryHdrP)) THEN PTR_FREE,self.BinaryHdrP
	IF (PTR_VALID(self.ImageP)) 	THEN PTR_FREE,self.ImageP
	IF (PTR_VALID(self.OtherP)) 	THEN PTR_FREE,self.OtherP
	IF (PTR_VALID(self.OverClkP)) 	THEN PTR_FREE,self.OverClkP
	IF (PTR_VALID(self.SaturatedP)) THEN PTR_FREE,self.SaturatedP
	IF (PTR_VALID(self.MissingP)) 	THEN PTR_FREE,self.MissingP
	IF OBJ_VALID(self.Labels) THEN OBJ_DESTROY,self.Labels
END




;########## CassImg::Define
; Class structure definition for CassImg objects
PRO CassImg__Define
	tmp = { CassImg, $					; Define the CassImg class structure:
			
			; Properties intoduced by/for our processing...
			Calibrated: 0, $			; Interlock to prevent reprocessing
			PerSecond: 0, $				; Has it been divided by exposure?
			ObjDebugLevel: 0, $			; Turn object debugging info OFF
			
			; Properties from the image labels...
			Instrument: '', $			; Which instrument ISSNA/ISSWA
			ImgNo: LONG(0), $			; Image number
			ExpDur: 0.0, $				; Exposure duration
			DetT: 0.0, OptT: 0.0, $		; Detector and Optics temperatures
			InstModID: '', $			; Instrument and modes
			GainModID: '', $			; Gain mode...
			GainState: 0, $				;  and gain state derived from it
			EncType: '', ConvType: '', $; Encoding and Conversion types
			LfFlag: '', AbFlag: '', $	; Lightflood and Antiblooming flags
			ClFlag: '', $				; Calibration lamp flag
			Target: '', ObsID: '', $	; Target name and Observation ID
			Filter1: '', Filter2: '', $	; Filters in wheels 1 and 2
			MissLns: 0, $				; Number of Missing lines
			RecSize: 0, $				; Record size used in the file
			LeadIn: 0, $				; Number of bytes of start-of-file labels read
			NL: 0, NS: 0, $				; Image size: No. of Lines and Samples
			NLB: 0, NBB: 0, $			; Size of "prefix" on each line
			VicFormat: '', $			; VICAR format used
			VicIntFmt: '', $			; Integer representation "ENDIAN"-ness
			VicRealFmt: '', $			; Floating point representation style
			Offset: '', $				; Offset (bias level) may be embedded
		
			; For structures of unknown size, create pointers and use them 
			; to reference dynamic structures on the heap
			BinaryHdrP: PTR_NEW(), $			; Pointer to a record created on the heap
			ImageP:PTR_NEW(), ImgBytes:0L, $	; Pointer to an image created on the heap
												; typically 1024*1024 or 512*512 (SUM2) or 256*256 (SUM4)
			OtherP:PTR_NEW(), $					; Pointer to the "other" image bytes: these are the
												; "binary prefix" bytes on each line containing 
												; e.g. overclocked pixels
			OverClkP:PTR_NEW(), $
			SaturatedP:PTR_NEW(), $
			MissingP:PTR_NEW(), $
		
			; Finally, include a CassLabels object which we will use to hold
			; the entire set of labels as a simulated associative array.
			; Labels:OBJ_NEW('CassLabels') $
			Labels:OBJ_NEW() $
		  }
END
