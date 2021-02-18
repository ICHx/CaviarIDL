;;	cassimg__readlabels.pro
;;	Function method to read labels from a Cassini VICAR image file
;;	Kevin Beurle	September 1998
;;	Version tag: $Id: cassimg__readlabels.pro,v 1.1 2004/02/16 16:18:44 mwe Exp $

FUNCTION GetLabelBuf, Lun, Offset, LabBuf
	TokenL = 12
	LabBuf =STRING(REPLICATE(32B, TokenL))

	ON_IOERROR, bad_io
	POINT_LUN, Lun, Offset
	READU, Lun, LabBuf
	ON_IOERROR, NULL

	I = STRPOS(LabBuf, 'LBLSIZE=')
	IF ( I EQ -1 ) THEN BEGIN
	CISSCAL_Log, 'Cannot read label length'
	RetVal = 0
	ENDIF ELSE BEGIN
	LabSize = FIX ( STRMID(LabBuf, STRLEN('LBLSIZE='), TokenL ) )
	IF NOT KEYWORD_SET(Quiet) THEN CISSCAL_Log, 'Labelsize is ', LabSize

	LabBuf =STRING(REPLICATE(32B, LabSize))
	ON_IOERROR, bad_io
	POINT_LUN, Lun, Offset	; Reposition file
	READU, Lun, LabBuf		;  and grab all labels
	ON_IOERROR, NULL
	;    RetVal = LabBuf
	RetVal = 1
	ENDELSE

	RETURN, RetVal

	bad_io:		; Catch I/O exceptions here and return bad status
	ON_IOERROR, NULL
	Status = 0
	CLOSE, Lun
	CISSCAL_Log, 'I/O Error caught: ', !ERR_STRING
	HELP, CALLS=CallTree
	CISSCAL_Log, 'Called from: ', CallTree[0], CallTree[1], CallTree[2]
	RETURN, Status
END

;###################################################################################################
; The NotCass keyword means we should assume this is just a generic VICAR file and not
; actually a Cassini image. Therefore don't try to use any but the most basic labels.
; IF KEYWORD_SET(Quiet) THEN CISSCAL_Log, 'ReadLabels: Quiet option selected'
FUNCTION CassImg::ReadLabels, fname, Quiet=Quiet, NotCass=NotCass

	OPENR, 1, fname
	Status = GetLabelBuf(1, 0, LabelBuf)
	IF ( Status EQ 0 ) THEN CISSCAL_Log, 'Couldn''t read labels' $
	ELSE BEGIN

		; Make a CassLabels object and get it to parse the label buffer into itself
		; (but first release any existing CassLabels if we reference one)
		IF OBJ_VALID(self.Labels) THEN OBJ_DESTROY,self.Labels
		self.Labels = OBJ_NEW('CassLabels', LabelBuf, DebugFlag=DebugFlag, Quiet=Quiet)

		; Copy some label information to local members
		self.RecSize = self.Labels->Get('RECSIZE')
		self.NL  	 = self.Labels->Get('NL')
		self.NS  	 = self.Labels->Get('NS')
		self.NLB 	 = self.Labels->Get('NLB')
		self.NBB 	 = self.Labels->Get('NBB')

		; Deal with possible trailing labels after image
		LabSize = self.Labels->Get('LBLSIZE')
		self.LeadIn = LabSize
		IF self.Labels->Have('EOL') THEN BEGIN
			IF self.Labels->Get('EOL') GT 0 THEN BEGIN
				; Skip past labels, binary header and image
				EolOffset = LONG(Labsize) + ( self.NLB + self.NL ) * LONG(self.RecSize)
				CISSCAL_Log, 'Looking for EOL labels at offset', EolOffset
				Status = GetLabelBuf(1, EolOffset, EolBuf)
				IF ( STATUS GT 0 ) THEN self.Labels->ParseAll, EolBuf, Quiet=Quiet
			ENDIF
		ENDIF
		
		IF self.ObjDebugLevel THEN self.Labels->LogAll

		; Make our own TASK area for any extra labels we might add to the image
		junk = self.Labels->Set('TASK', 'CISSCAL', 1, New=1) 
		junk = self.Labels->Set('USER', GETENV('USER'), 1, New=1) 
		junk = self.Labels->Set('DAT_TIM', SYSTIME(0), 1, New=1) 

		self.VicFormat  = self.Labels->Get('FORMAT')
		self.VicIntFmt  = self.Labels->Get('INTFMT')
		self.VicRealFmt = self.Labels->Get('REALFMT')
		
		IF NOT KEYWORD_SET(NotCass) THEN BEGIN
			self.ImgNo 		= LONG(self.Labels->Get('IMAGE_NUMBER'))
			self.ExpDur 	= FLOAT(self.Labels->Get('EXPOSURE_DURATION'))
			self.DetT 		= FLOAT(self.Labels->Get('DETECTOR_TEMPERATURE'))
			self.OptT 		= FLOAT(self.Labels->Get('OPTICS_TEMPERATURE'))
			self.InstModID 	= self.Labels->Get('INSTRUMENT_MODE_ID')
			self.GainModID 	= self.Labels->Get('GAIN_MODE_ID')
			
			; The format of the 'value' part of these key-value pairs
			; changed between ground testing and ICO, then again before Jupiter
			IF ((self.GainModID EQ '1400K') OR (self.GainModID EQ '215 e/DN') $
				OR (self.GainModID EQ '215 ELECTRONS PER DN')) THEN BEGIN
				self.GainState = 0
			ENDIF ELSE IF ((self.GainModID EQ '400K') OR (self.GainModID EQ '95 e/DN') $
				OR (self.GainModID EQ '95 ELECTRONS PER DN')) THEN BEGIN
				self.GainState = 1
			ENDIF ELSE IF ((self.GainModID EQ '100K') OR (self.GainModID EQ '29 e/DN') $
				OR (self.GainModID EQ '29 ELECTRONS PER DN')) THEN BEGIN
				self.GainState = 2
			ENDIF ELSE IF ((self.GainModID EQ '40K') OR (self.GainModID EQ '12 e/DN') $
				OR (self.GainModID EQ '12 ELECTRONS PER DN')) THEN BEGIN
				self.GainState = 3
			ENDIF ELSE BEGIN
				CISSCAL_Log, 'Warning: unexpected Gain State value: ' + self.GainModID
			ENDELSE

			self.LfFlag 	= self.Labels->Get('LIGHT_FLOOD_STATE_FLAG')
			self.AbFlag 	= self.Labels->Get('ANTIBLOOMING_STATE_FLAG')
			self.Target  	= self.Labels->Get('TARGET_NAME')
			self.ObsID 		= self.Labels->Get('OBSERVATION_ID')
			self.MissLns 	= self.Labels->Get('MISSING_LINES') EQ 'N/A' ? $
												0 : FIX(self.Labels->Get('MISSING_LINES'))
			self.Instrument = self.Labels->Get('INSTRUMENT_ID')

			; Accommodate changed keywords as of Fomalhaut/Jupiter (mid 2000)
			IF self.Labels->Have('ENCODING_TYPE') THEN BEGIN
				self.EncType = self.Labels->Get('ENCODING_TYPE')
			ENDIF ELSE IF self.Labels->Have('INST_CMPRS_TYPE') THEN BEGIN
				self.EncType = self.Labels->Get('INST_CMPRS_TYPE')
			ENDIF ELSE CISSCAL_Log, 'Cannot determine encoding type'

			IF self.Labels->Have('CONVERSION_TYPE') THEN BEGIN
				self.ConvType = self.Labels->Get('CONVERSION_TYPE')
			ENDIF ELSE IF self.Labels->Have('DATA_CONVERSION_TYPE') THEN BEGIN
				self.ConvType = self.Labels->Get('DATA_CONVERSION_TYPE')
			ENDIF ELSE CISSCAL_Log, 'Cannot determine conversion type'

			IF self.Labels->Have('CALIB_LAMP_STATE_FLAG') THEN BEGIN
				self.ClFlag = self.Labels->Get('CALIB_LAMP_STATE_FLAG')
			ENDIF ELSE IF self.Labels->Have('CALIBRATION_LAMP_STATE_FLAG') THEN BEGIN
				self.ClFlag = self.Labels->Get('CALIBRATION_LAMP_STATE_FLAG')
			ENDIF ELSE CISSCAL_Log, 'Cannot calibration lamp state'

			IF self.Labels->Have('BIAS_STRIP_MEAN') THEN BEGIN
				self.Offset = self.Labels->Get('BIAS_STRIP_MEAN')
			ENDIF ELSE IF self.Labels->Have('BIAS') THEN BEGIN
				self.Offset = self.Labels->Get('BIAS')
			ENDIF ELSE IF self.Labels->Have('OFFSET') THEN BEGIN
				self.Offset = self.Labels->Get('OFFSET')
			ENDIF ELSE CISSCAL_Log, 'Cannot determine bias/offset'

			;	Until mid 2000, filters were named individually
			;	then latterly as a parenthesised pair of quoted strings
			IF self.Labels->Have('FILTER1_NAME') THEN BEGIN
				self.Filter1 = self.Labels->Get('FILTER1_NAME')
				self.Filter2 = self.Labels->Get('FILTER2_NAME')
			ENDIF ELSE IF self.Labels->Have('FILTER_NAME') THEN BEGIN
				FilterPair = self.Labels->Get('FILTER_NAME')
				; CISSCAL_Log, 'Filterpair is:', FilterPair
				; Parse on the basis that the token is like "('CL1','CB2')"
				Quote = ''''
				TextAt = STRPOS(FilterPair, Quote, 0) + 1
				TermQ = STRPOS(FilterPair, Quote, TextAt)
				self.Filter1 = STRMID(FilterPair,TextAt,TermQ-TextAt)
				TextAt = STRPOS(FilterPair, Quote, TermQ+1) + 1
				TermQ = STRPOS(FilterPair, Quote, TextAt)
				self.Filter2 = STRMID(FilterPair,TextAt,TermQ-TextAt)
			ENDIF ELSE CISSCAL_Log, 'Cannot determine filters from labels available'
		ENDIF

		IF NOT KEYWORD_SET(Quiet) THEN BEGIN
			CISSCAL_Log, 'Recsize and lblsize ' + STRING(self.RecSize) + ', ' + STRING(self.Labels->Get('LBLSIZE'))
			CISSCAL_Log, 'NL and NS: ' + STRING(self.NL) + ', ' + STRING(self.NS)
			CISSCAL_Log, 'NLB and NBB: ' + STRING(self.NLB) + ', ' + STRING(self.NBB)
			CISSCAL_Log, 'Filters are: ' + STRING(self.Filter1) +  ' and ' + STRING(self.Filter2)
		ENDIF
		
		Status = self.RecSize NE '' $
			 AND self.Labels->Get('LBLSIZE') NE '' $
			 AND self.NL NE '' $
			 AND self.NS NE ''
		; Previously also required these (but this breaks with slope FLOAT images)
		;	AND self.NLB NE '' AND self.NBB NE ''

	ENDELSE
	
	CLOSE,1
	ON_IOERROR, NULL
	RETURN, Status 

END


