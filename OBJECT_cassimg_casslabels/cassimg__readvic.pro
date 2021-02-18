;;	cassimg__readvic.pro
;;	Method to read a Cassini VICAR image file into a CassImg object
;;	(and various support routines)
;;	Kevin Beurle	September 1998 and January 1999
;;	Version tag: $Id: cassimg__readvic.pro,v 1.1 2004/02/16 16:18:44 mwe Exp $

PRO LogFilePos, Lun
  Temp=FSTAT(Lun)
  CISSCAL_Log, 'Position in file on LUN', Lun, ':'
  CISSCAL_Log, ' byte', Temp.Cur_Ptr, ' out of', Temp.Size
END

FUNCTION LocalIntFmt
;	Find out whether the system we're running on is big or little endian
;	and return a string as per the VICAR "Int format" field.
;	For SPARC, etc., return 'HIGH' (aka Network byteorder, BigEndian)
;	For Intel, etc., return 'LOW'  (LittleEndian: LSByte is at lower addr.)
;	The /HTONS keyword to BYTEORDER makes it do 'Host to Network'
;	conversion for short integers. For SPARC this leaves it unchanged,
;	whereas for Intel it scrambles the value.
  A = 1234	; Any value that's not a multiple of 257!
  B = A
  BYTEORDER, B, /HTONS
  IF ( A EQ B ) THEN IntFmt = 'HIGH' ELSE IntFmt = 'LOW'
  RETURN, IntFmt
END


PRO CassImg::ReadVic, fname, Quiet=Quiet, NotCass=NotCass
	; The NotCass keyword means we should assume this is just a generic VICAR file
	;  and not actually a Cassini image. Therefore don't try to use any but the
	;  most basic labels.

	IF KEYWORD_SET(Quiet) THEN CISSCAL_Log, 'Quiet option selected'
	IF ( self->ReadLabels(fname, Quiet=Quiet, NotCass=NotCass) ) THEN BEGIN
	  ImBuff = INTARR(self.NS, self.NL)

	  CLOSE,1
	  OPENR,1,fname
	  POINT_LUN, 1, self.LeadIn	; Skip the labels
	  ON_IOERROR, readfailed

	  CISSCAL_Log, 'VICAR image file opened: ', fname

	; Free any old image/other buffers first...
	  IF (PTR_VALID(self.BinaryHdrP)) THEN PTR_FREE,self.BinaryHdrP
	  IF (PTR_VALID(self.ImageP)) THEN PTR_FREE,self.ImageP
	  IF (PTR_VALID(self.OtherP)) THEN PTR_FREE,self.OtherP
	  IF (PTR_VALID(self.OverClkP)) THEN PTR_FREE,self.OverClkP
	  IF (PTR_VALID(self.SaturatedP)) THEN PTR_FREE,self.SaturatedP
	  IF (PTR_VALID(self.MissingP)) THEN PTR_FREE,self.MissingP
	  HaveOtherBuff = 0
	  HaveOverclocks = 0
	  HaveSatdMissg = 0

	;	Read Binary Header if one is present in input file
	  IF ( self.NLB GT 0 ) THEN BEGIN
		BinaryHdr=BYTARR(self.RecSize, self.NLB)
		READU,1,BinaryHdr
		CISSCAL_Log, 'Binary header read'
		self.BinaryHdrP = PTR_NEW(BinaryHdr)
	  ENDIF ELSE BEGIN
		CISSCAL_Log, 'No Binary header to be read'
		self.BinaryHdrP = PTR_NEW()	; Null pointer
	  ENDELSE

	;	Read BYTE image...
	  IF ( self.VicFormat EQ 'BYTE' ) THEN BEGIN
		CISSCAL_Log, 'About to read image as byte format'
		bytbuff=BYTARR(self.NS+self.NBB,self.NL)
		READU,1,bytbuff
		ImBuff=FIX(bytbuff(self.NBB:*,*))	; Bytes are unsigned 0-255 in IDL
		bytecount = self.Labels->Get('LBLSIZE') $		; Labels
				+ self.RecSize*self.NLB $	;  binary header
				+ LONG( self.NS+self.NBB ) * self.NL	;  image
	;	Grab the entire "binary prefix" (if any)
	;	 then also explicitly grab overclocked pixels
		IF self.NBB GT 0 THEN BEGIN
		  OtherBuff = bytbuff(0:self.NBB-1,*)
		  HaveOtherBuff = 1
		  IF self.NBB EQ 24 THEN BEGIN
		    OverClkBuff = bytbuff(23,*)
		    HaveOverclocks = 1
		  ENDIF
		ENDIF
	;	Lets make the image floating point now to save trouble later
		FltImBuff = FLTARR(self.NS, self.NL)
		FltImBuff = FLOAT(ImBuff)
		CLOSE,1
	;	Now record where pixels are missing or saturated
		SaturatedPixels = WHERE( ImBuff EQ 255 )
		MissingPixels = WHERE( ImBuff EQ 0 )
		HaveSatdMissg = 1

	;	Read 2-byte Integer image...
	  ENDIF ELSE IF ( self.VicFormat EQ 'HALF' ) THEN BEGIN
		CISSCAL_Log, 'About to read image as word format'
	;	Each image record contains NBB bytes followed by NS integer pixel values
	;	 pretty way would define an anon structure, then an array of these as the buffer
	;	Instead just suck it all as two-byte integers for now...
	;    VicRec = { nbb:BYTARR(self.NBB), img:INTARR(self.NS) }
	;    bigbuff = REPLICATE(VicRec,self.NL)
	;    ...
	;    ImBuff=bigbuff.img
	;    IF self.NBB GT 0 THEN OtherBuff = bigbuff.nbb

		bigbuff=INTARR(self.NS+self.NBB/2,self.NL)
		READU,1,bigbuff
		IF ( self.VicIntFmt NE LocalIntFmt() ) THEN BEGIN
		CISSCAL_Log, 'Swapping byte order'
		BYTEORDER, bigbuff	; swap byte order 1234->2143
		ENDIF
		ImBuff=bigbuff(self.NBB/2:*,*)
		bytecount = self.Labels->Get('LBLSIZE') $		; Labels
				+ self.RecSize*self.NLB $	;  binary header
				+ LONG( 2*self.NS+self.NBB ) * self.NL	;  image

	;	Grab the entire "binary prefix" (if any)
	;	 then also explicitly grab overclocked pixels
		IF self.NBB GT 0 THEN BEGIN
		  OtherBuff = bigbuff(0:self.NBB/2-1,*)
		  HaveOtherBuff = 1
		  IF self.NBB EQ 24 THEN BEGIN
		    OverClkBuff = bigbuff(11,*)
		    HaveOverclocks = 1
		  ENDIF
		ENDIF
	; Not including end of file test due to possibility of End of File label
		CLOSE,1
	;	Lets make the image floating point now to save trouble later
		FltImBuff = FLTARR(self.NS, self.NL)
		FltImBuff = FLOAT(ImBuff)
	;	Now record where pixels are missing or saturated
		SaturatedPixels = WHERE( ImBuff EQ 4095 )
		MissingPixels = WHERE( ImBuff EQ 0 )
		HaveSatdMissg = 1

	;	Read 4-byte REAL image...
	  ENDIF ELSE IF ( self.VicFormat EQ 'REAL' ) THEN BEGIN
		CISSCAL_Log, 'About to read image as real format'
		IF ( self.VicRealFmt NE 'VAX' ) THEN BEGIN
	; Assume native float is IEEE (LocalIntFmt() is HIGH) or RIEEE (is LOW)
		  CISSCAL_Log, 'The real data are in ' + self.VicRealFmt + ' format'
		  bigbuff=FLTARR(self.NS+self.NBB/4,self.NL)
		  READU,1,bigbuff
		  FltImBuff=bigbuff(self.NBB/4:*,*)
		  IF ( self.VicIntFmt NE LocalIntFmt() ) THEN BEGIN
		      CISSCAL_Log, 'Swapping byte order'
		      BYTEORDER, FltImBuff, /LSWAP ; swap longword byte order 1234->4321
		  ENDIF
		  bytecount = self.Labels->Get('LBLSIZE') $           ; Labels
		                    + self.RecSize*self.NLB $       ;  binary header
		                    + LONG( 4*self.NS+self.NBB ) * self.NL      ;  image
		  IF self.NBB GT 0 THEN OtherBuff = bigbuff(0:self.NBB/4-1,*)
	; Not including end of file test due to possibility of End of File label
		  CLOSE,1
		ENDIF ELSE BEGIN
	;	First gobble it as longwords (integer*4)
		  bigbuff=LONARR(self.NS,self.NL)
		  READU,1,bigbuff
		  ImBuff=bigbuff
		  IF ( self.VicIntFmt NE LocalIntFmt() ) THEN BEGIN
		  CISSCAL_Log, 'Swapping byte order'
	  	  BYTEORDER, ImBuff, /LSWAP	; swap longword byte order 1234->4321
		  ENDIF
		  bytecount = self.Labels->Get('LBLSIZE') $			; Labels
				+ LONG( 4*self.NS ) * self.NL	;  image
	; Not including end of file test due to possibility of End of File label
		  CLOSE,1
	;	Now need to convert from VAX floating point to native
	;      print,FORMAT='("Imbuff[0] is ", Z)',ImBuff[0]
		  BYTEORDER,ImBuff
	;      print,FORMAT='("Imbuff[0] is ", Z)',ImBuff[0]
		  BYTEORDER,ImBuff,/LSWAP
	;      print,FORMAT='("Imbuff[0] is ", Z)',ImBuff[0]

	;	Extract sign bit, fraction and exponent fields
		  SignFld = ImBuff AND '80000000'XL
		  FracFld = ImBuff AND '007fffff'XL
		  ExpFld =  ImBuff AND '7f800000'XL

	;	Map sign to +/- 1
		  Minus1 = -1
		  Sign = (SignFld + 1 ) > Minus1

	;	Rightshift exponent field and strip excess
		  Exponent = ISHFT(ExpFld, -23) - 129

	;	Mantissa: Insert suppressed MS bit and righshift
		  Fraction = FLOAT(FracFld OR '00800000'XL)/FLOAT('00800000'XL)
	;	Revert to 0.0 wherever initial longword value was 0
		  ZeroPos = WHERE(ImBuff EQ 0)
		  SizeZeroPos = SIZE(ZeroPos)
		  IF SizeZeroPos[0] NE 0 THEN BEGIN
	;	There are some zero values, as indexed by ZeroPos
		    Fraction[ ZeroPos ] = 0.0
		  ENDIF

	;	*** Not yet certain this is quite right... ***

		  FltImBuff = FLTARR(self.NS, self.NL)
		  FltImBuff = Fraction * FLOAT(2)^Exponent * Sign
		ENDELSE
	  ENDIF ELSE BEGIN
		CISSCAL_Log, 'Unrecognised VICAR format'
	  ENDELSE

	  self.ImageP = PTR_NEW(FltImBuff)

	  IF HaveOtherBuff THEN BEGIN
		self.OtherP = PTR_NEW(OtherBuff)
	  ENDIF ELSE BEGIN
		self.OtherP = PTR_NEW()	; Null pointer
	  ENDELSE

	  IF HaveOverclocks THEN BEGIN
		self.OverClkP = PTR_NEW(OverClkBuff)
	  ENDIF ELSE BEGIN
		self.OverClkP = PTR_NEW()	; Null pointer
	  ENDELSE

	  IF HaveSatdMissg THEN BEGIN
		self.SaturatedP = PTR_NEW(SaturatedPixels)
		self.MissingP = PTR_NEW(MissingPixels)
	  ENDIF ELSE BEGIN
		self.SaturatedP = PTR_NEW()	; Null pointers
		self.MissingP = PTR_NEW()
	  ENDELSE

	  self.ImgBytes = bytecount

	ENDIF ELSE BEGIN	; If label reading failed...
	  CISSCAL_Log, 'Unable to read input file ' + fname
	  self.ImgBytes = 0

	ENDELSE
	RETURN

	readfailed:
	  CISSCAL_Log, 'Read failed: ' + !ERR_STRING
	  CISSCAL_Log, 'fstat(1) is: ', fstat(1)
	  self.ImgBytes = 0

END


