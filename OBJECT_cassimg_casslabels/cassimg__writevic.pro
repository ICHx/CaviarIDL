;;	cassimg__writevic.pro
;;	Procedure to write out this CassImg object as a VICAR file
;;	Kevin	10/9/98
;;	Version Tag: $Id: cassimg__writevic.pro,v 1.10 2001/11/19 16:36:19 kevin Exp $

PRO CassImg::WriteVic, FileName

;	N.B. Watch out for mismatches between the contents of the
;	associative array in our Labels subobject and the instance
;	variables we have for convenience!

;	Set a default file name
  IF N_PARAMS() LT 1 THEN FileName = 'outputtest'

;	Ensure LUN is free, then open for writing and set errorhandling
  CLOSE,1
  OPENW,1,FileName
  CISSCAL_Log, 'Output VICAR image file opened: ', FileName
  ON_IOERROR, writefailed

;	Modify or add to labels
  Junk = self.Labels->Set('CISSCAL_WTIM', SYSTIME(0), 1)

;	Currently we will always write the image as REAL
  WriteBuff= FLOAT(*self.ImageP)
  self.VicFormat = 'REAL'
  Junk = self.Labels->Set('FORMAT', 'REAL', 1)

;	Determine our REAL format
  CASE STRUPCASE(!version.arch) OF
    'VAX':	BEGIN
		ThisHost = 'VAX_VMS'
		self.VicRealFmt = 'VAX'
	END
    'ALPHA': BEGIN
		ThisHost = 'DECSTATN'
		self.VicRealFmt = 'RIEEE'
	END
    'X86': BEGIN
		ThisHost = 'PC_X86'
		self.VicRealFmt = 'RIEEE'
	END
    'SPARC': BEGIN
		ThisHost = 'SUN-4'
		self.VicRealFmt = 'IEEE'
	END
    'MIPSEB': BEGIN
		ThisHost = 'SGI'
		self.VicRealFmt = 'IEEE'
	END
    'IBMR2': BEGIN
		ThisHost = 'IBMR2'
		self.VicRealFmt = 'IEEE'
	END
    ELSE: BEGIN
      CISSCAL_Log, "Can't write float file: can't recognise host architecture"
      RETURN
      END
  ENDCASE
  ThisIntFmt = LocalIntFmt()

;	Update host/format labels appropriately
  Junk = self.Labels->Set('REALFMT', self.VicRealFmt, 1)
  Junk = self.Labels->Set('BREALFMT', self.VicRealFmt, 1)
  Junk = self.Labels->Set('HOST', ThisHost, 1)
  Junk = self.Labels->Set('BHOST', ThisHost, 1)
  Junk = self.Labels->Set('INTFMT', ThisIntFmt, 1)
  Junk = self.Labels->Set('BINTFMT', ThisIntFmt, 1)

;	We will just write all the labels followed by image:
;	 no binary prefix, no binary header, no trailing (EOL) labels
;	 therefore ensure that these labels and RECSIZE are set appropriately
  self.NBB = 0
  self.NLB = 0
  self.RecSize = self.NS * 4	; Each sample has a 4-byte REAL value
  Junk = self.Labels->Set('NBB', self.NBB, 0)
  Junk = self.Labels->Set('NLB', self.NLB, 0)
  Junk = self.Labels->Set('RECSIZE', self.RecSize, 0)
  Junk = self.Labels->Set('EOL', 0, 0)

;	LBLSIZE must be a multiple of RECSIZE
;	Now done automatically inside self.Labels->toString()
;	Get our label object to serialise itself, then write it and
;		pack to the announced size
  LabelText = self.Labels->toString()
  LabelSize = self.Labels->Get('LBLSIZE')
  WRITEU,1, LabelText
  WRITEU,1, BYTARR(LabelSize - STRLEN(LabelText))

;	Write image in real format
  CISSCAL_Log, 'About to write as real format'
  WRITEU,1,WriteBuff
  CLOSE,1
  CISSCAL_Log, 'output VICAR image file closed: ', FileName

RETURN

;	Handle I/O errors here ourselves
writefailed:
  CISSCAL_Log, 'Write failed: ' + !ERR_STRING
  CISSCAL_Log, 'fstat(1) is: ', fstat(1)

close,lun
free_lun,lun
END
