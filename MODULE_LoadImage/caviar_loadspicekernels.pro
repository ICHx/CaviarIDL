;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: caviar_loadspicekernels
; PURPOSE: Load SPICE kernels specified in the 'METAKERNEL' file. 
;-------------------------------------------------------------------------------
PRO caviar_loadspicekernels, metaKernel, loaded, TITLE=title

	loaded = 0
	
	WHILE ~ISA(metaKernel, 'STRING') || ~FILE_TEST(metaKernel, /REGULAR) DO BEGIN
		MESSAGE, "The 'METAKERNEL' variable is not valid ('"+STRING(metaKernel)+"')", /CONTINUE
		path = GETENV("CAVIAR_COMMON_PATH")
		IF NOT KEYWORD_SET(title) THEN title = "Select a meta-kernel file:"
		metaKernel = DIALOG_PICKFILE(PATH=path, TITLE=title, /MUST_EXIST)
		IF metaKernel EQ '' THEN RETURN
	ENDWHILE
		
	PRINT, "Metakernel: ", metakernel
	
	CATCH, error
	IF error THEN BEGIN
		CATCH, /CANCEL
		MESSAGE, "Cannot load SPICE kernels. "+!ERROR_STATE.MSG
	ENDIF

	cspice_furnsh, metaKernel

	loaded = 1

	RETURN
END
