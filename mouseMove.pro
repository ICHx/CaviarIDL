;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: MOUSEMOVE_CALLBACK
; PURPOSE: Call a procedure define by updt_callback and pass arguments contained in p_updt_cb_data with
;	the keyword 'DATA'. This procedure could be used to re-display the image after changing colors
;-------------------------------------------------------------------------------
PRO mouseMove_callBack, procName, TRANSLATE=shift, ROTATE=angle
	COMPILE_OPT HIDDEN

	errState = 0
    CATCH, errState
    IF (errState NE 0) THEN BEGIN
        CATCH, /CANCEL
        v = DIALOG_MESSAGE(['% MOUSEMOVE: Unexpected error!', !ERROR_STATE.msg], /ERROR)
       RETURN
    ENDIF
    
     IF STRLEN(procName) GT 0 THEN BEGIN
        IF KEYWORD_SET(shift) THEN CALL_PROCEDURE, procName, TRANSLATE=shift
        IF KEYWORD_SET(angle) THEN CALL_PROCEDURE, procName, ROTATE=angle
    ENDIF
 
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: MOUSEMOVE
; PURPOSE: Allow user to translate or rotate graphical objects in a window by 
;		moving the mouse in it. This routine call an external routine with a  
;		translation vector or a rotation angle asparameter allowing the user to 
;		manage object(s) motion.
;
;		Translation vector: 2-Elements array of double value for x and y translation.
;		Rotation angle: Scalar double representing an angle in radian.
;
; KNOWN ISSUES:
;		To use this routine on Mac OSX, the X11 preferences have to be modified.  
;		In the "windows" tab, check all the 3 options (don't know witch 
;		particular option cause the problem !).
; MODIFICATIONS:
;	2012 February, 14		MEUNIER L-E				OBSPM>IMCCE
;		- Add header and comments
;		- Cosmetics, variables name
;	2012 June				MEUNIER L-E				OBSPM>IMCCE
;		- Move computation block to external routine to be used by an other user  
;		  interactive procedure.
;		- Change algorithme to get better user experience
;	2012 October, 9			MEUNIER L-E				OBSPM>IMCCE
;		- Make this routine independant from final purpose. Now, just compute 
;		  shift and angle values, and give it to the procedure caller.
;		- Add 'RCENTER' to set his own center.
;-------------------------------------------------------------------------------	
PRO mouseMove, winID, cbProcName, TFACTOR=tFactor, RFACTOR=rFactor, RCENTER=centerVec
    
    ; Save current window and set new:
	old_win = !D.WINDOW
	WSET, winID
    
    ; Print user informations:
    PRINT, "Press a button and move the mouse over the window:"            	
	IF KEYWORD_SET(tFactor) THEN PRINT, "    Left button   => translate
	IF KEYWORD_SET(rFactor) THEN PRINT, "    Middle button => rotate 
	PRINT, "    Right button  => quit"
		
	
	IF NOT KEYWORD_SET(centerVec) THEN centerVec=[!D.X_SIZE/2.0D, !D.Y_SIZE/2.0D]
	CURSOR, xmA, ymA, /DEVICE
	ptA = [xmA, ymA]
	
	working=1
	WHILE working EQ 1 DO BEGIN
		
		CURSOR, xmB, ymB, /NOWAIT, /DEVICE
		ptB = [xmB, ymB]
		
		; Left button -> compute translation vector:
		IF !mouse.button EQ	1 && ~ARRAY_EQUAL(ptA,ptB) && KEYWORD_SET(tFactor) $
		THEN mouseMove_callBack, cbProcName, TRANSLATE=tFactor*[ptB[0]-ptA[0], ptB[1]-ptA[1]]
		
		; Middle button -> compute rotation angle
		IF !mouse.button EQ	2 && ~ARRAY_EQUAL(ptA,ptB) && KEYWORD_SET(rFactor) THEN BEGIN
			ptA_c = ptA - centerVec
			ptB_c = ptB - centerVec
			
			angle = atan(ptB_c[1]/ptB_c[0])-atan(ptA_c[1]/ptA_c[0])
			IF angle LT -!DPI/2 THEN angle += !DPI
			IF angle GT  !DPI/2 THEN angle -= !DPI
			
			mouseMove_callBack, cbProcName, ROTATE=rFactor*angle			
		ENDIF
		
		; Right button -> quit the loop and stop the procedure
		IF !mouse.button EQ 4 THEN working=0	
		
		; Mouse position B become mouse position A
		ptA = ptB
	ENDWHILE
	
	; Set old window:
	WSET, old_win
	
	RETURN
END
