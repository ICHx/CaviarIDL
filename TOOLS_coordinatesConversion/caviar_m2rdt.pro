PRO caviar_m2rdt, cmat, $					;input
				   ra, dec, twist			;output

	cspice_m2eul, cmat, 3, 1, 3, twist, dec, ra
	dec   = !dpi/2 - dec
	ra	  = ra - !dpi/2
	
	IF ra 	 LT 0.0d0 THEN ra	+=2*!dpi
	IF twist LT 0.0d0 THEN twist+=2*!dpi
	
	RETURN
END
