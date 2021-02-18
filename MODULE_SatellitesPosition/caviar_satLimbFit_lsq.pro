;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_SATLIMBFIT_LSQ_GETSATRADII
;
; PURPOSE: 
;	Retrieve tri-axial radii of a satellite given by scientifical papers and 
;	sigma values. These value are more precise than the one given by spice 
;	kernels
;
; INPUTS:
;	satID: Scalar value of type integer or long. NAIF ID of the satellite.
;
; OUTPUTS:
;	radii: 6-Elements array of doubles. Tri-axial radii of the satellite +
;		corresponding sigma values.
;
; NOTE:
;	Data from 501, 502, 503 come from:
;		P.C. Thomas et al., Shapes And Topography Of Galilean Satellites From 
;		Galileo Ssi Limb Coordinates
;	Data for 601-605, 607-618, 632-635 come from 
;		P.C. Thomas, Sizes, shapes, and derived properties of the saturnian 
;		satellites after the Cassini nominal mission, 2010
;
; MODIFICATIONS HISTORY:
;	2013 december, 18		R. Tajedine, L-E. MEUNIER			OBSPM>IMCCE
;		- Created with satellites: 601, 602, 603, 604, 605, 607, 608, 609
;-------------------------------------------------------------------------------
FUNCTION caviar_satLimbFit_lsq_getSatRadii, satID
	
	CASE satID OF
		501: RETURN, [1830.0D, 1819.5D, 1816.25D, 2.5D, 1.5D, 1.5D]
		502: RETURN, [1563.0D, 1561.0D, 1559.5D,  1.0D, 2.0D, 1.0D]
		503: RETURN, [2631.5D, 2631.5D, 2631.5D,  2.0D, 2.0D, 2.0D]
		
		601: RETURN, [207.8D, 196.7D, 190.6D, 0.5D, 0.5D, 0.3D]
		602: RETURN, [256.6D, 251.4D, 248.3D, 0.3D, 0.2D, 0.2D]
		603: RETURN, [538.4D, 528.3D, 526.3D, 0.3D, 1.1D, 0.6D]
		604: RETURN, [563.4D, 561.3D, 559.6D, 0.6D, 0.5D, 0.4D]
		605: RETURN, [765.0D, 763.1D, 762.4D, 0.7D, 0.6D, 0.6D]
		
		607: RETURN, [180.1D, 133.0D, 102.7D, 2.0D, 4.5D, 4.5D]
		608: RETURN, [746.1D, 745.3D, 712.1D, 2.9D, 2.9D, 1.6D]
		609: RETURN, [109.4D, 108.5D, 101.8D, 1.4D, 0.6D, 0.3D]
		610: RETURN, [101.5D,  92.5D,  76.3D, 1.9D, 1.2D, 1.2D]
		611: RETURN, [ 64.9D,  57.0D,  53.1D, 2.0D, 3.7D, 0.7D]
		612: RETURN, [ 21.7D,  19.1D,  13.0D, 0.5D, 0.3D, 0.3D]
		613: RETURN, [ 16.3D,  11.8D,  10.0D, 0.5D, 0.3D, 0.3D]
		614: RETURN, [ 15.1D,  11.5D,   7.0D, 0.3D, 2.2D, 0.6D]
		615: RETURN, [ 20.4D,  17.7D,   9.4D, 1.2D, 0.7D, 0.8D]
		616: RETURN, [ 67.8D,  39.7D,  29.7D, 3.1D, 3.1D, 1.9D]
		617: RETURN, [ 52.0D,  40.5D,  32.0D, 1.8D, 2.0D, 0.9D]
		618: RETURN, [ 17.2D,  15.7D,  10.4D, 1.9D, 1.3D, 0.84D]
		
		632: RETURN, [  1.6D,   1.6D,   1.6D, 0.6D, 0.6D, 0.6D]
		633: RETURN, [  2.9D,   2.8D,   2.0D, 0.6D, 0.8D, 0.4D]
		634: RETURN, [  1.5D,   1.2D,   1.0D, 0.6D, 0.4D, 0.2D]
		635: RETURN, [  4.3D,   4.1D,   3.2D, 0.7D, 0.9D, 0.8D]
		
		ELSE: MESSAGE, "Cannot find radii and sigma for satellite "+STRING(satID, '(I3)')
	ENDCASE
END	

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_SATLIMBFIT_LSQ_ELLFUNC
; PURPOSE: 
;	Compute the residual of an ellipse function given by its parameters (smajor,
;	sminor, xcenter, ycenter, phi) at point the (x,y). If this point is part of 
;	the ellipse, the residuals will equal zero, otherwise they will be non-zero.
; INPUTS:
;	x/y: Scalar value of type interger, float or double. X/Y-coordinates of the 
;		point at which to evaluate the ellipse residual.
;	xc/yc: Scalar value of type interger, float or double. X/Y-coordinates of
;		the center of the ellipse.
;	smajor/sminor: Scalar value of type interger, float or double. Semi-major
;		axis and semi-minor axis of the ellipse.
;	phi: Scalar value of type interger, float or double. Angle of the ellipse
;		in the plane.
; OUTPUTS:
;	res: Scalar value of type double. Residual of the ellipse function at point
;		(x,y).
;-------------------------------------------------------------------------------
FUNCTION caviar_satLimbFit_lsq_ellFunc, x, y, xc, yc, smajor, sminor, phi

	smajorsq = smajor*smajor
	sminorsq = sminor*sminor
	a = smajorsq * SIN(phi)^2 + sminorsq * COS(phi)^2
	b = smajorsq * COS(phi)^2 + sminorsq * SIN(phi)^2
	c = 2.D * (smajorsq - sminorsq) * cos(phi) * sin(phi)
	d = -smajorsq*sminorsq 
 
	RETURN, a*(x-xc)*(x-xc) + b*(y-yc)*(y-yc) + c*(x-xc)*(y-yc) + d
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_SATLIMBFIT_LSQ
; PURPOSE: 
;	Compute the fit of the detected limb with the ellipsoid shape model of the
;	satellite.
; INPUTS:
;       
; KEYWORDS:
;		
; OUTPUTS:
;	sigSc:
;	sigLc:
;	sigPhi:
; COMMON BLOCKS:
;       
; MODIFICATION HISTORY:
;	Created by R. Tajeddine							OBSPM>IMCCE
;	2013 december, 18		L-E. MEUNIER			OBSPM>IMCCE	
;		- Converted from fortran to IDL v8.2 
;-------------------------------------------------------------------------------
PRO caviar_satLimbFit_lsq, sat, satModel, satLimb, $	;input
							sigSc, sigLc, sigPhi	;output
	
	COMMON CAVIAR_DATA, image, stars, imgStars, planets
	
	; Get satellite state relative of the spacecraft:
	cspice_spkez, sat.ID, image.ET, 'J2000', 'CN+S', image.SPC.ID, state, ltime
	rho_J2000 = -state[0:2]
	
	; Compute distance represented by one pixel on the satellite in km:
	distance = SQRT(state ## TRANSPOSE(state))
	pixDist = distance * TAN(image.fovpix) ;in Km/pixel

	;***************************************************************************
	; Get ellipse parameters (center, smajor, sminor and their sigma) that 
	; intersect the satellite ellipsoid model with the plane orthogonal  
	; to spc->sat state vector:
	;***************************************************************************
	; Get satellite radii:
;	 cspice_bodvcd, sat.ID, 'RADII',3,radii
	radii = caviar_satLimbFit_lsq_getSatRadii(sat.ID)
	
	; Transform vector (rho) normal to the projection plane from inertial 
	; (x,y,z)J2000 to body-fixed (x,y,z)sat:
	cspice_tipbod, 'J2000', sat.ID, image.ET-ltime, tipm
	cspice_mxv, tipm, rho_J2000, rho_BODY
	
	; Get ellipsoid model parameters and their sigma:
	cspice_edlimb, radii[0], radii[1], radii[2], rho_BODY, spiceEllipse
	cspice_el2cgv, spiceEllipse, center, vSmajor, vSminor
	cspice_edlimb, radii[3], radii[4], radii[5], rho_BODY, spice_ellipse
	cspice_el2cgv, spice_ellipse, sigCenter, vSigSmajor, vSigSminor
	
	; Get semi-major, semi-minor axes & their sigma in pixel:
	smajor = SQRT(vSmajor ## TRANSPOSE(vSmajor)) / pixDist
	sminor = SQRT(vSminor ## TRANSPOSE(vSminor)) / pixDist
	sigSmajor = SQRT(vSigSmajor ## TRANSPOSE(vSigSmajor)) / pixDist
	sigSminor = SQRT(vSigSminor ## TRANSPOSE(vSigSminor)) / pixDist

	IF sminor GT smajor THEN BEGIN
		tmp = sminor 	& sigTmp = sigSminor
		sminor = smajor	& sigSminor = sigSmajor
		smajor = tmp	& sigSmajor = sigTmp
	ENDIF
	phi = 0.d0
	
	;***************************************************************************
	; Perform the limb fitting:
	;***************************************************************************
;	cnt = 0
	
	
	npts = N_ELEMENTS(satLimb[*,0])
	s = satLimb[*,0]
	l = satLimb[*,1]
	sc = (satModel.SLCENTER)[0]
	lc = (satModel.SLCENTER)[1]
	sc0 = sc
	lc0 = lc
	
	
	eps = 1.0D-4
	sigmapix = 0.5D
	
	ite = 0 & maxite = 50
	working = 1
	WHILE working DO BEGIN
		ite++
		
		Y = DBLARR(npts)
		drdsc = DBLARR(npts)
		drdlc = DBLARR(npts)
		drdphi = DBLARR(npts)
		sigma = DBLARR(npts)
		
		FOR i=0, npts-1 DO BEGIN
			; Compute dr/ds:
			Yp = caviar_satLimbFit_lsq_ellFunc(s[i]+eps, l[i], sc, lc, smajor, sminor, phi)
			Ym = caviar_satLimbFit_lsq_ellFunc(s[i]-eps, l[i], sc, lc, smajor, sminor, phi)
			drds = (Yp-Ym)/(2.d0*eps)
			
			; Compute dr/dl:
			Yp = caviar_satLimbFit_lsq_ellFunc(s[i], l[i]+eps, sc, lc, smajor, sminor, phi)
			Ym = caviar_satLimbFit_lsq_ellFunc(s[i], l[i]-eps, sc, lc, smajor, sminor, phi)
			drdl = (Yp-Ym)/(2.d0*eps)
			
			; Compute dr/da:
			Yp = caviar_satLimbFit_lsq_ellFunc(s[i], l[i], sc, lc, smajor+eps, sminor, phi)
			Ym = caviar_satLimbFit_lsq_ellFunc(s[i], l[i], sc, lc, smajor-eps, sminor, phi)
			drda = (Yp-Ym)/(2.d0*eps)

			; Compute dr/db:
			Yp = caviar_satLimbFit_lsq_ellFunc(s[i], l[i], sc, lc, smajor, sminor+eps, phi)
			Ym = caviar_satLimbFit_lsq_ellFunc(s[i], l[i], sc, lc, smajor, sminor-eps, phi)
			drdb = (Yp-Ym)/(2.d0*eps)
			
			; Compute sigma 
			sigdrdssq = (drds*sigmapix)*(drds*sigmapix)
			sigdrdlsq = (drdl*sigmapix)*(drdl*sigmapix)
			sigdrdasq = (drda*sigSmajor)*(drda*sigSmajor)
			sigdrdbsq = (drdb*sigSminor)*(drdb*sigSminor)
			sigma[i] = SQRT(sigdrdssq + sigdrdlsq + sigdrdasq + sigdrdbsq)
			
			; Compute Y:
			Y[i] = caviar_satLimbFit_lsq_ellFunc(s[i], l[i], sc, lc, smajor, sminor, phi)
			Y[i] = -Y[i]/sigma[i]			
;njc in Radwan's code there is no sigma in divisor:
;                        Y[i] = -Y[i]
						
			; Compute dr/dxc:
			Yp = caviar_satLimbFit_lsq_ellFunc(s[i], l[i], sc+eps, lc, smajor, sminor, phi)
			Ym = caviar_satLimbFit_lsq_ellFunc(s[i], l[i], sc-eps, lc, smajor, sminor, phi)
			drdsc[i] = (Yp-Ym)/(2.d0*eps*sigma[i])
;njc in Radwan's code there is no sigma in divisor:
;                        drdsc[i] = (Yp-Ym)/(2.d0*eps)

			; Compute dr/dyc:
			Yp = caviar_satLimbFit_lsq_ellFunc(s[i], l[i], sc, lc+eps, smajor, sminor, phi)
			Ym = caviar_satLimbFit_lsq_ellFunc(s[i], l[i], sc, lc-eps, smajor, sminor, phi)
			drdlc[i] = (Yp-Ym)/(2.d0*eps*sigma[i])
;njc in Radwan's code there is no sigma in divisor:
;                        drdlc[i] = (Yp-Ym)/(2.d0*eps)
		
			; Compute dr/dphi:
			Yp = caviar_satLimbFit_lsq_ellFunc(s[i], l[i], sc, lc, smajor, sminor, phi+eps)
			Ym = caviar_satLimbFit_lsq_ellFunc(s[i], l[i], sc, lc, smajor, sminor, phi-eps)
			drdphi[i] = (Yp-Ym)/(2.d0*eps*sigma[i])	
;njc in Radwan's code there is no sigma in divisor:
;                        drdphi[i] = (Yp-Ym)/(2.d0*eps)
		ENDFOR

		tM = [[drdsc],[drdlc],[drdphi]]
		
		;***********************************************************************
		; Matrix inversion:
		;***********************************************************************
		tMM = tM ## TRANSPOSE(tM)
		tMM_1 = LA_INVERT(tMM, /DOUBLE)
		corr = tMM_1 ## (tM ## Y)		
			
		;***********************************************************************
		; Update parameters with the correction and get the new sigma:
		;***********************************************************************	
		sc  += corr[0]
		lc  += corr[1]
;njc uncomment:
		phi += corr[2]

		sigSc  = SQRT(ABS(tMM_1[0,0]))
		sigLc  = SQRT(ABS(tMM_1[1,1]))
		sigPhi = SQRT(ABS(tMM_1[2,2]))
		
		; Compute new Y:
;njc uncomment:
;		FOR i=0, npts-1 DO BEGIN
;			Y[i] = caviar_satLimbFit_lsq_ellFunc(s[i], l[i], sc, lc, smajor, sminor, phi)
;		ENDFOR
		
		PRINT, FORMAT='(A,I-3,X,3(F10.6,X))', "Iteration ", ite, TRANSPOSE(corr)
		
		
		
		IF TOTAL(corr LT [1D-3,1D-3,1D-4]) EQ N_ELEMENTS(corr) || ite GE maxite THEN BEGIN
			PRINT, FORMAT='(A,/,A,2(G0,X),/,A,G0," (",G0,")",/,A,2(G0,X)," (",2(G0,X),")",/,A,2(G0,X),/)', $
				"Ellipse fitted parameters:", $
				"  Semi-major/Semi-minor axis [pix]: ", smajor, sminor, $
				"  Phi    (sigma) [rad]: ", phi, sigPhi, $
				"  Center (sigma) [pix]: ", sc, lc, sigSc, sigLc, $
				"Offset [pix]: ", sc-sc0, lc-lc0
			BREAK	;Quit the while-loop
;			cnt++
;			IF cnt EQ 20 THEN BREAK
;			
			; Exclude points with Yres greater than 3-sigma:

;njc uncomment:
;			index = WHERE(sigma LT 100)
;			IF index[0] NE -1 THEN Ygood = Y[index]
;			ngood = N_ELEMENTS(Ygood)
;njc uncomment:			
;			Yres = Ygood - REPLICATE(MEAN(Ygood), ngood)
;			sigmaY = SQRT( TOTAL(Yres*Yres)/ngood )
;njc uncomment:			
;			index = WHERE( ABS(Yres) GE 3*sigmaY )
;			IF index[0] NE -1 THEN sigma[index] = 100D		
		ENDIF

	ENDWHILE
	
	;***************************************************************************
	; Update satModel points position with correction parameters:
	;***************************************************************************
	sloffset = [sc-sc0, lc-lc0]
	rmat1 = [[COS(phi), -SIN(phi)], [SIN(phi),  COS(phi)]]
	rslCenter = ([[1,0],[0,1]] - rmat1) ## satModel.SLCENTER
	IF haveTag(satModel, 'SLCENTER') THEN satModel.slCenter += slOffset
	IF haveTag(satModel, 'SLLIMB')   THEN BEGIN
		nlimb = N_ELEMENTS(satModel.SLLIMB[*,0])
		satModel.SLLIMB += slOffset ## MAKE_ARRAY(nlimb, VALUE=1.0D)
		satModel.SLLIMB = rmat1 ## satModel.SLLIMB + rslCenter ## MAKE_ARRAY(nlimb, VALUE=1.0D)
	ENDIF
	IF haveTag(satModel, 'SLTERM')   THEN BEGIN
		nterm = N_ELEMENTS(satModel.SLTERM[*,0])
		satModel.SLTERM += slOffset ## MAKE_ARRAY(nterm, VALUE=1.0D)
		satModel.SLTERM = rmat1 ## satModel.SLTERM + rslCenter ## MAKE_ARRAY(nterm, VALUE=1.0D)
	ENDIF
	IF haveTag(satModel, 'SLEQUA')   THEN BEGIN
		nequa = N_ELEMENTS(satModel.SLEQUA[*,0])
		satModel.SLEQUA += slOffset ## MAKE_ARRAY(nequa, VALUE=1.0D)
		satModel.SLEQUA = rmat1 ## satModel.SLEQUA + rslCenter ## MAKE_ARRAY(nequa, VALUE=1.0D)
	ENDIF
	
	RETURN
END
