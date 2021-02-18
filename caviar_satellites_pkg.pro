;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;	Procedures/functions :
;		- PRO				caviar_satellites_getvisible
;		- PRO				caviar_satellites_setvisible
;		- PRO				caviar_satellites_display
;		- FUNCTION 			caviar_satellites_getHash
;		- FUNCTION 			caviar_satellites_getstruct
;		- FUNCTION	main	caviar_satellites_load
;		- PRO				caviar_satellites_gui_event
;		- PRO				caviar_satellites_gui
;-------------------------------------------------------------------------------

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_SATELLITES_GETVISIBLE
; PURPOSE: Get index of 'planets' elements with 'VISIBLE' tag set to 1 
; INPUTS:
;	None.
; OUTPUTS:
;   INDICES: 1D array of integer. Zero-based index of the planets set as 
;		"visible" in the image.
; OPTIONNAL KEYWORDS:
;	UPDATE: Set that keyword to update "VISIBLE" state tag of the planets before
;		getting indices.
;-------------------------------------------------------------------------------
FUNCTION caviar_satellites_getvisible, UPDATE=UPDATE
	
	COMMON CAVIAR_DATA, image, catstars, imgStars, planets
	
	IF KEYWORD_SET(UPDATE) THEN caviar_satellites_setvisible
	
	indices = LIST()
	FOR i=0, N_ELEMENTS(planets)-1 DO IF planets[i].visible EQ 1 THEN indices.ADD, i
	
	RETURN, indices.toArray()
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_SATELLITES_SETVISIBLE
; PURPOSE: Set 'planets' list 'VISIBLE' tag to 1 if 'XCOORD' & 'YCOORD' 
;	tag are inside the image and 0 otherwise.
; INPUTS:
;	None.
; OUTPUTS:
;   None.
;-------------------------------------------------------------------------------
PRO caviar_satellites_setvisible
	
	COMMON CAVIAR_DATA, image, catstars, imgStars, planets
	
	FOR i=0, N_ELEMENTS(planets)-1 DO BEGIN
		p = planets[i]
		IF p.XCOORD GT 0 && p.XCOORD LT image.NS $
		&& p.YCOORD GT 0 && p.YCOORD LT image.NL $
		THEN p.visible = 1 $
		ELSE p.visible = 0
		planets[i] = p
	ENDFOR
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_SATELLITES_DISPLAY
; PURPOSE: 
;-------------------------------------------------------------------------------
PRO caviar_satellites_display, satellites, dispParams, nl, offset, ZOOMFACTOR=zf

	IF dispParams.state NE 1 OR (n = N_ELEMENTS(satellites)) EQ 0 THEN RETURN
		
	IF ~KEYWORD_SET(zf) THEN zf=1

	; Get display parameters
	p_color = color(dispParams.color, /SILENT)	;Be careful, color function changes the color table!
	p_psym    = dispParams.psym
	p_symsize = dispParams.symsize
		
	; Display planets
	xp = DBLARR(n)
	yp = DBLARR(n)
	names = STRARR(n)
	FOR i=0, n-1 DO BEGIN
		sati = satellites[i]
		xp[i] = zf * ( sati.xcoord ) - offset[0]
		yp[i] = zf * ( (nl-1) - sati.ycoord ) - offset[1]
		names[i]=sati.name
	ENDFOR
	
	PLOTS, xp, yp, /DEVICE, COLOR=p_color, PSYM=p_psym, SYMSIZE=p_symsize
	XYOUTS, xp+10, yp-3, names, /DEVICE, COLOR=p_color
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_SATELLITES_GETHASH
; PURPOSE: 
;-------------------------------------------------------------------------------
FUNCTION caviar_satellites_getHash, planet_name
	
	CASE STRUPCASE(planet_name) OF
		"MERCURY": 	RETURN, ORDEREDHASH()
		"VENUS":	RETURN, ORDEREDHASH()
		"EARTH":	RETURN, ORDEREDHASH('MOON', 301)
		"MARS":		RETURN, ORDEREDHASH('PHOBOS', 401, 'DEIMOS', 402)
		"JUPITER":	RETURN, ORDEREDHASH( $
'IO',			501, 'EUROPA',		502, 'GANYMEDE',	503, 'CALLISTO',	504, $
'AMALTHEA',		505, 'HIMALIA',		506, 'ELARA'   ,	507, 'PASIPHAE',	508, $
'SINOPE',		509, 'LYSITHEA',	510, 'CARME',		511, 'ANANKE',		512, $
'LEDA',			513, 'THEBE',		514, 'ADRASTEA',	515, 'METIS',		516, $
'CALLIRRHOE',	517, 'THEMISTO',	518, 'MAGACLITE',	519, 'TAYGETE',		520, $
'CHALDENE',		521, 'HARPALYKE',	522, 'KALYKE',		523, 'IOCASTE',		524, $
'ERINOME',		525, 'ISONOE',		526, 'PRAXIDIKE',	527, 'AUTONOE',		528, $
'THYONE',		529, 'HERMIPPE',	530, 'AITNE',		531, 'EURYDOME',	532, $
'EUANTHE',		533, 'EUPORIE',		534, 'ORTHOSIE',	535, 'SPONDE',		536, $
'KALE',			537, 'PASITHEE',	538, 'HEGEMONE',	539, 'MNEME',		540, $
'AOEDE',		541, 'THELXINOE',	542, 'ARCHE',		543, 'KALLICHORE',	544, $
'HELIKE',		545, 'CARPO',		546, 'EUKELADE',	547, 'CYLLENE',		548, $
'KORE',			549, 'HERSE',		550 $
				 )
		"SATURN": RETURN, ORDEREDHASH( $
'MIMAS',		601, 'ENCELADUS',	602, 'TETHYS',		603, 'DIONE',		604, $
'RHEA',			605, 'TITAN',		606, 'HYPERION',	607, 'IAPETUS',		608, $
'PHOEBE',		609, 'JANUS',		610, 'EPIMETHEUS',	611, 'HELENE',		612, $
'TELESTO',		613, 'CALYPSO',		614, 'ATLAS',		615, 'PROMETHEUS',	616, $
'PANDORA',		617, 'PAN',			618, 'YMIR',		619, 'PAALIAQ',		620, $
'TARVOS',		621, 'IJIRAQ',		622, 'SUTTUNGR',	623, 'KIVIUQ',		624, $
'MUNDILFARI',	625, 'ALBIORIX',	626, 'SKATHI',		627, 'ERRIAPUS',	628, $
'SIARNAQ',		629, 'THRYMR',		630, 'NARVI',		631, 'METHONE',		632, $
'PALLENE',		633, 'POLYDEUCES',	634, 'DAPHNIS',		635, 'AEGIR',		636, $
'BEBHIONN',		637, 'BERGELMIR',	638, 'BESTLA',		639, 'FARBAUTI',	640, $
'FENRIR',		641, 'FORNJOT',		642, 'HATI',		643, 'HYROKKIN',	644, $
'KARI',			645, 'LOGE',		646, 'SKOLL',		647, 'SURTUR',		648, $
'ANTHE',		649, 'JARNSAXA',	650, 'GREIP',		651, 'TARQEQ',		652, $
'AEGAEON',		653 $
				 )			 
		"URANUS": RETURN, ORDEREDHASH( $
'ARIEL',		701, 'UMBRIEL',		702, 'TITANIA',		703, 'OBERON',		704, $
'MIRANDA',		705, 'CORDELIA',	706, 'OPHELIA',		707, 'BIANCA',		708, $
'CRESSIDA',		709, 'DESDEMONA',	710, 'JULIET',		711, 'PORTIA',		712, $
'ROSALIND',		713, 'BELINDA',		714, 'PUCK',		715, 'CALIBAN',		716, $
'SYCORAX',		717, 'PROSPERO',	718, 'SETEBOS',		719, 'STEPHANO',	720, $
'TRINCULO',		721, 'FRANCISCO',	722, 'MARGARET',	723, 'FERDINAND',	724, $
'PERDITA',		725, 'MAB',			726, 'CUPID',		727 $
				 )
		"NEPTUNE": RETURN, ORDEREDHASH( $
'TRITON',		801, 'NEREID',		802, 'NAIAD',		803, 'THALASSA',	804, $
'DESPINA',		805, 'GALATEA',		806, 'LARISSA',		807, 'PROTEUS',		808, $
'HALIMEDE',		809, 'PSAMATHE',	810, 'SAO',			811, 'LAOMEDEIA',	812, $
'NESO',			813 $
				 )
		"PLUTO": RETURN, HASH('CHARON', 901, 'NIX', 902, 'HYDRA', 903)				 

		ELSE: RETURN, ORDEREDHASH()
	ENDCASE
END



FUNCTION caviar_satellites_getstruct, id
	
	COMMON CAVIAR_DATA, image, catstars, imgStars, planets
	
	CATCH, error_status
	IF error_status NE 0 THEN BEGIN
		error_msg = STRCOMPRESS(STRJOIN(STRSPLIT(!ERROR_STATE.MSG, STRING(10b), /REGEX, /EXTRACT)))
		PRINT, !ERROR_STATE.MSG_PREFIX+error_msg
		CATCH, /CANCEL
		RETURN, {}
	ENDIF
	
	; GET object NAME:
	cspice_bodc2s, LONG(id), name
	
	; Get object RADII:
	cspice_bodvcd, id, 'RADII', 3, radii
	
	; Get object POLE_RA:
	cspice_bodvcd, id, 'POLE_RA', 3, polera
	
	; Get object POLE_DEC:
	cspice_bodvcd, id, 'POLE_DEC', 3, poledec
	
	; Get object RA, DEC and PIXSIZE:
	cspice_spkez, id , image.ET, 'J2000', 'CN+S', image.SPC.ID, state, ltime
	cspice_recrad, state[0:2], range, ra, dec
	pixsize = range*TAN(image.FOVPIX)*image.BINNING
	
	; Convert RA/dec to sample/line image coordinates
	radec2slcoord, ra, dec, sample, line
	
	; Is the object visible in the image?
	visible = (0 LE sample AND sample LE image.NS AND 0 LE line AND line LE image.NL) ? 1 : 0
	
	RETURN, {ID:id, NAME:name, RADII: radii, POLERA : polera[0], POLEDEC : poledec[0], $
			 RA: ra, DEC: dec, XCOORD: sample, YCOORD: line, PIXSIZE: pixsize, VISIBLE:visible}
END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;	CAVIAR_SATELLITES_LOAD
;
; PURPOSE:
;	Create and return a list of structures containing all informations about 
;	planet and satellites : planets = {name, id[, polera, poledec], RA, dec}
;	
;	North pole RA and dec are only set for the planet, not the satellites.
;	X and Y coords are set in "caviar_set_image_coords".
; CALLING SEQUENCE:
;   caviar_satellites_load, planet_id, et, sc
;	
; INPUTS:
;   planetId: Planet identification number (eg: 599->Jupiter, 699->Saturn, etc)
;
; OUTPUTS:
;   planets: List of structures {NAME:'STRING', ID:LONG, RA:DOUBLE, DEC:DOUBLE,
;									POLERA:DOUBLE, POLEDEC:DOUBLE}
;
; COMMON BLOCKS:
;	CAVIAR_DATA
;
; Modifications:
;	2012 February, 14		MEUNIER L-E			OBSPM>IMCCE
;		- Written base upon saturn/jupiter_get_planet and old caviar image load procedure
;	2012 June, 01			MEUNIER L-E			OBSPM>IMCCE
;		- Change output structure into list of structure, one for each body.
;		- Generalization of sub parts
;	2013 June				MEUNIER L-E			OBSPM>IMCCE
;		- Add search in image labels or ask the user for a valid target.
;		- 'planet' pole_ra and pole_dec are now a 3-elements array.
;		- 'satellites' have now pole_ra and pole_dec entry.
;		- Planets (planet+satellites) name, radii, pole_ra, pole_dec are set
;		  from spice kernels.
;	2014 January			MEUNIER L-E			OBSPM>IMCCE
;		- Modification of the target searching to improve compatibility.
;		- Rename in 'caviar_satellites_load'.
;	2014 October			EUNIER L-E			OBSPM>IMCCE
;		- Body structures are now prepared in caviar_satellites_getstruct function.
;		- VISIBLE tag is also definied when body structure is prepared.
;		- Returned 'planets' list now contains only the bodies with all the tags sets.
;-------------------------------------------------------------------------------
PRO caviar_satellites_load, planetId
	
	COMMON CAVIAR_DATA, image, catstars, imgStars, bodies
	
	IF N_ELEMENTS(image) EQ 0 THEN MESSAGE, "You need to load an image first"
	IF ~haveTag(image,'CMAT') || TOTAL(image.cmat) EQ 0 $
	THEN MESSAGE, "Cannot load satellites without camera pointing matrix (cmat)."
	
	PRINT, FORMAT='(/A)', ">>>>> Start getting planet and satellites"
	
	found = 0 
	planetId = LONG(planetId)
	cspice_bodc2n, planetId, planetName, found
	IF NOT found THEN MESSAGE, "Parent ID does not match any SPICE object."
	PRINT, "Parent planet: ", planetName, STRING(planetId,'(" # ",I3)')
	
	
	; Get the parent planet structure:
	planet = caviar_satellites_getstruct(planetId)
	 
	; Get satellites list of structures:
	satsHash = caviar_satellites_getHash(planetName)
	
	satIDs = satsHash.Values()
	nsat = N_ELEMENTS(values)

	satellites = LIST()
	FOREACH satID, satIDs DO BEGIN
		sati = caviar_satellites_getstruct(LONG(satID))
		IF sati NE !NULL THEN satellites.add, sati
	ENDFOREACH
	
	; Gather 'planet' & 'satellites' into the 'bodies' list:
	bodies = LIST()
	IF planet NE !NULL THEN bodies.add, planet
	bodies.add, satellites, /EXTRACT


	IF N_ELEMENTS(bodies) GT 0 $
	THEN PRINT, "<<<<< "+planet.NAME+" & "+STRING(N_ELEMENTS(satellites),'(I0)')+" satellites have been loaded succesfully!"
END


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_GETRING_GUI_EVENT
; PURPOSE: Manage events from the widget
;-------------------------------------------------------------------------------
PRO caviar_satellites_gui_event, event
	
	COMMON CAVIAR_DATA, image, catstars, imgStars, planets
		
	; Get state from the first child of the widget root:
	child = WIDGET_INFO(event.HANDLER, /CHILD)
	WIDGET_CONTROL, child, GET_UVALUE=pState
	WIDGET_CONTROL, /HOURGLASS
	
	CATCH, error_status
	IF error_status NE 0 THEN BEGIN
		res = DIALOG_MESSAGE(!ERROR_STATE.MSG, DIALOG_PARENT=event.handler, /CENTER)
		RETURN
	ENDIF
	
	uname = WIDGET_INFO(event.id, /UNAME)
	CASE uname OF
		"PLANETS_LIST": BEGIN
			; Get the selected planet id:
			planetName = STRUPCASE((*pState).pList[event.INDEX])
			cspice_bods2c, planetName, planetId, found
			IF NOT found THEN MESSAGE, "Cannot recognize selected planet name."
			
			; Get planet & satellites:
			caviar_satellites_load, planetId
			
			nfound = N_ELEMENTS(planets)
			
			; Update planets found list
			fList = STRARR(nfound > 1)
			FOR i=0, nfound-1 $
			DO fList[i] = STRING((planets[i]).ID,'(I3)')+' - '+(planets[i]).NAME
			WIDGET_CONTROL, (*pState).wPlanetFoundList, SET_VALUE=fList
			
			IF nfound EQ 0 $
			THEN MESSAGE, "Cannot load "+planetName+" and related satellites."
				
			caviar_satpos_gui_init
		END
			
		"CLOSE": WIDGET_CONTROL, event.TOP, /DESTROY
		
		ELSE: RETURN
	ENDCASE
END

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME: CAVIAR_SATELLITES_GUI
; PURPOSE:
;	Create a widget or compound widget (depend if the "PARENT" keyword is set)
;
; INPUTS:
;	All input parameters are passed as keywords or in common block
;
; KEYWORDS:
;	parent: Set this keyword to the parent widget id in witch you want to place 
;			this compound widget.
;
; OUTPUTS:
;	No explicit outputs.
;
; COMMON BLOCKS:
;	CAVIAR_DATA
; SIDE EFFECTS:
;	A window is created if no "PARENT" keyword has been set.
;
; MODIFICATION HISTORY:
;	2012, Mai			Louis-Etienne MEUNIER 		IMCCE>OBSPM
;		- Written		
;-------------------------------------------------------------------------------
PRO caviar_satellites_gui, PARENT=wMAINbase, GROUP_LEADER=groupLeader, $
							XOFFSET=xoffset, YOFFSET=yoffset
	
	COMMON CAVIAR_DATA, image, catstars, imgStars, planets
		
	
	; Test if the widget has already be launched
	IF(XRegistered('caviar_satellites_gui') NE 0) THEN RETURN
	
	
	; Get the list of planets:
	pList = ['Mercury', 'Venus', 'Earth', 'Mars', 'Jupiter', 'Saturn', 'Uranus', 'Neptune', 'Pluto']
	
	; If planets have already been found, set the list. Otherwise, define empty list:
	n_pf = N_ELEMENTS(planets)
	pfList = STRARR(n_pf > 1)
	FOR i=0, n_pf-1 $
	DO pfList[i] = STRING((planets[i]).ID, '(I3)')+' - '+(planets[i]).NAME
	pfSel = (n_pf GT 0) ? 0 : -1
		
	
	;***************************************************************************
	; Define the widget base and subwidgets tree
	IF NOT KEYWORD_SET(title) THEN title = "Satellites"
	
	IF KEYWORD_SET(wMAINbase) THEN BEGIN
		IF NOT WIDGET_INFO(wMAINbase, /VALID_ID) $
		THEN res=DIALOG_MESSAGE('Invalid parent widget identifier.',/ERROR)
		isTopBase = 0
	ENDIF ELSE BEGIN
		isTopBase = 1
		IF NOT KEYWORD_SET(groupLeader) $
		THEN wMAINbase = WIDGET_BASE(TITLE=title, XOFFSET=xoffset, YOFFSET=yoffset) $
		ELSE BEGIN
			IF WIDGET_INFO(groupLeader, /VALID_ID) $
			THEN wMAINbase = WIDGET_BASE(TITLE=title, XOFFSET=xoffset, YOFFSET=yoffset, $
										GROUP_LEADER=groupLeader, /FLOATING) $
			ELSE res=DIALOG_MESSAGE('Invalid group leader widget identifier.',/ERROR)
		ENDELSE
	ENDELSE
	
	extraBase = WIDGET_BASE(wMainBase, /COLUMN)
	wBase = WIDGET_BASE(extraBase, /ROW)
		wBase1 = WIDGET_BASE(wBase, /COLUMN, /BASE_ALIGN_LEFT)
			wPlanetListLbl = WIDGET_LABEL(wBase1, VALUE="Select parent:")
			wPlanetListList = WIDGET_LIST(wBase1, UNAME="PLANETS_LIST", $
				VALUE=pList, XSIZE=12, YSIZE=9)
		wBase2 = WIDGET_BASE(wBase, /COLUMN, /BASE_ALIGN_LEFT)
			wPlanetFoundLbl = WIDGET_LABEL(wBase2, VALUE="Loaded:")
			wPlanetFoundList = WIDGET_LIST(wBase2, UNAME="PLANETS_FOUND_LIST", $
				VALUE=pfList, XSIZE=18, YSIZE=9)

	IF isTopBase THEN close_buttonID = WIDGET_BUTTON(extraBase, VALUE="CLOSE", UNAME="CLOSE")
	
	; Create the widget (i.e. display it)
	WIDGET_CONTROL, wMAINbase, /REALIZE							
	;***************************************************************************
	
	
	state = {pList:pList, wPlanetFoundList:wPlanetFoundList}
	WIDGET_CONTROL, extraBase, SET_UVALUE=PTR_NEW(state, /NO_COPY)
		
	; Let XMANAGER takes control of widgets event processing
	XMANAGER, 'caviar_satellites_gui', wMAINbase, /JUST_REG, GROUP_LEADER=groupLeader
END
