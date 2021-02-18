FUNCTION getMyCT
	
	;# Most of the colors (and names) are taken from wikipedia "web colors" article (X11 section)
	
	ct=[ [BYTARR(256)], [BYTARR(256)], [BYTARR(256)] ]

	;# Gray colors (white to black ; 0 -> 19)
	ct[  0,*] = [255, 255, 255]	;White
	ct[  1,*] = [248, 248, 248]
	ct[  2,*] = [234, 234, 234]
	ct[  3,*] = [220, 220, 220]	;Gainsboro
	ct[  4,*] = [211, 211, 211]	;LightGray
	ct[  5,*] = [192, 192, 192]	;Silver
	ct[  6,*] = [180, 180, 180]
	ct[  7,*] = [169, 169, 169]	;DarkGray
	ct[  8,*] = [155, 155, 155]
	ct[  9,*] = [140, 140, 140]
	ct[ 10,*] = [128, 128, 128]	;Gray
	ct[ 11,*] = [116, 116, 116]
	ct[ 12,*] = [105, 105, 105]	;DimGray
	ct[ 13,*] = [ 90,  90,  90]
	ct[ 14,*] = [ 75,  75,  75]
	ct[ 15,*] = [ 60,  60,  60]
	ct[ 16,*] = [ 45,  45,  45]
	ct[ 17,*] = [ 30,  30,  30]
	ct[ 18,*] = [ 15,  15,  15]
	ct[ 19,*] = [  0,   0,   0]	;Black
	
	;# Magenta colors (20 -> 49)
	ct[ 20,*] = [255,   0, 255]	;Magenta
	ct[ 21,*] = [  0,   0,   0]
	ct[ 22,*] = [  0,   0,   0]
	ct[ 23,*] = [  0,   0,   0]
	ct[ 24,*] = [  0,   0,   0]
	ct[ 25,*] = [  0,   0,   0]
	ct[ 26,*] = [  0,   0,   0]
	ct[ 27,*] = [  0,   0,   0]
	ct[ 28,*] = [  0,   0,   0]
	ct[ 29,*] = [  0,   0,   0]
	ct[ 30,*] = [  0,   0,   0]
	ct[ 31,*] = [  0,   0,   0]
	ct[ 32,*] = [  0,   0,   0]
	ct[ 33,*] = [  0,   0,   0]
	ct[ 34,*] = [  0,   0,   0]
	ct[ 35,*] = [  0,   0,   0]
	ct[ 36,*] = [  0,   0,   0]
	ct[ 37,*] = [  0,   0,   0]
	ct[ 38,*] = [  0,   0,   0]
	ct[ 39,*] = [  0,   0,   0]
	ct[ 40,*] = [  0,   0,   0]
	ct[ 41,*] = [  0,   0,   0]
	ct[ 42,*] = [  0,   0,   0]
	ct[ 43,*] = [  0,   0,   0]
	ct[ 44,*] = [  0,   0,   0]
	ct[ 45,*] = [  0,   0,   0]
	ct[ 46,*] = [  0,   0,   0]
	ct[ 47,*] = [  0,   0,   0]
	ct[ 48,*] = [  0,   0,   0]
	ct[ 49,*] = [  0,   0,   0]
	
	;# Red colors (50 -> 79)
	ct[ 50,*] = [255,   0,   0]	;Red
	ct[ 51,*] = [  0,   0,   0]
	ct[ 52,*] = [255, 153,   0] ;Orange
	ct[ 53,*] = [  0,   0,   0]
	ct[ 54,*] = [139,   0,   0]	;DarkRed
	ct[ 55,*] = [128,   0,   0]	;Maroon
	ct[ 56,*] = [  0,   0,   0]
	ct[ 57,*] = [  0,   0,   0]
	ct[ 58,*] = [  0,   0,   0]
	ct[ 59,*] = [  0,   0,   0]
	ct[ 60,*] = [  0,   0,   0]
	ct[ 61,*] = [  0,   0,   0]
	ct[ 62,*] = [  0,   0,   0]
	ct[ 63,*] = [  0,   0,   0]
	ct[ 64,*] = [  0,   0,   0]
	ct[ 65,*] = [  0,   0,   0]
	ct[ 66,*] = [  0,   0,   0]
	ct[ 67,*] = [  0,   0,   0]
	ct[ 68,*] = [  0,   0,   0]
	ct[ 69,*] = [  0,   0,   0]
	ct[ 70,*] = [  0,   0,   0]
	ct[ 71,*] = [  0,   0,   0]
	ct[ 72,*] = [  0,   0,   0]
	ct[ 73,*] = [  0,   0,   0]
	ct[ 74,*] = [  0,   0,   0]
	ct[ 75,*] = [  0,   0,   0]
	ct[ 76,*] = [  0,   0,   0]
	ct[ 77,*] = [  0,   0,   0]
	ct[ 78,*] = [  0,   0,   0]
	ct[ 79,*] = [  0,   0,   0]
	
	
	
	
	;# Yellow colors (80 -> 109)
	ct[ 80,*] = [255, 255,   0]	;Yellow
	ct[ 82,*] = [200, 200,   0] 		;My DarkYellow 2
	ct[ 84,*] = [150, 150,   0] 		;My DarkYellow 4
	ct[ 92,*] = [200, 220,   0]			;My DarkYellow-Green 2
	ct[ 93,*] = [180, 200,   0]			;My DarkYellow-Green 3
	
	
	
	;# Green colors (110 -> 139)
	ct[110,*] = [  0, 255,   0]	;Lime
	
	ct[115,*] = [  0, 128,   0]	;Green
	ct[116,*] = [  0, 100,   0]	;DarkGreen
	
	ct[120,*] = [  0, 250, 154]	;MediumSpringGreen
	ct[121,*] = [  0, 255, 127]	;SpringGreen
	ct[122,*] = [  0, 255,  60]	;My LightGreen
	ct[123,*] = [  0, 200,  60]	;My LightGreen 2
	
	ct[130,*] = [153, 255,   0] ;Chartreuse Green
	
	
	;# Cyan colors (140 -> 169)
	ct[140,*] = [  0, 255, 255]	;Cyan
	ct[141,*] = [  0, 200, 255]	;My Light Cyan
	ct[144,*] = [  0, 139, 139]	;DarkCyan
	ct[145,*] = [  0, 128, 128]	;Teal
	
	
	
	;# Blue colors (170 -> 199)
	ct[170,*] = [  0,   0, 255]	;Blue
	ct[171,*] = [  0,   0, 205]	;MediumBlue
	
	ct[173,*] = [  0,   0, 139]	;DarkBlue
	ct[174,*] = [  0,   0, 128]	;Navy
	ct[178,*] = [ 51, 102, 153] ;Azure
	ct[180,*] = [  0, 120, 255]	;My LightBlue
	ct[181,*] = [135, 206, 250]	;LightSkyBlue
	
	ct[190,*] = [148,   0, 211]	;DarkViolet
	ct[191,*] = [125,   0, 255]			;My	DarkViolet (more blue)
	
	RETURN, ct
END


FUNCTION color, color_name, SILENT=SILENT
	
	IF KEYWORD_SET(SILENT) EQ 0 THEN MESSAGE, "Color table has been changed!", /CONTINUE

	ncolor = N_ELEMENTS(color_name)
	IF ncolor EQ 0 THEN RETURN, 0
	
	index = INTARR(ncolor)
	FOR i=0, ncolor-1 DO BEGIN
		CASE STRUPCASE(color_name[i]) OF
			'WHITE':				index[i]=0
			
			'BLACK':				index[i]=19
			
			'MAGENTA':				index[i]=20
			
			
			'RED':					index[i]=50
			'ORANGE':				index[i]=52
			
			'YELLOW':				index[i]=80
			'DARK YELLOW 2':		index[i]=82
			'DARK YELLOW 4':		index[i]=84
			'DARK YELLOW GREEN 2': 	index[i]=92
			'DARK YELLOW GREEN 3': 	index[i]=93
			
			'LIME':					index[i]=110
			'GREEN':				index[i]=115
			'SPRING GREEN':			index[i]=121
			'LIGHT GREEN':			index[i]=122
			'LIGHT GREEN 2':		index[i]=123
			'CHARTREUSE GREEN':		index[i]=130
			
			'CYAN':					index[i]=140
			'LIGHT CYAN':			index[i]=141
			'DARK CYAN':			index[i]=144
			
			
			'BLUE':					index[i]=170
			'BLUE DARK':			index[i]=173
			'AZURE':				index[i]=178
			'LIGHT BLUE': 			index[i]=180	
			'LIGHT SKY BLUE':		index[i]=181
			'DARK VIOLET':			index[i]=190

							
			ELSE: BEGIN
				index[i]=0
				MESSAGE, "Found no color matches. Loading white in replacement.", /CONTINUE
			END
		ENDCASE
	ENDFOR
	TVLCT, getMyCT()
	
	If ncolor EQ 1 THEN RETURN, index[0] ELSE RETURN, index
END

