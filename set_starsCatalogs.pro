;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Set list of cataloges and their parameters
;
; TAGS definitions:
;	'name': Set to the name of the catalogs. It will be use in the function 
;			'caviar_get_catStars'
;	'state': Set 1 to make the function to search in that catalog. Set 0 otherwise.
;	'srchmag_min/max': Set to the magnitude min/max limit for searching stars in 
;			catalogs.
;	'prefmag_min/max': Set to the magnitude min/max limit for prefered catalog 
;			result.
;
; LIST order:
;	The catalogs order in starCatalogs list will define the default preference 
;	order to select between identical stars found in different catalogs. This 
;	order can be changed with the 'caviar_catstars_gui' GUI.
; MODIFICATIONS:
;       2017, April         Nick Cooper                            QMUL
;               - Added UCAC5 catalogue
;-------------------------------------------------------------------------------
PRO set_starsCatalogs, starsCatalogs

 	tycho2 = {name:'TYCHO2', $
	   		  state:1, $
			  srchmag_min: -5.0D0, $
;	 		   srchmag_max: 13.0D0, $
                          srchmag_max: 11.0D0, $
			  prefmag_min:  9.5D0, $
			  prefmag_max: 14.0D0 $
			 }
			 
        ucac5  = {name:'UCAC5',  $
			  state:1, $
			  srchmag_min: -2.0D0, $
; 			   srchmag_max: 13.0D0, $
                          srchmag_max: 11.0D0, $
			  prefmag_min: -2.0D0, $
			  prefmag_max: 16.0D0 $
			 }
        starsCatalogs = LIST(tycho2, ucac5)

	RETURN
END

;FUNCTION getStarsCatalogs
;njc Function not used?
;
;        COMMON CAVIAR_DATA, image
;
;        RETURN, {NAME:LIST('TYCHO2','UCAC5'), $
;			 STATE:LIST(1,1), $
;			 SRCHMAGMIN: LIST(-5.0D0, -5.0D0, -2.0D0), $
;			 SRCHMAGMAX: LIST(13.0D0, 13.0D0, 13.0D0), $
;			 PREFMAGMIN: LIST( 9.5D0, -5.0D0,  9.5D0), $
;			 PREFMAGMAX: LIST(14.0D0, -2.0D0, 16.0D0) $
;			}
;
;END
