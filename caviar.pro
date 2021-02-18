;.FULL_RESET_SESSION

DEVICE, DECOMPOSED=0
DEVICE, RETAIN=2

pathsep = PATH_SEP()

;################ USER-DEFINED ENVIRONMENT VARIABLES & OPTIONS ##################
;Path to Caviar source code:
caviarDir = '/media/sf_Project/caviar_r1.0'

;Path to SPICE Toolkit (directory containing file icy.dlm):
icyDir = '/media/sf_Project/naif_toolkit/icy/lib'

;The path to the images may optionally be changed here.
;The default setting, below, points to the /demo_images in the caviar source directory:
SETENV, 'CAVIAR_IMAGES_DIRECTORY='+caviarDir+pathsep+'demo_images'+pathsep
;Or alternatively, for example, use the local working directory:
;SETENV, 'CAVIAR_IMAGES_DIRECTORY=.'

;######################### OTHER ENVIRONMENT VARIABLES ###########################
SETENV, 'CAVIAR_ICONS_PATH='+caviarDir+pathsep+'ICONS'+pathsep
SETENV, 'CAVIAR_CAMPARAMS_FILE='+caviarDir+pathsep+'camparams.txt'
SETENV, 'CAVIAR_VERSION_TEXT=CaVIaR r1.0 release post 01 June 2017'

DLM_REGISTER, icyDir+'icy.dlm'

!PATH = EXPAND_PATH('+'+caviarDir)+':'+!PATH

;############################ DEFINE COMMON BLOCKS #############################
COMMON CAVIAR_GUI, wCAVIARtlb, wFITSATLIMBbase, wRINGSLOADbase, wCATSTARSLOADbase, wSATPOSbase
COMMON CAVIAR_DATA, image, catstars, imgstars, planets, rings
COMMON CAVIAR_PARAMS, dispParams, imgDraw, dispProName, starsCatalogs, find_imgStars_params
SETENV, 'SPICEKERNEL_CASSINI='       +caviarDir+'/kernels.ker'

COMMON CAVIAR_SATPOS, selSatIndex, wSatTblID, wSaveLbl, wSLFbaseYsize
COMMON CAVIAR_SATLIMBFIT, satModel, satLimb

;############################# COMPILE ROUTINES ################################
PRINT, FORMAT='(%"\n", A)', "## Compiling math tools routines"
.r getDivisors.pro

PRINT, FORMAT='(%"\n", A)', "## Compiling structure tools routines"
.r haveTag.pro
.r setStruct.pro

PRINT, FORMAT='(%"\n", A)', "## Compiling routines for coordinates conversion"
.r radec2radlon.pro
.r radec2slcoord.pro
.r slcoord2radec.pro
.r caviar_m2rdt.pro
.r caviar_updtCoords.pro

PRINT, FORMAT='(%"\n", A)', "## Compiling routines for setting parameters"
.r set_starsCatalogs.pro
.r set_dispParams.pro

PRINT, FORMAT='(%"\n", A)', "## Compiling routines for global caviar purpose"
.r caviar_data_routinesWrapper.pro
.r caviar_save2qmpf.pro
.r dialog_getValue.pro
.r caviar_xmove.pro
.r caviar_xvarinfo.pro
.r caviar_display.pro

PRINT, FORMAT='(%"\n", A)', "## Compiling routines for displaying the image"
.r color.pro

.r xloadct3.pro

PRINT, FORMAT='(%"\n", A)', "## Compiling RINGS package routines:"
.r caviar_rings_pkg.pro

PRINT, FORMAT='(%"\n", A)', "## Compiling SATELLITES package routines:"
.r caviar_satellites_pkg.pro

PRINT, FORMAT='(%"\n", A)', "## Compiling CATSTARS package routines:"
.r caviar_catstars_pkg.pro

PRINT, FORMAT='(%"\n", A)', "## Compiling IMGSTARS package routines:"
.r caviar_imgstars_pkg.pro

PRINT, FORMAT='(%"\n", A)', "## Compiling MATCH package routines:"
.r caviar_matchstars_pkg.pro

PRINT, FORMAT='(%"\n", A)', "## Compiling REPOINT package routines:"
.r caviar_iteRepoint.pro
.r caviar_repoint_pkg.pro

PRINT, FORMAT='(%"\n", A)', "## Compiling SATPOS package routines:"
.r caviar_satpos_gui.pro

PRINT, FORMAT='(%"\n", A)', "## Compiling IMAGE package routines:"
.r caviar_image_load_gui


PRINT, FORMAT='(%"\n", A)', "## Compiling routines of GUI
.r caviar_gui.pro

PRINT, FORMAT='(%"\n", A)', "----------> CaVIaR r1.0 <----------"
;########################## START PLAYING CaVIaR ###############################

set_dispParams, dispParams, /SET_DEFAULT
set_starsCatalogs, starsCatalogs

; Get Monitor information
oInfo = OBJ_NEW('IDLsysMonitorInfo')
rects = oInfo->GetRectangles(/EXCLUDE_TASKBAR)
primaryIndex = oInfo->GetPrimaryMonitorIndex()
OBJ_DESTROY, oInfo

; Get the index of the monitor on which to display CaVIaR (centered):
maxHeight = MAX(rects[3,*], monIndex)	;Use the monitor with the maximum height
;monIndex = primaryIndex				;Use the primary monitor 

; Get the offset values to centered the GUI:
xoff = rects[0,monIndex] + 0.5*rects[2,monIndex] - 400
yoff = rects[1,monIndex] + 0.5*rects[3,monIndex] - 150

;*******************************************************************************
; Launch the GUI to get the image:
;*******************************************************************************
caviar_image_load_gui, XOFFSET=xoff, YOFFSET=yoff

;*******************************************************************************
; Launch CaVIaR main interface:
;*******************************************************************************
autoloadsats = 1        ;Automatically load satellites and parent planet when GUI launch?
auloadstars = 1         ;Automatically load stars from catalogues when GUI launch?
IF ISA(image, 'STRUCT') THEN caviar_gui, AUTOLOADSATS=autoloadsats, AUTOLOADSTARS=auloadstars
