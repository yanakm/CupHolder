;----------------------------------------------------------------------------------
; 				TWO HANDLED CUP HOLDER				   
;----------------------------------------------------------------------------------

 ;Load Vl library
  (vl-load-com)

;GLOBAL VARIABLES
(setq wallThickness 3.5)
(setq baseThickness 5.0)

(setq downRadius 31.0)
(setq upperRadius 60.0)
(setq height 120.0)
(setq bottomless 1)

(setq cutoutWidth 25.0)
(setq cutoutDisplacement 50.0)
(setq cutoutTop 10.0)

(setq handleType "T")
(setq handleSize 26.0)
(setq handleThickness "M")

; Global variable to hold saved settings
(setq CupHolderEnv nil)

;Global variable that shows if utils file is loaded
(setq LoadUtils 0)

;----------------------------------------------------------------------------------

;BASE OF THE CUP HOLDER
;Draws and returns the base of the cupholder
(defun base (R bottomless / paddedR paddedRD holeR c  baseSolid loftSolid ss baseProfile p1 p2 p3 p4)
  (setq paddedR (+ R wallThickness)) ; The inner radius + wall thickness = the radius of the upper base
  (setq paddedRD (* paddedR 1.075)) ; A bit bigger lower base
  (setq holeR (* R 0.7)) ; 70% of the base will be hollowed out

  ; Making the part which will be subtracted
  (cond
  ((= bottomless 1)
	   (entmake
		 (list
		 '(0 . "CIRCLE")
		 '(10 0.0 0.0 0.0)
	     	(cons 40  (* R 0.7))
		 ))
	  (setq c (entlast))

   	  (UT_extrude mSpace (list c) 6.0)
	  (setq baseSolid (entlast))
  ))
; Creating the profile for the base
  (setq p1 (list 0.0 0.0 0.0)
	p2 (polar p1 0 paddedRD)
	p3 (polar p1 (/ pi 2.0)  baseThickness)
	p4 (polar p3 0 paddedR)
	)

  (setq baseProfile
	  (entmakex (list
	        '(0 . "LWPOLYLINE")
	        '(100 . "AcDbEntity")
	        '(100 . "AcDbPolyline")
	        (cons 90 4)
	        '(70 . 1) ; closed polyline
		 ;vertices
		(cons 10 p2)
		(cons 10 p1)
		(cons 10 p3)
		(cons 10 p4)
		)
		
	      )
	)
  ;revolving base profile
  (UT_revolve mSpace (list baseProfile) p1 p3 360.0)

  ;Rotating the base
  (setq loftSolid (ssadd (entlast) (ssadd)))

  (UT_rotate3D loftSolid p1 (list 1 0 0) 90.0)

  (setq loftSolid (entlast))

  ; Subtract the base only if the cup is bottomless
  (if bottomless
    	; Turn entity name to a selection then subtract
  	(UT_subtract (ssadd loftSolid (ssadd)) (ssadd baseSolid (ssadd)))
  )

  ; Returning the base
  loftSolid
  ); end of defun : base

;----------------------------------------------------------------------------------

;OUTER BODY OF THE CUP HOLDER
;Draws and returns the outer body of the cup
(defun outerBody (dR uR H / loftSolidOuter ss outerBodyProfile p1 p2 p3 p4)
  ; Creating the profile for   outer Body
  (setq p1 (list  0.0 5.0 0.0 )
	p2 (polar p1 0 (+ dR wallThickness))
	p3 (polar (list 0.0 0.0 0.0) (/ pi 2.0)  (+ baseThickness H))
	p4 (polar p3 0 (+ uR 3.5))
	)

  (setq outerBodyProfile
	  (entmakex (list
	        '(0 . "LWPOLYLINE")
	        '(100 . "AcDbEntity")
	        '(100 . "AcDbPolyline")
	        (cons 90 4)
	        '(70 . 1) ; closed polyline
		 ;vertices
		(cons 10 p2)
		(cons 10 p1)
		(cons 10 p3)
		(cons 10 p4)
		)
		
	      )
	)
  ;revolving outer body profile
  (UT_revolve mSpace (list outerBodyProfile) p1 p3 360.0)

  ;Rotating the outer body
  (setq loftSolidOuter (ssadd (entlast) (ssadd)))

  (UT_rotate3D loftSolidOuter (list 0 0 0) (list 1 0 0) 90.0)
  (setq loftSolidOuter (entlast))

  loftSolidOuter
  );end of defun : outerBody

;-------------------------------------------------------------------------------------------

; INNER BODY - THE SUBTRACTING PART
; Draws and returns the inner body
(defun innerBody (dr uR H /  loftSolidInner ss innerBodyProfile p1 p2 p3 p4)
  ;Creating the profile for inner body
  (setq p1 (list  0.0 baseThickness 0.0 )
	p2 (polar p1 0 dR)
	p3 (polar (list 0.0 0.0 0.0) (/ pi 2.0)  (+ baseThickness H))
	p4 (polar p3 0 uR)
	)

  (setq innerBodyProfile
	  (entmakex (list
	        '(0 . "LWPOLYLINE")
	        '(100 . "AcDbEntity")
	        '(100 . "AcDbPolyline")
	        (cons 90 4)
	        '(70 . 1) ; closed polyline
		 ;vertices
		(cons 10 p2)
		(cons 10 p1)
		(cons 10 p3)
		(cons 10 p4)
		)
		
	      )
	)
  ;revolving inner body profile
  (UT_revolve mSpace (list innerBodyProfile) p1 p3 360.0)

  ;Rotating inner body
  (setq loftSolidInner (ssadd (entlast) (ssadd)))

  (UT_rotate3D loftSolidInner (list 0 0 0) (list 1 0 0) 90.0)
  
  (setq loftSolidInner (entlast))

  ; Returning the inner body solid
  loftSolidInner
  
  ); end of defun : innerBody

;------------------------------------------------------------------------------------------

;SWEEP FIGURE - THE SHAPE OF THE HANDLE WIDTH
(defun sweepFigure ( W H / x y p1 p2 p3 p4 p5 p6 p7 p8 R bulge1 bulge2 bulge3 bulge4)
   (setq x (/ W 2.0))
   (setq y (/ H 2.0))
   (setq R 2.0
	p1 (list x (- y R))
	p2 (list x (+ (- y) R))
	p3 (list (- x R) (- y))
	p4 (list (+ (- x) R) (- y))
	p5 (list (- x) (+ (- y) R))
	p6 (list (- x) (- y R))
	p7 (list (+ (- x) R) y)
	p8 (list (- x R) y)
	)
  
   ; calculate bulges
     (setq bulge1 (- (getBulge p8 p1 R)))
     (setq bulge2 (- (getBulge p2 p3 R)))
     (setq bulge3 (- (getBulge p4 p5 R)))
     (setq bulge4 (- (getBulge p6 p7 R)))
    
   
  (setq fig (entmakex
      (list
        '(0 . "LWPOLYLINE")
        '(100 . "AcDbEntity")
        '(100 . "AcDbPolyline")
        (cons 90 8)
        '(70 . 1) ; closed polyline
	 ;vertices
	(cons 10 p8)
	(cons 42 bulge1)
	(cons 10 p1)
	(cons 10 p2)
	(cons 42 bulge2)
	(cons 10 p3)
	(cons 10 p4)
	(cons 42 bulge3)
	(cons 10 p5)
	(cons 10 p6)
	(cons 42 bulge4)
	(cons 10 p7)
	(cons 10 p8)
	
      	)
      )
  )

  ; Returning the sweep figure
  fig
  ); end of defun : sweepFigure

;------------------------------------------------------------------------------------------------------

; HANDLE
; Draws and returns a generic  handle
; H - height, S - Start (insertion point), F - factor
; hT - handle type, hS - handle size, hTh - handle thickness
(defun handle (H S F hT hS hTh / p1 p2 p3 p4 p5 p6 figW figH widthFactor)
  
  (setq figW 10.0)
  (setq figH 6.0)
  (setq widthFactor (/ (- hS 10.0) H))

  ; managing the thickness
  (if (= hTh "S") (progn
	(setq figW 10.0)
  	(setq figH 6.0)
		   ))
   (if (= hTh "M") (progn
	(setq figW 12.0)
  	(setq figH 7.0)
		   ))
   (if (= hTh "L") (progn
	(setq figW 14.0)
  	(setq figH 8.0)
		   ))

  (setq handleLine nil)

  (if (= hT "T") (progn
	  ; The main shape of the handle
	  (setq p1 (list (- figH) 0.0))
	  (setq p2 (list (* widthFactor H) 0.0))
	  (setq p3 (list (* (+ widthFactor 0.2) H) (-(* widthFactor H))))
	  (setq p4 (list (- 0 (/ H F) figH) (- H)))

	  	;-------------------------------------------
   		; 		COMMAND		            
   		;-------------------------------------------
	  
	  (command "_.PLINE" p1 p2 "A" p3 p4 "")
	  (setq handleLine (entlast))))

  (if (= hT "R") (progn
	   ; The main shape of the handle
	  (setq p1 (list (- figH) 0.0))
	  (setq p2 (list (* widthFactor H) 0.0))
	  (setq p3 (list (* (+ widthFactor 0.2) H) (-(* 0.25 H))))
	  (setq p4 (list (* (+ widthFactor 0.2) H) (-(* 0.75 H))))
	  (setq p5 (list (* widthFactor H) (- H) ))
	  (setq p6 (list (- 0 (/ H F) figH) (- H)))
	  
	  	;-------------------------------------------
   		; 		COMMAND		            
   		;-------------------------------------------
	  (command "_.PLINE" p1 p2 "A" p3 "L" p4 "A" p5 "L" p6 "" )
	  (setq handleLine (entlast))))

  (if (= hT "O") (progn
	   ; The main shape of the handle
	  (setq p1 (list (- figH) 0.0))
	  (setq p2 (list (* widthFactor H) (/ H -2.0)))
	  (setq p3 (list (- 0 (/ H F) figH) (- H)))

	  	;-------------------------------------------
   		; 		COMMAND		            
   		;-------------------------------------------
	  (command "_.ARC" p1 p2 p3 "" )
	  (setq handleLine (entlast))))

  ; Getting the sweep figure
  (setq fig (sweepFigure figH figW))
  (setq p1 (list (- figH) 0.0 0.0))
  (UT_move3D (ssadd fig (ssadd)) (list 0.0 0.0 0.0) p1)
  (UT_rotate3D (ssadd fig (ssadd)) p1 (list (car p1) 1 0) 90.0)

  ; Sweeping
  (UT_sweep (getModelSpace)  (list fig) handleLine)
  ;(entdel handleLine)
  
  (setq handleSolid (entlast))

  ; Rotating and moving to the correct spot
  (UT_rotate3D (ssadd handleSolid (ssadd)) (list 0 0 0) (list 1 0 0) 90.0)

  (setq handleSolid (entlast))

  ; Returning the generic handle
  handleSolid
)
;---------------

; RIGHT  HANDLE
; Draws and returns a right handle, moving it to place
(defun handleRight (H S F hT hS hTh / )
  (setq obj (handle H S F hT hS hTh))
  
  (UT_move3D (ssadd obj (ssadd)) (list 0.0 0.0 0.0) S)
  (setq handleSolid (entlast))
  handleSolid
  )
;---------------

; LEFT  HANDLE
;Draws and returns a left handle, moving it to place
(defun handleLeft (H S F hT hS hTh / )
  (setq obj (handle H S F hT hS hTh))

  		;-------------------------------------------
   		; 		COMMAND		            
   		;-------------------------------------------
  (command "_.ROTATE3D" handleSolid "" "Z" '(0 0 0) "180" "")
  (setq handleSolid (entlast))
  
  		;-------------------------------------------
   		; 		COMMAND		            
   		;-------------------------------------------
  
  (command "_.3DMOVE" obj "" '(0 0 0) S "")
  (setq handleSolid (entlast))
  handleSolid
  )

;---------------------------------------------------------------------------------------------

;CUTOUT
;Draws and returns the cutout piece
(defun cutout (H W displacement top / p4 p5 p6 p7 R b1 b2 b3 b4 b5 b6 b7 b8 bulge1 bulge2 bulge3 bulge4)
  	;; Creating the points of the cutout polyline
  	;; And creating the polyline
  	(setq p4 (list (* 20 W) H 0.0))
  	(setq p5 (list (* 20 W) (+ H 10.0 top) 0.0))
  	(setq p6 (list (- (* 20 W)) (+ H 10.0 top) 0.0))
  	(setq p7 (list (- (* 20 W)) H  0.0))
  	(setq R 8.0
	      b1 (list (+ (/ W -2.0) R) 0.0 0.0)
	      b2 (list (- (/ W 2.0) R) 0.0 0.0)
	      b3 (list (/ W 2.0) R 0.0)
	      b4 (list (/ W  2.0) (- H R) 0.0)
	      b5 (list (+ (/ W  2.0) R) H 0.0)
	      b6 (list (- (+ R (/ W 2.0))) H 0.0)
	      b7 (list (/ W -2.0) (- H R) 0.0)
	      b8 (list (/ W -2.0) R 0.0)
	      )

	  ; calculate bulges
	     (setq bulge1 (getBulge b8 b1 R))
	     (setq bulge2 (getBulge b2 b3 R))
	     (setq bulge3 (- (getBulge b4 b5 R)))
	     (setq bulge4 (- (getBulge b6 b7 R)))
	    
	   
	  (setq fig (entmakex
	      (list
	        '(0 . "LWPOLYLINE")
	        '(100 . "AcDbEntity")
	        '(100 . "AcDbPolyline")
	        (cons 90 13)
	        '(70 . 1) ; closed polyline
		 ;vertices
		(cons 10 b8)
		(cons 42 bulge1)
		(cons 10 b1)
		(cons 10 b2)
		(cons 42 bulge2)
		(cons 10 b3)
		(cons 10 b4)
		(cons 42 bulge3)
		(cons 10 b5)
		(cons 10 p4)
		(cons 10 p5)
		(cons 10 p6)
		(cons 10 p7)
		(cons 10 b6)
		(cons 42 bulge4)
		(cons 10 b7)
		(cons 10 b8)
		
	      )
	      )
	  )
  
  	;; Extruding, rotating and positioning in the right place
  	(UT_extrude mSpace (list fig) 250.0)
  
  	(setq fig (entlast))

  	(UT_rotate3D (ssadd fig (ssadd)) (list 0 0 0) (list 1 0 0) 90.0)
  
  	(UT_move3D (ssadd fig (ssadd)) (list 0 0 0) (list 0.0 125.0 (+ displacement baseThickness)))

  	;; Returning the cutout
  	fig
  ); end of defun : cutout


;Draws the entire cupholder 
(defun DrawCupHolder ( /
			cutoutHeight
			baseSolid
			cutoutSolid) 

  	; Calculating aditional variables
  	(setq cutoutHeight (- height cutoutTop cutoutDisplacement))
  	(setq finalHeight (- height cutoutTop))
  	(setq factor (/ height (- upperRadius downRadius)))
  	(setq ip1 (list (- (+ (/ finalHeight factor) downRadius))
			0.0
			(* 0.90 (+ finalHeight baseThickness))))
  	(setq ip2 (list (+ (/ finalHeight factor) downRadius)
			0.0
			(* 0.90 (+ finalHeight baseThickness))))

  	; Creating the main solids
  	(setq baseSolid (base downRadius bottomless))
	(setq outerBodySolid (outerBody downRadius upperRadius height))

  	; Creating the handle solids
  	(setq rightHandleSolid (handleRight (* 0.7 finalHeight) ip2 factor handleType handleSize handleThickness))
  	(setq leftHandleSolid (handleLeft (* 0.7 finalHeight) ip1 factor handleType handleSize handleThickness))

  	; Combining all the elements
	(setq ss (ssadd))
	(if baseSolid (ssadd baseSolid ss))
	(if outerBodySolid (ssadd outerBodySolid ss))
  	(if rightHandleSolid (ssadd rightHandleSolid ss))
  	(if leftHandleSolid (ssadd leftHandleSolid ss))
  
  	(UT_union ss)
	(setq mainSolid (entlast))
  	

  	; Subtracting the inner body - hollowing out the cup holder
  	(setq innerBodySolid (innerBody downRadius upperRadius height))

  	; Turn entity name to a selection then subtract
  	(UT_subtract (ssadd mainSolid (ssadd)) (ssadd innerBodySolid (ssadd)))
  	(setq mainSolid (entlast))

  	; Subtracting the cutout - creating the distance from top and slits
  	(setq cutoutSolid (cutout cutoutHeight cutoutWidth cutoutDisplacement cutoutTop))
  
  	; Turn entity name to a selection then subtract
  	(UT_subtract (ssadd mainSolid (ssadd)) (ssadd cutoutSolid (ssadd)))
  	(setq mainSolid (entlast))
  
  ); end of defun : DrawCupHolder

;---------------------------------------------------------------------------------------------------
; function for loading vla-functions from external file VlaUtils.LSP
;NB Must be the first function in main function
(defun LoadVlaUtils ( / filePath)
  ;Finds the full path to a file
  ;NB Works only if the file is in Support File Search Path
  (setq filePath (findfile "CupHolder/VlaUtils.LSP"))
  
  ;Check if file exists
  (if filePath
    (progn
      ;Check if utils file has already been loaded
      (if (= LoadUtils 0)
        (progn
          (load filePath)
	  ;TODO - check if all functions from utils are loaded

	  ;Set LoadUtils to show that utils are loaded
          (setq LoadUtils 1)
          
          (princ (strcat "\nLoaded: VlaUtils.LSP"))
        )
        (princ "\nAlready loaded: VlaUtils.LSP")
      )
    )
    (princ "\nERROR: File not found: VlaUtils.LSP")
  )

  (princ)
  
  ); end of defun : LoadVlaUtils

;---------------------------------------------------------------------------------------------------

; MAIN FUNCTION
; the main function that calls the dialog window
(defun C:CUPHOLDER ( / dcl_id result DialogPath dir)
  ; Add program path to Support File Search Path
  (if (not (findfile "DialogWindows/cupHolderWindow.dcl"))
      (progn
	(setq dir (getProgramDir))
	(if dir 
      	    (addSupportPath dir  nil)
	    (progn
		    (alert "Program location not specified.")
	            (exit)
	    );progn
	  );if
	);progn
    );if

  
  ; Load functions from VlaUtils
  (LoadVlaUtils)
  ; get Model Space for vla-functions
  (setq mSpace (getModelSpace))

  ;Check if utils are loaded
  (if (= LoadUtils 0)
    (progn
      (alert "Cannot load file: VlaUtils.LSP.")
      (exit)
    )
    )
  ; Use test function
  ;(test)

  
  ;Finds the full path to a file
  ;NB Works only if the file is in Support File Search Path
  (setq DialogPath (findfile "CupHolder/DialogWindows/cupHolderWindow.dcl"))
  ; Load DCL file
  (setq dcl_id (load_dialog DialogPath))

  (if (not (new_dialog "CupDialog" dcl_id))
    (progn
      (alert "Cannot load dialog.")
      (exit)
    )
  )

  ; Populate Handle Type
  (start_list "handle_type")
  (mapcar 'add_list '("Triangle" "Oval" "Rectangle"))
  (end_list)

  ; Populate Handle Width
  (start_list "handle_width")
  (mapcar 'add_list '("Thin" "Medium" "Thick"))
  (end_list)

  ; Set defaults
  (set_tile "handle_type" "0")
  (set_tile "handle_width" "1")
  (set_tile "handle_size" "2.6")
  
  (set_tile "cup_height" "10")
  (set_tile "lower_dia" "5")
  (set_tile "upper_dia" "7")
  (set_tile "bottomless" "1")

  (set_tile "cut_width" "2")
  (set_tile "cut_bottom_dist" "2")
  (set_tile "cut_top_dist" "1.5")

  
  ; OK button
	(action_tile "accept" "(acceptCallback)" )
  
  ; Cancel button
  (action_tile "cancel" "(done_dialog 0)")

  ; Show dialog
  (setq result (start_dialog))

  ; Cleanup
  (unload_dialog dcl_id)

  ; Process result
  (if (= result 1)
  (progn
    (SaveAndPrepareEnvironment)
    (agregateValues)
    (DrawCupHolder)
    (RestoreEnvironment)
  )
)

  (princ)
); end of defun : CUPHOLDER

;---------------------------------------------------------------------------------------------

; SETTING THE VALUES
; sets the global values with the agregated values from the dialog window
(defun agregateValues ( / )
  	(setq downRadius (* (atof lowerDiamVal) 5.0))
	(setq upperRadius (* (atof upperDiamVal) 5.0))
	(setq height (* (atof heightVal) 10.0))
	(setq bottomless (if (= bottomlessVal "1") 1 0))

	(setq cutoutWidth (* (atof cutoutWidthVal) 10.0))
	(setq cutoutDisplacement (* (atof cutoutBottomVal) 10.0))
	(setq cutoutTop (* (atof cutoutTopVal) 10.0))

	(setq handleType (cond ((= handleTypeVal "0") "T")
			       ((= handleTypeVal "1") "O")
			       ((= handleTypeVal "2") "R")))
	(setq handleSize (* (atof handleSizeVal) 10.0))
	(setq handleThickness (cond ((= handleThicknessVal "0") "S")
			       ((= handleThicknessVal "1") "M")
			       ((= handleThicknessVal "2") "L")))
  ); end of defun : agregateValues

;-------------------------------------------------------------------------------------------

; ACCEPT CALLBACK
; runs after the accept button of the dialog window has been clicked
(defun acceptCallback ( / )
	  (setq heightVal (get_tile "cup_height"))
	  (setq lowerDiamVal (get_tile "lower_dia"))
	  (setq upperDiamVal (get_tile "upper_dia"))
	  (setq bottomlessVal (get_tile "bottomless"))
	  (setq cutoutWidthVal (get_tile "cut_width"))
	  (setq cutoutBottomVal (get_tile "cut_bottom_dist"))
	  (setq cutoutTopVal (get_tile "cut_top_dist"))
	  (setq handleTypeVal (get_tile "handle_type"))
	  (setq handleSizeVal (get_tile "handle_size"))
	  (setq handleThicknessVal (get_tile "handle_width"))
  	(if (ValidateInputs)
	  (done_dialog 1))
	  
); end of defun : acceptCallback

;-----------------------------------------------------------------------------------------------

; SYSTEM VARIABLES TURNING ON AND OFF
; saves and turns off sysytem variables
(defun SaveAndPrepareEnvironment (/)
  (setq CupHolderEnv
    (list
      (cons "OSMODE" (getvar "OSMODE"))
      (cons "UCSFOLLOW" (getvar "UCSFOLLOW"))
      (cons "CMDECHO" (getvar "CMDECHO"))
      (cons "VIEWRES" (getvar "VIEWRES"))
      (cons "VISUALSTYLE" (getvar "VISUALSTYLE"))
    )
  )

  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)
  
  ;sets visual styles and isometric view
  (Set3DView)
  (command "_.UCS" "_World")

  (princ)
); end of defun : SaveAndPrepareEnvironment

;restores the old system variables
(defun RestoreEnvironment (/)

  (setq acadObj (vlax-get-acad-object))
  (vla-ZoomAll acadObj)
  (foreach item CupHolderEnv
    (setvar (car item) (cdr item))
  )
  (princ)
); end of defun : RestoreEnvironment

;--------------------------------------------------------------------------

; INPUT VALIDATION
; Validates the given inputs, returns true if everything is fine
(defun ValidateInputs ( / h l u cw cb ct hs)
  (setq h  (atof (get_tile "cup_height")))
  (setq l  (atof (get_tile "lower_dia")))
  (setq u  (atof (get_tile "upper_dia")))
  (setq hs (atof (get_tile "handle_size")))
  (setq cutW (atof (get_tile "cut_width")))
  (setq cutB (atof (get_tile "cut_bottom_dist")))
  (setq cutT (atof (get_tile "cut_top_dist")))

  (cond
    ; height between 5 and 30 cm
    ((or (< h 5) (> h 30))
     (alert "Cup height must be between 5cm and 30cm!")
     nil)

    ; upper and lower diameter between 3 and 15 cm
    ((or (< l 2) (> l 15))
     (alert "Lower diameter out of range. Must be between 3cm and 15cm!")
     nil)

    ((or (< u 2) (> u 15))
     (alert "Upper diameter out of range. Must be between 3cm and 15cm!")
     nil)

    ; upper diameter => lower diameter
    ((< u l)
     (alert "Upper diameter must be larger or equal than lower diameter!")
     nil)

    ; handle size btweeen 1 and 6
    ((or (< hs 1) (> hs 6))
     (alert "Handle size out of range. Must be between 1cm and 6cm!")
     nil)

    ; bottom distance <= height
    ((> cutB h)
     (alert "Cut distance from bottom must be less than the cup height.")
     nil)

    ; top distance <= 50% of height
    ((> cutT (* 0.5 h))
     (alert "Cutout top distance must be at most 50% of the cup height.")
     nil)

    ; cut width <= 50% of bottom diameter
    ((> cutW (* 0.5 l))
     (alert "Cutout width must be at most 50% of lower diameter.")
     nil)

    (T T)
  )
)


; get path for program with user input
(defun getProgramDir ( / path dir testPath)
  (setq dir nil)
  (setq path
  (getfiled "Select CupHolder.LSP from the program directory(CupHolder)" "CupHolder.LSP" "lsp" 0))
  ; verify that the user has chosen the right path
  (while path
	(setq dir (vl-filename-directory path)
	      testPath (strcat dir "\\DialogWindows\\cupHolderWindow.DCL")
		   );setq
    (if (findfile testPath) (setq path nil)
      ;user did not select the right path
      (progn
	(alert "You must select CupHolder.LSP from the program directory(CupHolder)!")
	(setq path
  		(getfiled "Select CupHolder.LSP from the program directory(CupHolder)" "CupHolder.LSP" "lsp" 0))
	);progn
      );if
	
    
    );while
  ;return
  dir

  ); end of defun : getProgramDir

;subroutine to add support path on the pos+1 place
(defun addSupportPath (dir pos / tmp c lst )

	(setq tmp ""
	      c -1
	)
	(if
		(not
		  (member (strcase dir)
			(setq lst 
			  (mapcar 'strcase 
				(parse (getenv "ACAD") ";")
			  )
			);setq
		  );member
		);not	
	(progn
		(if (not pos)
			(setq tmp (strcat (getenv "ACAD") ";" dir))
			(mapcar '(lambda (x)
			(setq tmp (if (= (setq c (1+ c)) pos)
					(strcat tmp ";" dir ";" x)
					(strcat tmp ";" x)
				  )
			)
			)
		(parse (getenv "ACAD") ";")
		)
	)
	(setenv "ACAD" tmp)
	)
	)
(princ)
); end of defun : addSupportPath



;parsing routine - split str on character delim and return a list 
(defun parse (str delim / lst pos)
	(setq pos (vl-string-search delim str))
	(while pos
		(setq lst (cons (substr str 1 pos) lst)
			str (substr str (+ pos 2))
			pos (vl-string-search delim str)
		)
	)
	(if (> (strlen str) 0)
		(setq lst (cons str lst))
	)
		(reverse lst)
); end of defun : parse


(defun Set3DView (/ acadObject acadDocument viewport)
  
  (setq acadObject   (vlax-get-acad-object))
  (setq acadDocument (vla-get-ActiveDocument acadObject))

  (setq viewport
        (vla-get-ActiveViewport acadDocument))

  ;set visual style to Realistic
  (vla-SendCommand (vla-get-ActiveDocument (vlax-get-acad-object)) "_.vscurrent _realistic ")


  ;set SE Isometric view
  (vla-put-Direction viewport (vlax-3d-point '(1 -1 1)))


  ;apply viewport changes
  (vla-put-ActiveViewport acadDocument viewport)
); end of defun : parse

;calculate bulge for entmake
(defun GetBulge (p1 p2 rad / dist h)
  (setq dist (distance p1 p2))
  (if (<= (* 2.0 rad) dist)
    (setq h rad)
    (setq h (- rad (sqrt (- (* rad rad) (* (/ dist 2.0) (/ dist 2.0))))))
  )
  (/ (* 2.0 h) dist)
); end GetBulge 