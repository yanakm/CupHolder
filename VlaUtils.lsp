(defun getModelSpace (/ acadObject acadDocument mSpace)
  (setq acadObject   (vlax-get-acad-object))
  (setq acadDocument (vla-get-ActiveDocument acadObject))
  (setq mSpace       (vla-get-ModelSpace acadDocument))
  ;return
  mSpace
  ); end of defun : getModelSpace

(defun isValidRegionType (entity / validCurveTypes)
   ; valid objects for creating regions
  (setq validCurveTypes
        '(
          "LINE"
          "ARC"
          "CIRCLE"
          "LWPOLYLINE"
          "POLYLINE"
          "ELLIPSE"
         ))
  (and entity (entget entity)
  	(member
   	 (cdr (assoc 0 (entget entity)))
    		validCurveTypes)
       )
  
  ); end of defun : isValidRegionType

;NB entityList must be a list of entity names
(defun createRegions (modelSpace entityList /  curveObjects curveArray regionResult regionArray index)

  ;validate input
  (if (or (null modelSpace)
          (null entityList))
    (progn
      (prompt "\nUT:CreateRegion - Invalid input.")
      nil
    )

    (progn

      ;convert valid entities to VLA objects
      (setq curveObjects nil)

      (foreach entity entityList

        (if (isValidRegionType entity)
          (setq curveObjects
                (cons
                  (vlax-ename->vla-object entity)
                  curveObjects))
        )
      )

      ;check if any valid curves exist
      (if (null curveObjects)

        (progn
          (prompt
            "\nUT:CreateRegion - No valid curves found.")
          nil
        )

        (progn

          ;cCreate SafeArray
          (setq curveArray
                (vlax-make-safearray
                  vlax-vbObject
                  (cons 0
                        (1- (length curveObjects)))))

          ;fill SafeArray
          (setq index 0)

          (foreach curve curveObjects

            (vlax-safearray-put-element
              curveArray
              index
              curve)

            (setq index (1+ index))
          )


          ;call AddRegion safely
          (setq regionResult
                (vl-catch-all-apply
                  'vla-AddRegion
                  (list
                    modelSpace
                    (vlax-make-variant curveArray))))


          ;check COM error
          (if (vl-catch-all-error-p regionResult)

            (progn
              (prompt
                (strcat
                  "\nUT:CreateRegion failed: "
                  (vl-catch-all-error-message
                    regionResult)))
              nil
            )

            (progn
	      ;delete entites
	      (foreach entity entityList
	      (entdel entity)
		)

              ;convert returned SafeArray to list
              (setq regionArray
                    (vlax-variant-value
                      regionResult))
		;returns a list of regions
              (vlax-safearray->list
                regionArray)
            )
          )
        )
      )
    )
  )
); end of defun : createRegions

;createRegon returns a list of regions
;this function creates regions and return the first
(defun getRegion  (modelSpace entityList / )
  (car (createRegions modelSpace entityList))
  ); end of defun : getRegion


;NB entityList must be a list of entity names
;revolveAngle is in degrees
(defun UT_revolve (modelSpace entityList axisPoint axisDirection revolveAngle / solidResult regionObject)

  ;validate input
  (if (or (null modelSpace)
          (null entityList)
          (null axisPoint)
          (null axisDirection)
          (null revolveAngle))

    (progn
      (prompt "\nUT_revolve - Invalid input.")
      nil
    )

    (progn
       (setq regionObject (getRegion modelSpace entityList))

      ;create revolved solid
      (setq solidResult
            (vl-catch-all-apply
              'vla-AddRevolvedSolid
              (list
                modelSpace
                regionObject
                (vlax-3d-point axisPoint)
                (vlax-3d-point axisDirection)
                (* revolveAngle (/ pi 180.0)))))

      ;check ActiveX error
      (if (vl-catch-all-error-p solidResult)

        (progn
          (prompt
            (strcat
              "\nUT_revolve failed: "
              (vl-catch-all-error-message solidResult)))
          nil
        )
	(progn
	  ;delete region
	  (vla-delete regionObject)
        solidResult
	)
      )
    )
  )
); end of defun : UT_revolve

(defun UT_extrude (modelSpace entityList extrusionHeight / regions regionObject solidResult)

  ;validate input
  (if (or (null modelSpace)
          (null entityList)
          (null extrusionHeight))

    (progn
      (prompt "\nUT_extrude - Invalid input.")
      nil
    )

    (progn

      ;create region 
      (setq regionObject
            (getRegion modelSpace entityList))


      ;check region creation
      (if (null regionObject)

        (progn
          (prompt "\nUT_extrude - Region creation failed.")
          nil
        )

        (progn

          ;create extruded solid
          ;taper angle = 0.0
          (setq solidResult
                (vl-catch-all-apply
                  'vla-AddExtrudedSolid
                  (list
                    modelSpace
                    regionObject
                    extrusionHeight
                    0.0)))


          ;check ActiveX error
          (if (vl-catch-all-error-p solidResult)

            (progn
              (prompt
                (strcat
                  "\nUT_extrude failed: "
                  (vl-catch-all-error-message solidResult)))
              nil
            )
	    (progn
	       ;delete region
	    (vla-delete regionObject)

            solidResult
	    )
          )
        )
      )
    )
  )
); end of defun : UT_extrude