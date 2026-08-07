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

(defun UT_subtract (mainSS subSS / mainEname mainSolid subEname subSolid i result)

  ;validate input
  (if (or (null mainSS)
          (null subSS)
          (= (sslength mainSS) 0)
          (= (sslength subSS) 0))

    (progn
      (prompt "\nUT_subtract - Invalid input.")
      nil
    )

    (progn

      ;get main solid
      (setq mainEname
            (ssname mainSS 0))

      (setq mainSolid
            (vlax-ename->vla-object mainEname))


      ;check main object type
      (if (/= (vla-get-ObjectName mainSolid)
              "AcDb3dSolid")

        (progn
          (prompt "\nUT_subtract - Main object is not a 3D solid.")
          nil
        )

        (progn

          ;subtract every solid in subSS
          (setq i 0)
          (setq result T)

          (while (< i (sslength subSS))

            (setq subEname
                  (ssname subSS i))

            (setq subSolid
                  (vlax-ename->vla-object subEname))


            (if (= (vla-get-ObjectName subSolid)
                   "AcDb3dSolid")

              (progn

                (setq result
                      (vl-catch-all-apply
                        'vla-Boolean
                        (list
                          mainSolid
                          2 ; acSubtraction
                          subSolid)))

                (if (vl-catch-all-error-p result)
                  (progn
                    (prompt
                      (strcat
                        "\nUT_subtract failed: "
                        (vl-catch-all-error-message result)))
                    (setq result nil)
                  )
                )
              )
            )

            (setq i (1+ i))
          )


          ;update solid
          (if result
            (progn
              (vla-Update mainSolid)
              mainSolid
            )
            nil
          )
        )
      )
    )
  )
); end of defun : UT_subtract

(defun UT_union (selection / mainEname mainSolid unionEname unionSolid i result)

  ;validate input
  (if (or (null selection)
          (= (sslength selection) 0))

    (progn
      (prompt "\nUT_union - Invalid input.")
      nil
    )

    (progn

      ;get main solid
      (setq mainEname
            (ssname selection 0))

      (setq mainSolid
            (vlax-ename->vla-object mainEname))


      ;verify main object
      (if (/= (vla-get-ObjectName mainSolid)
              "AcDb3dSolid")

        (progn
          (prompt
            "\nUT_union - First selected object is not a 3D solid.")
          nil
        )

        (progn

          (setq i 1)

          ;union all remaining solids
          (while (< i (sslength selection))

            (setq unionEname
                  (ssname selection i))

            (setq unionSolid
                  (vlax-ename->vla-object unionEname))


            ;verify object type
            (if (/= (vla-get-ObjectName unionSolid)
                    "AcDb3dSolid")

              (prompt  "\nUT_union - Skipping object (not a 3D solid).")

              (progn

                (setq result
                      (vl-catch-all-apply
                        'vla-Boolean
                        (list
                          mainSolid
                          acUnion           ; acUnion
                          unionSolid)))

                ;check COM error
                (if (vl-catch-all-error-p result)

                  (prompt
                    (strcat
                      "\nUT_union failed: "
                      (vl-catch-all-error-message result)))
                )
              )
            )

            (setq i (1+ i))
          )

          ;update solid
          (vla-Update mainSolid)

          mainSolid
        )
      )
    )
  )
); end of defun : UT_union

(defun isValidPathType (entity / objectName)

  (and entity

       (setq objectName
             (cdr
               (assoc 0
                 (entget entity))))

       (member objectName
         '("LINE"
           "LWPOLYLINE"
           "POLYLINE"
           "SPLINE"
           "ARC"
           "CIRCLE"
           "ELLIPSE"))
  )
); end of defun : isValidPathType

;NB profileSelection must be a list of entity names
;NB about path and profile placement: 
;   - The profile and the path should not intersect.
;   - The ActiveX method (vla-AddExtrudedSolidAlongPath) is more restrictive
;     than AutoCAD's SWEEP command. Unlike the command, it does not
;     automatically align the profile to the path.
;   - The profile should already be positioned and oriented so that its plane
;     is perpendicular to the tangent of the path at the start point.
;   - If the profile is tangent to the path (for example, both lie in the
;     same XY plane), the operation may fail with:
;         "Profile and path are tangential"
;         "Automation Error. General modeling failure"
(defun UT_sweep (modelSpace profileSelection pathEntity / regionObject pathObject solidResult)

  ;validate input
  (if (or (null modelSpace)
          (null profileSelection)
          (null pathEntity))

    (progn
      (prompt "\nUT_sweep - Invalid input.")
      nil
    )

    (progn

      ;verify path
      (if (not (isValidPathType pathEntity))

        (progn
          (prompt "\nUT_sweep - Invalid path.")
          nil
        )

        (progn

          ;create region
          (setq regionObject (getRegion modelSpace profileSelection))

          ;verify region
          (if (null regionObject)

            (progn
              (prompt "\nUT_sweep - Region creation failed.")
              nil
            )

            (progn

              ;convert path to VLA object
              (setq pathObject
                    (vlax-ename->vla-object pathEntity))

              ;create swept solid
              (setq solidResult
                    (vl-catch-all-apply
                      'vla-AddExtrudedSolidAlongPath
                      (list
                        modelSpace
                        regionObject
                        pathObject)))

              ;check COM error
              (if (vl-catch-all-error-p solidResult)

                (progn
                  (prompt
                    (strcat
                      "\nUT_sweep failed: "
                      (vl-catch-all-error-message
                        solidResult)))
                  nil
                )

                (progn
	 		 ;delete region
	 		(vla-delete regionObject)
		  	(vla-delete  pathObject)
        		solidResult
		)
              )
            )
          )
        )
      )
    )
  )
); end of defun : UT_sweep

;rotationAngle is in degrees
(defun UT_rotate3D (selection axisPoint axisDirection rotationAngle
		    / index entity object result successCount)
  
  ;validate input
  (if (or (null selection)
          (= (sslength selection) 0)
          (null axisPoint)
          (null axisDirection)
          (null rotationAngle))

    (progn
      (prompt "\nUT_rotate3D - Invalid input.")
      nil
    )

    (progn

      ;convert degrees to radians
      (setq rotationAngle (* rotationAngle (/ pi 180.0)))

      (setq index 0)
      (setq successCount 0)


      ;rotate every selected object
      (while (< index (sslength selection))

        (setq entity
              (ssname selection index))

        (setq object
              (vlax-ename->vla-object entity))


        ;apply rotation safely
        (setq result
              (vl-catch-all-apply
                'vla-Rotate3D
                (list
                  object
                  (vlax-3d-point axisPoint)
                  (vlax-3d-point axisDirection)
                  rotationAngle)))


        ;check error
        (if (vl-catch-all-error-p result)

          (prompt
            (strcat
              "\nUT_rotate3D failed: "
              (vl-catch-all-error-message result)))

          (progn
            (vla-Update object)
            (setq successCount
                  (1+ successCount))
          )
        )


        (setq index
              (1+ index))
      )


      successCount
    )
  )
); end of defun : UT_UT_rotate3D