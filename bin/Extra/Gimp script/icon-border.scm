; Add a semitransparent border to icons
(define (script-fu-iconborder inImg inLayer InBorderColor InBorderTrans) 

  (if (< 0 (car (gimp-image-base-type inImg))) (gimp-image-convert-rgb inImg))
  (gimp-layer-add-alpha inLayer)
  
  ; Removing alpha pixels (previous border)
  (plug-in-threshold-alpha 1 inImg inLayer 254)

  ; Make sure there is size for border
  (gimp-layer-resize inLayer 
    (+ 2 (car (gimp-drawable-width inLayer)))
    (+ 2 (car (gimp-drawable-height inLayer)))
    1
    1
  )
  
  (let*
  (
    (CurrX 0)
    (CurrY 0)
    (ImgX (car (gimp-drawable-width inLayer)))
    (ImgY (car (gimp-drawable-height inLayer)))
    (CurrColor (cons-array 4 'byte))
    (BorColor (cons-array 4 'byte))
    (Alpha 0)
    (AddBorder FALSE)
  )    
    
    (vector-set! BorColor 0 (car InBorderColor))
    (vector-set! BorColor 1 (cadr InBorderColor 1))
    (vector-set! BorColor 2 (caddr InBorderColor 2))
    (vector-set! BorColor 3 InBorderTrans)
    
    (while (< CurrX ImgX)
      (set! CurrY 0)
      (while (< CurrY ImgY)       
        (set! CurrColor (cadr (gimp-drawable-get-pixel inLayer CurrX CurrY)))
        (set! Alpha (vector-ref CurrColor 3))
        (set! AddBorder FALSE)
        
        (if (= Alpha 0)
        (begin
          ; Left
          (if (= AddBorder FALSE)
          (if (> CurrX 0)
          (begin
            (set! CurrColor (cadr (gimp-drawable-get-pixel inLayer (- CurrX 1) CurrY)))
            (set! Alpha (vector-ref CurrColor 3))
            (if (> Alpha 254) (set! AddBorder TRUE))
          )
          )
          )
        
          ; Right
          (if (= AddBorder FALSE)
          (if (< CurrX (- ImgX 1))
          (begin
            (set! CurrColor (cadr (gimp-drawable-get-pixel inLayer (+ CurrX 1) CurrY)))
            (set! Alpha (vector-ref CurrColor 3))
            (if (> Alpha 254) (set! AddBorder TRUE))
          )
          )
          )
          
          ; Up
          (if (= AddBorder FALSE)
          (if (> CurrY 0)
          (begin
            (set! CurrColor (cadr (gimp-drawable-get-pixel inLayer CurrX (- CurrY 1))))
            (set! Alpha (vector-ref CurrColor 3))
            (if (> Alpha 254) (set! AddBorder TRUE))
          )
          )
          )

          ; Down
          (if (= AddBorder FALSE)
          (if (< CurrY (- ImgY 1))
          (begin
            (set! CurrColor (cadr (gimp-drawable-get-pixel inLayer CurrX (+ CurrY 1))))
            (set! Alpha (vector-ref CurrColor 3))
            (if (> Alpha 254) (set! AddBorder TRUE))
          )
          )
          )
          
          (if (= AddBorder TRUE) (gimp-drawable-set-pixel inLayer CurrX CurrY 4 BorColor))        
        )
        )
         
        (set! CurrY (+ CurrY 1))
      )
      (set! CurrX (+ CurrX 1))
    )
  )
  
  (plug-in-autocrop-layer 1 inImg inLayer)
  
  (gimp-image-resize-to-layers inImg) 
  
  (gimp-displays-flush)
  (gimp-drawable-update inLayer 0 0 (car (gimp-image-width inImg)) (car (gimp-image-height inImg)))
)

(script-fu-register "script-fu-iconborder"
  _"Add _Icon Border..."
  _"Add a border to images"
  "Chixpy"
  "GPL3 by Chixpy"
  "2020/06/07"
  "RGB* INDEXED* GRAY*"
  SF-IMAGE       "Input image" 0
  SF-DRAWABLE    "Input drawable" 0
  SF-COLOR      _"Border color" '(128 128 128)
  SF-ADJUSTMENT _"Border opacity" '(128 0 254 1 16 0 0)
)

(script-fu-menu-register "script-fu-iconborder"
                         "<Image>/Filters")