(load "./path-lib.scm")

(define canvas (image-new 500 500))
(image-show canvas)

(define stickperson (image-path-new canvas "stickperson"))

;head - bezier 1
(path-circle! stickperson (position 250 100) 75)
;body - bezier 2
(path-bezier-line! stickperson (position 250 175) (position 250 350))
;arms - bezier 3
(path-bezier-line! stickperson (position 200 350) (position 250 225))
(path-bezier-extend-auto-conic! stickperson 3 (position 400 100) -1)
;legs - bezier 4
(path-bezier-lines! stickperson (list (position 175 480) (position 250 350) (position 325 480)))
