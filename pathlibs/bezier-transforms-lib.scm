;(load "/home/fletcher1/pathlibs/path-lib.scm")

;;; Procedure:
;;;   path-bezier-hscale!
;;; Parameters:
;;;   path, a path
;;;   bezier, an int
;;;   x-scale, a real
;;; Purpose:
;;;   scales bezier horizontally by x-scale
;;; Produces:
;;;   nothing; called for side effects
;;; Preconditions:
;;;   bezier is the id of a bezier on path
;;; Postconditions:
;;;   the top-left of bezier remains in the same place, so scaling may make the bezier move off the canvas

(define _path-bezier-hscale!
  (lambda (path bezier x-scale)
    (gimp-vectors-stroke-scale (path-id path)
                               bezier
                               (exact->inexact x-scale)
                               1)))

(define path-bezier-hscale!
  (lambda (path bezier x-scale)
    (cond ((not (path? path))
           (error "path-bezier-hscale: path must be a valid path name or id, given " path))
          ((not (path-valid-bezier? path bezier))
           (error "path-bezier-hscale!: bezier is not a valid bezier id on the given path. Valid bezier ids for the given path are " (path-list-beziers path)))
          ((not (real? x-scale))
           (error "path-bezier-hscale!: x-scale  must be a real number, given " x-scale))
          (else
           (_path-bezier-hscale! path bezier x-scale)))))

;;; Procedure:
;;;   path-bezier-hscale
;;; Parameters:
;;;   path, a path
;;;   bezier, an int
;;;   x-scale, a real
;;; Purpose:
;;;   horizontally scales a copy of bezier by scale on a new path
;;; Produces:
;;;   scaled-bezier-path, a path
;;; Preconditions:
;;;   bezier is the id of a bezier on path
;;; Postconditions:
;;;   scaled-bezier-path is named [image-id]-[instance]-(get-path-name path)-bezier-[bezier-id], 
;;;     where instance is the number of times bezier has been previously duplicated in the image

(define _path-bezier-hscale
  (lambda (path bezier x-scale)
    (let ((duplicated-bezier (path-bezier-duplicate path bezier)))
      (_path-bezier-hscale! duplicated-bezier 
                            1 
                            x-scale)
      duplicated-bezier)))

(define path-bezier-hscale
  (lambda (path bezier x-scale)
    (cond ((not (path? path))
           (error "path-bezier-hscale: path must be a valid path name or id, given " path))
          ((not (path-valid-bezier? path bezier))
           (error "path-bezier-hscale: bezier is not a valid bezier id on the given path. Valid bezier ids for the given path are " (path-list-beziers path)))
          ((not (real? x-scale))
           (error "path-bezier-hscale: x-scale  must be a real number, given " x-scale))
          (else
           (_path-bezier-hscale path
                                bezier
                                x-scale)))))


;;; Procedure:
;;;   path-bezier-vscale!
;;; Parameters:
;;;   path, a path
;;;   bezier, an int
;;;   y-scale, a real
;;; Purpose:
;;;   scales bezier vertically by y-scale
;;; Produces:
;;;   nothing; called for side effects
;;; Preconditions:
;;;   bezier is the id of a bezier on path
;;; Postconditions:
;;;   the top-left of bezier remains in the same place, so scaling may make the bezier move off the canvas

(define _path-bezier-vscale!
  (lambda (path bezier y-scale)
    (gimp-vectors-stroke-scale (path-id path)
                               bezier
                               1
                               (exact->inexact y-scale))))

(define path-bezier-vscale!
  (lambda (path bezier y-scale)
    (cond ((not (path? path))
           (path? path))
          ((not (path-valid-bezier? path bezier))
           (error "path-bezier-vscale!: bezier is not a valid bezier id on the given path. Valid bezier ids for the given path are " (path-list-beziers path)))
          ((not (real? y-scale ))
           (error "path-bezier-vscale!: y-scale  must be a real number, given " y-scale ))
          (else
           (_path-bezier-vscale! path bezier y-scale)))))

;;; Procedure:
;;;   path-bezier-vscale
;;; Parameters:
;;;   path, a path
;;;   bezier, an int
;;;   y-scale, a real
;;; Purpose:
;;;   vertically scales a copy of bezier by scale on a new path
;;; Produces:
;;;   scaled-bezier-path, a path
;;; Preconditions:
;;;   bezier is the id of a bezier on path
;;; Postconditions:
;;;   scaled-bezier-path is named [image-id]-[instance]-(get-path-name path)-bezier-[bezier-id], 
;;;     where instance is the number of times bezier has been previously duplicated in the image

(define _path-bezier-vscale
  (lambda (path bezier y-scale)
    (let ((duplicated-bezier (path-bezier-duplicate path bezier)))
      (_path-bezier-vscale! duplicated-bezier 
                            1 
                            y-scale)
      duplicated-bezier)))

(define path-bezier-vscale
  (lambda (path bezier y-scale)
    (cond ((not (path? path))
           (error "path-bezier-vscale: path must be a valid path name or id, given " path))
          ((not (path-valid-bezier? path bezier))
           (error "path-bezier-vscale: bezier is not a valid bezier id on the given path. Valid bezier ids for the given path are " (path-list-beziers path)))
          ((not (real? y-scale ))
           (error "path-bezier-vscale: y-scale  must be a real number, given " y-scale ))
          (else
           (_path-bezier-vscale path
                                bezier
                                y-scale)))))

;;; Procedure:
;;;   path-bezier-prop-scale!
;;; Parameters:
;;;   path, a path
;;;   bezier, an int
;;;   scale, a real
;;; Purpose:
;;;   scales bezier proportionally by scale
;;; Produces:
;;;   Nothing; called for side effects
;;; Preconditions:
;;;  bezier is the id of a bezier on path
;;; Postconditions:
;;;   the top-left of bezier remains in the same place, so scaling may make the bezier move off the canvas

(define _path-bezier-prop-scale!
  (lambda (path bezier scale)
    (path-hide! path)
    (gimp-vectors-stroke-scale (path-id path)
                               bezier
                               (exact->inexact scale)
                               (exact->inexact scale))
    (path-show! path)))

(define path-bezier-prop-scale!
  (lambda (path bezier scale)
    (cond ((not (path? path))
           (error "path-bezier-prop-scale!: path must be a valid path name or id, given " path))
          ((not (path-valid-bezier? path bezier))
           (error "path-bezier-prop-scale!: bezier is not a valid bezier id on the given path. Valid bezier ids for the given path are " (path-list-beziers path)))
          ((not (real? scale))
           (error "path-bezier-prop-scale!: scale must be a real number, given " scale))
          (else
           (_path-bezier-prop-scale! path bezier scale)))))

;;; Procedure:
;;;   path-bezier-prop-scale
;;; Parameters:
;;;   path, a path
;;;   bezier, an int
;;;   scale, a real
;;; Purpose:
;;;   scales a copy of bezier proportionally by scale on a new path
;;; Produces:
;;;   scaled-bezier-path, a path
;;; Preconditions:
;;;  bezier is the id of a bezier on path
;;; Postconditions:
;;;   the top-left of bezier remains in the same place, so scaling may make the bezier move off the canvas
;;;   scaled-bezier-path is named [image-id]-[instance]-(get-path-name path)-bezier-[bezier-id], 
;;;     where instance is the number of times bezier has been previously duplicated in the image

(define _path-bezier-prop-scale
  (lambda (path bezier scale)
    (let ((duplicated-bezier (path-bezier-duplicate path bezier)))
      (_path-bezier-prop-scale! duplicated-bezier 
                                1 
                                scale)
      duplicated-bezier)))

(define path-bezier-prop-scale
  (lambda (path bezier scale)
    (cond ((not (path? path))
           (error "path-bezier-prop-scale: path must be a valid path name or id, given " path))
          ((not (path-valid-bezier? path bezier))
           (error "path-bezier-prop-scale: bezier is not a valid bezier id on the given path. Valid bezier ids for the given path are " (path-list-beziers path)))
          ((not (real? scale))
           (error "path-bezier-prop-scale: scale must be a real number, given " scale))
          (else
           (_path-bezier-prop-scale path 
                                    bezier 
                                    scale)))))

;;; Procedure:
;;;   path-bezier-hflip!
;;; Parameters:
;;;   path, a path
;;;   bezier, an int
;;; Purpose:
;;;   flips bezier on its horizontal axis
;;; Produces:
;;;   nothing; called for its side effects
;;; Preconditions:
;;;   bezier is the id of a bezier on path
;;; Postconditions:
;;;   the horizontal axis is calculated based on anchors, so if bezier has extreme handles, it may be off

(define _path-bezier-hflip!
  (lambda (path bezier)
    (let ((horizontal-axis (position-col (path-bezier-get-center path bezier))))
      (path-hide! path)
      (gimp-vectors-stroke-flip (path-id path)
                                bezier
                                0
                                horizontal-axis)
      (path-show! path))))

(define path-bezier-hflip!
  (lambda (path bezier)
    (cond ((not (path? path))
           (error "path-bezier-hflip!: path must be a valid path name or id, given " path))
          ((not (path-valid-bezier? path bezier))
           (error "path-bezier-hflip!: bezier is not a valid bezier id on the given path. Valid beziers ids for the given path are " (path-list-beziers path)))
          (else 
           (_path-bezier-hflip! path bezier)))))

;;; Procedure:
;;;   path-bezier-hflip
;;; Parameters:
;;;   path, a path
;;;   bezier, an int
;;; Purpose:
;;;   flips a copy of bezier on its horizontal axis on a new path
;;; Produces:
;;;   flipped-bezier-path, a path
;;; Preconditions:
;;;  bezier is the id of a bezier on path
;;; Postconditions:
;;;   the horizontal axis is calculated based on anchors, so if bezier has extreme handles, 
;;;     the procedure may not work as expected
;;;   flipped-bezier-path is named [image-id]-[instance]-(get-path-name path)-bezier-[bezier-id], 
;;;     where instance is the number of times bezier has been previously duplicated in the image

(define _path-bezier-hflip
  (lambda (path bezier)
    (let ((duplicated-bezier (path-bezier-duplicate path bezier)))
      (_path-bezier-hflip! duplicated-bezier 
                           1)
      duplicated-bezier)))

(define path-bezier-hflip
  (lambda (path bezier)
    (cond ((not (path? path))
           (error "path-bezier-hflip: path must be a valid path name or id, given " path))
          ((not (path-valid-bezier? path bezier))
           (error "path-bezier-hflip: bezier is not a valid bezier id on the given path. Valid beziers ids for the given path are " (path-list-beziers path)))
          (else 
           (_path-bezier-hflip path
                               bezier)))))

;;; Procedure:
;;;   path-bezier-vflip!
;;; Parameters:
;;;   path, a path
;;;   bezier, an int
;;; Purpose:
;;;   flips bezier on its vertical axis
;;; Produces:
;;;   nothing; called for its side effects
;;; Preconditions:
;;;   bezier is the id of a bezier on path
;;; Postconditions:
;;;   the vertical axis is calculated based on anchors, so if bezier has extreme handles, it may be off

(define _path-bezier-vflip!
  (lambda (path bezier)
    (let ((vertical-axis (position-row (path-bezier-get-center path bezier))))
      (path-hide! path)
      (gimp-vectors-stroke-flip (path-id path)
                                bezier 
                                1
                                vertical-axis))
    (path-show! path)))

(define path-bezier-vflip!
  (lambda (path bezier)
    (cond ((not (path? path))
           (error "path-bezier-vflip!: path must be a valid path name or id, given " path))
          ((not (path-valid-bezier? path bezier))
           (error "path-bezier-vflip!: bezier is not a valid bezier id on the given path. Valid beziers ids for the given path are " (path-list-beziers path)))
          (else 
           (_path-bezier-vflip! path bezier)))))

;;; Procedure:
;;;   path-bezier-vflip
;;; Parameters:
;;;   path, a path
;;;   bezier, an int
;;; Purpose:
;;;   flips a copy of bezier on its vertical axis on a new path
;;; Produces:
;;;   flipped-bezier-path, a path
;;; Preconditions:
;;;  bezier is the id of a bezier on path
;;; Postconditions:
;;;   the vertical axis is calculated based on anchors, so if bezier has extreme handles, 
;;;     the procedure may not work as expected
;;;   flipped-bezier-path is named [image-id]-[instance]-(get-path-name path)-bezier-[bezier-id], 
;;;     where instance is the number of times bezier has been previously duplicated in the image

(define _path-bezier-vflip
  (lambda (path bezier)
    (let ((duplicated-bezier (path-bezier-duplicate path bezier)))
      (_path-bezier-vflip! duplicated-bezier 
                             1)
      duplicated-bezier)))

(define path-bezier-vflip
  (lambda (path bezier)
    (cond ((not (path? path))
           (error "path-bezier-vflip: path must be a valid path name or id, given " path))
          ((not (path-valid-bezier? path bezier))
           (error "path-bezier-vflip: bezier is not a valid bezier id on the given path. Valid beziers ids for the given path are " (path-list-beziers path)))
          (else 
           (_path-bezier-vflip path
                               bezier)))))

;;; Procedure:
;;;   path-bezier-line-flip!
;;; Parameters:
;;;   path, a path
;;;   bezier, an int
;;;   line-start, a position
;;;   line-end, a position
;;; Purpose:
;;;   flips bezier about the line that extends from line-start to line-end
;;; Produces:
;;;   nothing; called for side effects
;;; Preconditions:
;;;   bezier must be the id of a bezier on path
;;; Postconditions:
;;;   

(define _path-bezier-line-flip!
  (lambda (path bezier line-start line-end)
    (path-hide! path)
    (gimp-vectors-stroke-flip-free (path-id path)
                                   bezier
                                   (position-col line-start)
                                   (position-row line-start)
                                   (position-col line-end)
                                   (position-row line-end))
    (path-show! path)))

(define path-bezier-line-flip!
  (lambda (path bezier line-start line-end)
    (cond ((not (path? path))
           (error "path-bezier-line-flip!: path must be a valid path name or id, given " path))
          ((not (path-valid-bezier? path bezier))
           (error "path-bezier-line-flip!: bezier is not a valid bezier id on the given path. Valid beziers ids for the given path are " (path-list-beziers path)))
          ((not (position? line-start))
           (error "path-bezier-line-flip!: line-start must be a position, given " line-start))
          ((not (position? line-end))
           (error "path-bezier-line-flip!: line-end must be a position, given " line-end))
          (else
           (_path-bezier-line-flip! path bezier line-start line-end)))))

;;; Procedure:
;;;   path-bezier-line-flip
;;; Parameters:
;;;   path, a path
;;;   bezier, an int
;;;   line-start, a position
;;;   line-end, a position
;;; Purpose:
;;;   flips a copy of bezier about the line that extends from line-start to line-end on a new path
;;; Produces:
;;;   flipped-bezier-path, a path
;;; Preconditions:
;;;  bezier is the id of a bezier on path
;;; Postconditions:
;;;   flipped-bezier-path is named [image-id]-[instance]-(get-path-name path)-bezier-[bezier-id], 
;;;     where instance is the number of times bezier has been previously duplicated in the image

(define _path-bezier-line-flip
  (lambda (path bezier line-start line-end)
    (let ((duplicated-bezier (path-bezier-duplicate path bezier)))
      (_path-bezier-line-flip! duplicated-bezier 
                               1 
                               line-start
                               line-end)
      duplicated-bezier)))

(define path-bezier-line-flip
  (lambda (path bezier line-start line-end)
    (cond ((not (path? path))
           (error "path-bezier-line-flip: path must be a valid path name or id, given " path))
          ((not (path-valid-bezier? path bezier))
           (error "path-bezier-line-flip: bezier is not a valid bezier id on the given path. Valid beziers ids for the given path are " (path-list-beziers path)))
          ((not (position? line-start))
           (error "path-bezier-line-flip: line-start must be a position, given " line-start))
          ((not (position? line-end))
           (error "path-bezier-line-flip: line-end must be a position, given " line-end))
          (else
           (_path-bezier-line-flip path 
                                   bezier 
                                   line-start 
                                   line-end)))))

;;; Procedure:
;;;   path-bezier-hoffset!
;;; Parameters:
;;;   path, a path
;;;   bezier, an int
;;;   offset, a real
;;; Purpose:
;;;   moves the given bezier horizontally by offset
;;; Produces:
;;;   nothing; called for side effects
;;; Preconditions:
;;;  bezier is the id of a bezier on path
;;; Postconditions:
;;;   offset is added to the x coordinates of all control points on bezier 

(define _path-bezier-hoffset!
  (lambda (path bezier offset)
    (path-hide! path)
    (gimp-vectors-stroke-translate (path-id path)
                                   bezier
                                   offset
                                   0) ;;no y offset
    (path-show! path)))

(define path-bezier-hoffset!
  (lambda (path bezier offset)
    (cond ((not (path? path))
           (error "path-bezier-hoffset!: path must be a valid path name or id, given " path))
          ((not (path-valid-bezier? path bezier))
           (error "path-bezier-hoffset!: bezier is not a valid bezier id on the given path. Valid beziers ids for the given path are " (path-list-beziers path)))
          ((not (real? offset))
           (error "path-bezier-hoffset!: offset must be a real number, given " offset))
          (else
           (_path-bezier-hoffset! path bezier offset)))))

;;; Procedure:
;;;   path-bezier-hoffset
;;; Parameters:
;;;   path, a path
;;;   bezier, an int
;;;   offset, a real
;;; Purpose:
;;;   moves a copy of the given bezier horizontally by offset on a new path
;;; Produces:
;;;   translated-bezier-path, a path
;;; Preconditions:
;;;  bezier is the id of a bezier on path
;;; Postconditions:
;;;   offset is added to the x coordinates of all control points on the copy of bezier 
;;;   translated-bezier-path is named [image-id]-[instance]-(get-path-name path)-bezier-[bezier-id], 
;;;     where instance is the number of times bezier has been previously duplicated in the image

(define _path-bezier-hoffset
  (lambda (path bezier offset)
    (let ((duplicated-bezier (path-bezier-duplicate path bezier)))
      (_path-bezier-hoffset! duplicated-bezier 
                             1 
                             offset)
      duplicated-bezier)))

(define path-bezier-hoffset
  (lambda (path bezier offset)
    (cond ((not (path? path))
           (error "path-bezier-hoffset: path must be a valid path name or id, given " path))
          ((not (path-valid-bezier? path bezier))
           (error "path-bezier-hoffset: bezier is not a valid bezier id on the given path. Valid beziers ids for the given path are " (path-list-beziers path)))
          ((not (real? offset))
           (error "path-bezier-hoffset: offset must be a real number, given " offset))
          (else
           (_path-bezier-hoffset  path 
                                  bezier 
                                  offset)))))

;;; Procedure:
;;;   path-bezier-voffset!
;;; Parameters:
;;;   path, a path
;;;   bezier, an int
;;;   offset, a real
;;; Purpose:
;;;   moves the given bezier vertically by offset
;;; Produces:
;;;   nothing; called for side effects
;;; Preconditions:
;;;   bezier is the id of a bezier on path
;;; Postconditions:
;;;   offset is added to the y coordinates of all control points on bezier 

(define _path-bezier-voffset!
  (lambda (path bezier offset)
    (path-hide! path)
    (gimp-vectors-stroke-translate (path-id path)
                                   bezier
                                   0 ;;no x offset
                                   offset) 
    (path-show! path)))

(define path-bezier-voffset!
  (lambda (path bezier offset)
    (cond ((not (path? path))
           (error "path-bezier-voffset!: path must be a valid path name or id, given " path))
          ((not (path-valid-bezier? path bezier))
           (error "path-bezier-voffset!: bezier is not a valid bezier id on the given path. Valid beziers ids for the given path are " (path-list-beziers path)))
          ((not (real? offset))
           (error "path-bezier-voffset!: offset must be a real number, given " offset))
          (else
           (_path-bezier-voffset! path bezier offset)))))

;;; Procedure:
;;;   path-bezier-voffset
;;; Parameters:
;;;   path, a path
;;;   bezier, an int
;;;   offset, a real
;;; Purpose:
;;;   moves a copy of the given bezier vertically by offset on a new path
;;; Produces:
;;;   translated-bezier-path, a path
;;; Preconditions:
;;;  bezier is the id of a bezier on path
;;; Postconditions:
;;;   offset is added to the y coordinates of all control points on the copy of bezier 
;;;   translated-bezier-path is named [image-id]-[instance]-(get-path-name path)-bezier-[bezier-id], 
;;;     where instance is the number of times bezier has been previously duplicated in the image

(define _path-bezier-voffset
  (lambda (path bezier offset)
    (let ((duplicated-bezier (path-bezier-duplicate path bezier)))
      (_path-bezier-voffset! duplicated-bezier 
                             1 
                             offset)
      duplicated-bezier)))

(define path-bezier-voffset
  (lambda (path bezier offset)
    (cond ((not (path? path))
           (error "path-bezier-voffset: path must be a valid path name or id, given " path))
          ((not (path-valid-bezier? path bezier))
           (error "path-bezier-voffset: bezier is not a valid bezier id on the given path. Valid beziers ids for the given path are " (path-list-beziers path)))
          ((not (real? offset))
           (error "path-bezier-voffset: offset must be a real number, given " offset))
          (else
           (_path-bezier-voffset path 
                                  bezier 
                                  offset)))))

;;; Procedure:
;;;   path-bezier-rotate-around!
;;; Parameters:
;;;   path, a path
;;;   bezier, an int
;;;   rotate-center, a position
;;;   angle, a real
;;; Purpose:
;;;   rotates the given bezier around rotate-center by angle degrees 
;;; Produces:
;;;   Nothing; called for side effects
;;; Preconditions:
;;;   bezier is the id of a bezier on path
;;;   rotate is given in degrees
;;; Postconditions:
;;;   

(define _path-bezier-rotate-around!
  (lambda (path bezier rotate-center angle)
    (path-hide! path)
    (gimp-vectors-stroke-rotate (path-id path)
                                bezier
                                (position-col rotate-center)
                                (position-row rotate-center)
                                angle)
    (path-show! path)))

(define path-bezier-rotate-around!
  (lambda (path bezier rotate-center angle)
    (cond ((not (path? path))
           (error "path-bezier-rotate-around!: path must be a valid path name or id, given " path))
          ((not (path-valid-bezier? path bezier))
           (error "path-bezier-rotate-around!: bezier is not a valid bezier id on the given path. Valid beziers ids for the given path are " (path-list-beziers path)))
          ((not (position? rotate-center))
           (error "path-bezier-rotate-around!: rotate-center must be a  position, given " rotate-center))
          ((not (real? angle))
           (error "path-bezier-rotate-around!: angle must be a real number, given " angle))
          (else
           (_path-bezier-rotate-around! path bezier rotate-center angle)))))

;;; Procedure:
;;;   path-bezier-rotate-around
;;; Parameters:
;;;   path, a path
;;;   bezier, an int
;;;   angle, a real
;;; Purpose:
;;;   rotates a copy of bezier around rotate-center by angle degrees on a new path
;;; Produces:
;;;   rotated-bezier-path, a path
;;; Preconditions:
;;;   bezier is the id of a bezier on path
;;;   rotate is given in degrees
;;; Postconditions:
;;;   rotated-bezier-path is named [image-id]-[instance]-(get-path-name path)-bezier-[bezier-id], 
;;;     where instance is the number of times bezier has been previously duplicated in the image

(define _path-bezier-rotate-around
  (lambda (path bezier rotate-center angle)
    (let ((duplicated-bezier (path-bezier-duplicate path bezier)))
      (_path-bezier-rotate-around! duplicated-bezier 
                                   1
                                   rotate-center
                                   angle)
      duplicated-bezier)))

(define path-bezier-rotate-around
  (lambda (path bezier rotate-center angle)
    (cond ((not (path? path))
           (error "path-bezier-rotate-around: path must be a valid path name or id, given " path))
          ((not (path-valid-bezier? path bezier))
           (error "path-bezier-rotate-around: bezier is not a valid bezier id on the given path. Valid beziers ids for the given path are " (path-list-beziers path)))
          ((not (position? rotate-center))
           (error "path-bezier-rotate-around: rotate-center must be a  position, given " rotate-center))
          ((not (real? angle))
           (error "path-bezier-rotate-around: angle must be a real number, given " angle))
          (else
           (_path-bezier-rotate-around path 
                                       bezier
                                       rotate-center
                                       angle)))))

;;; Procedure:
;;;   path-bezier-rotate!
;;; Parameters:
;;;   path, a path
;;;   bezier, an int
;;;   angle, a real
;;; Purpose:
;;;   rotates the given bezier around the center of its anchor points by angle degrees
;;; Produces:
;;;   Nothing; called for side effects
;;; Preconditions:
;;;   bezier is the id of a bezier on path
;;;   angle is given in degrees
;;; Postconditions:
;;;   since the center is determined by the anchor points, this procedure may not work as expected 
;;;     for beziers with extreme handles

(define _path-bezier-rotate!
  (lambda (path bezier angle)
    (path-hide! path)
    (path-bezier-rotate-around! path
                                bezier
                                (path-bezier-get-center path bezier)
                                angle)
    (path-show! path)))

(define path-bezier-rotate!
  (lambda (path bezier angle)
    (cond ((not (path? path))
           (error "path-bezier-rotate-around!: path must be a valid path name or id, given " path))
          ((not (path-valid-bezier? path bezier))
           (error "path-bezier-rotate-around!: bezier is not a valid bezier id on the given path. Valid beziers ids for the given path are " (path-list-beziers path)))
          ((not (real? angle))
           (error "path-bezier-rotate-around!: angle must be a real number, given " angle))
          (else
           (_path-bezier-rotate! path bezier angle)))))

;;; Procedure:
;;;   path-bezier-rotate
;;; Parameters:
;;;   path, a path
;;;   bezier, a bezier
;;;   angle, a real
;;; Purpose:
;;;   rotates a copy of bezier around the center of its anchor points by angle degrees on a new path
;;; Produces:
;;;   rotated-bezier-path, a path
;;; Preconditions:
;;;   bezier exits on path
;;; Postconditions:
;;;   Since the center is determined by the anchor points, this procedure may not work as expected 
;;;     for beziers with extreme handles
;;;   rotated-bezier-path is named [image-id]-[instance]-(get-path-name path)-bezier-[bezier-id], 
;;;     where instance is the number of times bezier has been previously duplicated in the image

(define _path-bezier-rotate
  (lambda (path bezier angle)
    (let ((duplicated-bezier (path-bezier-duplicate path bezier)))
      (_path-bezier-rotate! duplicated-bezier 
                            1 
                            angle)
      duplicated-bezier)))

(define path-bezier-rotate
  (lambda (path bezier angle)
    (cond ((not (path? path))
           (error "path-bezier-rotate: path must be a valid path name or id, given " path))
          ((not (path-valid-bezier? path bezier))
           (error "path-bezier-rotate: bezier is not a valid bezier id on the given path. Valid beziers ids for the given path are " (path-list-beziers path)))
          ((not (real? angle))
           (error "path-bezier-rotate: angle must be a real number, given " angle))
          (else
           (_path-bezier-rotate path bezier angle)))))
