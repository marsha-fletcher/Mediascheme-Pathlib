(load "/home/fletcher1/Wrappers/Illustrative Examples Code/patterns.scm")
;; patterns.scm can be found at 
;;     http://foswiki.cs.grinnell.edu/foswiki/pub/Glimmer/WrappingProcedures/patterns.scm

(load "/home/fletcher1/Wrappers/Illustrative Examples Code/text.scm")
;;loaded for the (font? val) procedure
;;text.scm can be found at
;;     http://foswiki.cs.grinnell.edu/foswiki/pub/Glimmer/WrappingProcedures/text.scm

(load "/home/fletcher1/Wrappers/Illustrative Examples Code/opacity.scm")
;; opacity.scm can be found at
;;     http://foswiki.cs.grinnell.edu/foswiki/pub/Glimmer/WrappingProcedures/opacity.scm

(load "/home/fletcher1/Wrappers/paint-modes.scm")
;; I wrote paint-modes.scm so it isn't online anywhere at the moment.

(load "./path-info-lib.scm")

(load "./bezier-transforms-lib.scm")

(load "./merge-paths.scm")

;;; pathlib.scm

;;;defined for use in path-fill-unabridged!
(define FOREGROUND-FILL 0)
(define BACKGROUND-FILL 1)
(define PATTERN-FILL 2)


;;;   bezier-position-new
;;; Parameters:
;;;   anchor-x, a real
;;;   anchor-y, a real
;;;   handle1-x, a real
;;;   handle1-y, a real
;;;   handle2-x, a real
;;;   handle2-y, a real
;;; Purpose:
;;;   Takes coordinates for a position in a subpath.
;;; Produces:
;;;   new-bezier-position, a list of reals
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   (anchor-x, anchor-y) is the anchor point
;;;   (handle1-x, handle1y) is the handle for the curve from the previous anchor point
;;;   (handle2-x, handle2-y) is the handle for the curve to the next anchor point

(define _bezier-position-new
  (lambda (anchor-x anchor-y handle1-x handle1-y handle2-x handle2-y)
    (list anchor-x anchor-y handle1-x handle1-y handle2-x handle2-y)))

(define bezier-position-new
  (guard-proc 'bezier-position-new
              _bezier-position-new
              (list 'real 'real 'real 'real 'real 'real)
              (list real? real? real? real? real? real?)))


;;; Procedure:
;;;   image-path-new
;;; Parameters:
;;;   image, an image
;;;   name, a string
;;;   position, an integer
;;; Purpose:
;;;   Creates a new path in the paths tab
;;; Produces:
;;;   new-path, a path named "name"
;;; Preconditions:
;;;   none
;;; Postconditions
;;;   The new path is at the top of the paths list (position 0)
;;;   If a path called "name" already exists, then new-path is renamed with the pattern 
;;;    [image-id]-[instance]-name, where instance is the number of other paths in image that 
;;;    are named a variation of name

(define _image-path-new
  (lambda (image name)
    (let ((change-name? (if (path-name? name)
                            #t
                            #f))
          (path (car (gimp-vectors-new image name))))
      (when change-name?
        (auto-rename-path path image)
        (display "image-path-new: The given path name was already in use, so it has been altered.")
        (newline))
      (gimp-image-add-vectors image 
                              path
                              0) ;puts the path at the top position
      path
      )))

(define image-path-new
  (guard-proc 'image-path-new
              _image-path-new
              (list 'image 'string)
              (list image? string?)))

;;; Procedure:
;;;   path-remove!
;;; Parameters:
;;;   path, a path
;;; Purpose:
;;;   deletes the path from the image it currently belongs to
;;; Produces:
;;;   nothing; called for side effects
;;; Preconditions:
;;;   path exists on an image
;;; Postconditions:
;;;   path no longer exists on an image

(define _path-remove!
  (lambda (path)
    (let ((image (path-get-image path)))
      (gimp-image-remove-vectors image
                                  (path-id path)))))

(define path-remove!
  (guard-proc 'path-remove!
              _path-remove!
              (list 'path)
              (list path?)))

;;; Procedure:
;;;   path-rename!
;;; Parameters:
;;;   path, a path
;;;   new-name, a string
;;; Purpose:
;;;   sets the name of path to new-name
;;; Produces:
;;;   nothing; called for side effects
;;; Preconditions:
;;;   
;;; Postconditions:
;;;   new-name is now associated with path's id

(define _path-rename!
  (lambda (path new-name)
    (gimp-vectors-set-name (path-id path)
                           new-name)))

(define path-rename!
  (guard-proc 'path-rename!
              _path-rename!
              (list 'path 'path-name-free?)
              (list path? path-name-free?)))

;;; Procedure:
;;;   path-visible?
;;; Parameters:
;;;   path, a path
;;; Purpose:
;;;   determines if path is set to visible
;;; Produces:
;;;   is-visible?, a boolean
;;; Preconditions:
;;;   no additional
;;; Postconditions:
;;;   if path is set to visible, is-visible? is true (#t)
;;;    else, is-visible? is false (#f)

(define path-visible?
  (lambda (path)
    (cond ((not (path? path))
           (error "path-visible?: path must be a valid path name or id, given " path))
          ((= 1 (car (gimp-vectors-get-visible (path-id path))))
           #t)
          (else
           #f))))

;;; Procedure:
;;;   paths-any-visible?
;;; Parameters:
;;;   paths, a list of paths
;;; Purpose:
;;;   determine if any path in paths is visible
;;; Produces:
;;;   has-visible? a boolean
;;; Preconditions:
;;;   no additional
;;; Postconditions:
;;;   if any path in paths is set to visible, has-visible? is true (#t)
;;;   else, has-visible? is false (#f)

(define paths-any-visible?
  (lambda (paths)
    (and (not (null? paths))
         (or (path-visible? (car paths))
             (paths-any-visible? (cdr paths))))))

;;; Procedure:
;;;   path-show!
;;; Parameters:
;;;   path, a path
;;; Purpose:
;;;   Makes the specified path visible
;;; Produces:
;;;   nothing - called for the postconditions
;;; Preconditions:
;;;   none

(define _path-show!
  (lambda (path)
    (gimp-vectors-set-visible (path-id path)
                              1)))
(define path-show!
  (guard-proc 'path-show!
              _path-show!
              (list 'path)
              (list path?)))

;;; Procedure:
;;;   path-hide!
;;; Parameters:
;;;   path, a path
;;; Purpose:
;;;   Makes the specified path invisible
;;; Produces:
;;;   nothing - called for the postconditions
;;; Preconditions:
;;;   none

(define _path-hide!
  (lambda (path)
    (gimp-vectors-set-visible (path-id path)
                              0)))
(define path-hide!
  (guard-proc 'path-hide!
              _path-hide!
              (list 'path)
              (list path?)))

;;; Procedure:
;;;   path-bezier-new!
;;; Parameters:
;;;   path, a path
;;;   point, a position
;;; Purpose:
;;;   Creates an anchor point at point which can be extended using other procedures
;;; Produces:
;;;   bezier-anchor, a bezier stroke
;;; Preconditions:
;;;   none

(define _path-bezier-new!
  (lambda (path point)
    (car (gimp-vectors-bezier-stroke-new-moveto (path-id path)
                                                (position-col point)
                                                (position-row point)))))

(define path-bezier-new!
  (guard-proc 'path-bezier-new!
              _path-bezier-new!
              (list 'path 'position)
              (list path? position?)))

;;; Procedure:
;;;   path-bezier-extend-line!
;;; Parameters:
;;;   path, a path
;;;   bezier, an open bezier
;;;   point, a position
;;; Purpose:
;;;   extends the bezier to point by a straight line
;;; Produces:
;;;   anchor, an anchor point at point
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   the next extend procedure that calls bezier will extend it from anchor

(define _path-bezier-extend-line!
  (lambda (path bezier point)
    (path-hide! path)
    (gimp-vectors-bezier-stroke-lineto (path-id path)
                                       bezier
                                       (position-col point)
                                       (position-row point))
    (path-show! path)))

(define path-bezier-extend-line!
  (lambda (path bezier point)
    (cond ((not (path-bezier-open? path bezier))
           (error "path-bezier-extend-line!: expects an open bezier, given a closed one."))
          ((not (position? point))
           (error "path-bezier-extend-line!: point must be a position, given " point))
          (else 
           (_path-bezier-extend-line! path bezier point)))))

;;; Procedure:
;;;   path-bezier-line!
;;; Parameters:
;;;   path, a path 
;;;   start, a position
;;;   end, a position
;;; Purpose:
;;;   create a straight bezier from (col1, row1) to (col2, row2) 
;;; Produces:
;;;   bezier-line, a bezier
;;; Preconditions:
;;;    none

(define _path-bezier-line!
  (lambda (path start end)
    (let ((bezier (path-bezier-new! path
                                    start)))
      (path-hide! path)
      (path-bezier-extend-line! path
                                bezier
                                end)
      (path-show! path)
      bezier)))

(define path-bezier-line!
  (guard-proc 'path-bezier-line!
              _path-bezier-line!
              (list 'path 'position 'position)
              (list path? position? position?)))

;;; Procedure:
;;;   path-bezier-extend-conic!
;;; Parameters:
;;;   path, a path
;;;   bezier, a bezier
;;;   point, a position
;;;   handle, a position
;;; Purpose:
;;;   Extends bezier from its most recently created anchor point to point with a curve toward
;;;   handle
;;; Produces:
;;;   anchor, a new anchor point on bezier
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   The next extend procedure that calls bezier will extend it from anchor

(define _path-bezier-extend-conic!
  (lambda (path bezier point handle)
    (path-hide! path)
    (gimp-vectors-bezier-stroke-conicto (path-id path)
                                        bezier
                                        (position-col handle)
                                        (position-row handle)
                                        (position-col point)
                                        (position-row point))
    (path-show! path)))

(define path-bezier-extend-conic!
  (lambda (path bezier point handle)
    (cond ((not (path-bezier-open? path bezier))
           (error "path-bezier-extend-conic!: expects an open bezier, given a closed one."))
          ((not (position? point))
           (error "path-bezier-extend-conic!: point must be a position, given " point))
          ((not (position? handle))
           (error "path-bezier-extend-conic!: handle must be a position, given " handle))
          (else 
           (_path-bezier-extend-conic! path bezier point handle)))))

;;; Procedure:
;;;   path-bezier-conic!
;;; Parameters:
;;;   path, a path
;;;   start, a position
;;;   end, a position
;;;   handle, a position
;;; Purpose:
;;;   To create a bezier from start to end that curves toward handle
;;; Produces:
;;;   bezier-conic, a bezier
;;; Preconditions:
;;;   none

(define _path-bezier-conic!
  (lambda (path start end handle)
    (let ((bezier (path-bezier-new! path
                                    start)))
      (path-bezier-extend-conic! path
                                  bezier
                                  end
                                  handle)
      bezier)))

(define path-bezier-conic!
  (guard-proc 'path-bezier-conic!
              _path-bezier-conic!
              (list 'path 'position 'position 'position)
              (list path? position? position? position?)))

;;; Procedure:
;;;   path-bezier-extend-auto-conic!
;;; Parameters:
;;;   path,a  path
;;;   bezier, an open bezier
;;;   extend-to, a position
;;;   curviness, a real
;;; Purpose:
;;;   extend bezier from its most recent anchor with a conic curve
;;; Produces:
;;;   new-anchor, an anchor
;;; Preconditions:
;;;   bezier is located in path
;;; Postconditions:
;;;   The transition from beziers most recent curve to the extended curve will not be smooth

(define _path-bezier-extend-auto-conic!
  (lambda (path bezier extend-to curviness)
    (let* ((extend-from (path-bezier-get-last-anchor path bezier))
           (col1 (position-col extend-from))
           (row1 (position-row extend-from))
           (col2 (position-col extend-to))
           (row2 (position-row extend-to))
           (delta-row (- row2 row1))
           (delta-col (- col2 col1))
           (dist (sqrt (+ (square delta-row) (square delta-col))))
           (delta-row2 (/ delta-row dist))
           (delta-col2 (/ delta-col dist))
           (scale (* 0.5 dist curviness))
           (perp-slope (if (= row2 row1)
                           (* 1 delta-col) ;;don't divide by 0
                           (* 1 (/ delta-col delta-row)))) ;;slope of the line perpendicular to anchor1->anchor2
           (mid-x (* 0.5 (+ col2 col1))) ;;x coordinate of anchor1->anchor2's midpoint
           (mid-y (* 0.5 (+ row2 row1))) ;;y coordinate of anchor1->anchor2's midpoint
           (perp-y-int (- mid-y (* perp-slope mid-x))) ;; y intercept of the perpendicular line
           (handle-slope (* -1 curviness)) 
           (handle-y-int (- row1 (* handle-slope col1))) ;;y intercept of line anchor1->handle
           (handle-x (if (= row2 row1)
                         (/ (- handle-y-int perp-y-int) 
                            (- perp-slope handle-slope))
                         (+ (* scale delta-row2) mid-x)))
           (handle-y (if (= row2 row1)
                         (+ (* handle-slope handle-x) handle-y-int)
                         (+ (* -1 scale delta-col2) mid-y))))
      (path-hide! path)
      (_path-bezier-extend-conic! path bezier extend-to (position handle-x handle-y))
      (path-show! path))))

(define path-bezier-extend-auto-conic!
  (lambda (path bezier extend-to curviness)
    (cond ((not (path-bezier-open? path bezier))
           (error "path-bezier-extend-auto-conic!: bezier must be open, given a closed bezier."))
          ((not (position? extend-to))
           (error "path-bezier-extend-auto-conic!: extend-to must be a position, given " extend-to))
          ((not (real? curviness))
           (error "path-bezier-extend-auto-conic!: curviness must be a real number, given " curviness))
          (else
           (_path-bezier-extend-auto-conic! path bezier extend-to curviness)))))

;;; Procedure:
;;;   path-bezier-auto-conic!
;;; Parameters:
;;;   path, a path
;;;   anchor1, a position
;;;   anchor2, a position
;;;   curviness, a real
;;; Purpose:
;;;   create a reasonable bezier that curves toward a control point calculated by the code
;;; Produces:
;;;   bezier-conic, a bezier
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   bezier begins at anchor 1, curves toward a control point determined by curviness, and ends at anchor 2

(define _path-bezier-auto-conic!
  (lambda (path anchor1 anchor2 curviness)
    (let ((bezier (path-bezier-new! path anchor1)))
      (path-bezier-extend-auto-conic! path bezier anchor2 curviness)
      bezier)))

(define path-bezier-auto-conic!
  (guard-proc 'path-bezier-auto-conic!
              _path-bezier-auto-conic!
              (list 'path 'position 'position 'real)
              (list path? position? position? real?)))

;;; Procedure:
;;;   path-bezier-extend-cubic!
;;; Parameters:
;;;   path, a path
;;;   bezier, a bezier
;;;   point, a position
;;;   handle1, a position
;;;   handle2, a position
;;; Purpose:
;;;   To extend bezier from its most recently created anchor point to point, with curves toward
;;;   handle1 and handle2
;;; Produces:
;;;   anchor, a new anchor point on bezier
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   The next extend procedure that calls bezier will extend it from anchor

(define _path-bezier-extend-cubic!
  (lambda (path bezier point handle1 handle2)
    (path-hide! path)
    (gimp-vectors-bezier-stroke-cubicto (path-id path)
                                        bezier
                                        (position-col handle1)
                                        (position-row handle1)
                                        (position-col handle2)
                                        (position-row handle2)
                                        (position-col point)
                                        (position-row point))
    (path-show! path)))

(define path-bezier-extend-cubic!
  (lambda (path bezier point handle1 handle2)
    (cond ((not (path-bezier-open? path bezier))
           (error "path-bezier-extend-cubic!: expects an open bezier, given a closed one."))
          ((not (position? point))
           (error "path-bezier-extend-cubic!: point must be a position, given " point))
          ((not (position? handle1))
           (error "path-bezier-extend-cubic!: handle1 must be a position, given " handle1))
          ((not (position? handle2))
           (error "path-bezier-extend-cubic!: handle2 must be a position, given " handle2))
          (else 
           (_path-bezier-extend-cubic! path bezier point handle1 handle2)))))

;;; Procedure:
;;;   path-bezier-cubic!
;;; Parameters:
;;;   path, a path
;;;   start
;;;   end
;;;   handle1
;;;   handle2
;;; Purpose:
;;;   Creates a bezier from start to end that curves toward handle1 and handle2
;;; Produces:
;;;   anchor, a new anchor point on bezier
;;; Preconditions:
;;;   none

(define _path-bezier-cubic!
  (lambda (path start end handle1 handle2)
    (let ((bezier (path-bezier-new! path
                                    start)))
      (path-bezier-extend-cubic! path
                                  bezier
                                  end
                                  handle1
                                  handle2)
      bezier)))

(define path-bezier-cubic!
  (guard-proc 'path-bezier-cubic!
              _path-bezier-cubic!
              (list 'path 'position 'position 'position 'position)
              (list path? position? position? position? position?)))

;;; Procedure:
;;;   bezier-positions->floats
;;; Parameters:
;;;   bezier-position-list, a list of bezier positions
;;; Purpose:
;;;   translate the list of bezier positions into a list that the GIMP PDB procedures can interpret correctly
;;; Produces:
;;;   coords-list, a list of reals
;;; Preconditions:
;;;   the coordinates in each bezier position are in the order 
;;;      (anchor-x anchor-y handle1-x handle1-y handle2-x handle2-y)
;;; Postconditions:
;;;   The coordinates in coords-list are in the order
;;;     (handle1-x1 handle1-y1 anchor-x1 anchor-y1 handle2-x1 handle2-y1
;;;      handle1-x2 handle1-y2 anchor-x2 anchor-y2 handle2-x2 handle2-y2
;;;      ...
;;;      handle1-xn handle1-yn anchor-xn anchor-yn handle2-xn handle2-yn)

(define _bezier-positions->floats
  (lambda (positions)
    (let* ((len (length positions))
           (floats (make-vector (* len 6)))
           (positions-in-floats (iota 6)) 
           (new-coord-order (list 2 3 0 1 4 5))) ;New order for the coordinates
      (let kernel ((pos 0)
                   (remaining positions))
        (if (null? remaining)
            floats
            (let ((current-position (list->vector (car remaining))))
              (map (lambda (x y) 
                     (vector-set! floats (+ pos x) (exact->inexact (vector-ref current-position y))))
                   positions-in-floats
                   new-coord-order)
              (kernel (+ pos 6) (cdr remaining))))))))

(define bezier-positions->floats
  (guard-proc 'bezier-positions->floats
              _bezier-positions->floats
              (list 'bezier-position-list)
              (list bezier-position-list?)))

;;; Procedure:
;;;   bezier-line-anchors->floats
;;; Parameters:
;;;   positions, a list of positions
;;; Purpose:
;;;   Converts a list of positions to a vector that the GIMP can use to draw bezier subpaths
;;; Produces:
;;;   floats, a vector
;;; Preconditions:
;;;   elements in positions contain two coordinates as a pair
;;; Postconditions:
;;;   

(define _bezier-line-anchors->floats
  (lambda (positions)
    (let* ((len (length positions))
           (floats (make-vector (* len 6)))
           (positions-in-floats (iota 6)) 
           (new-coord-order (list 0 1 0 1 0 1))) ;handle coordinates are the same as the anchor
      (let kernel ((pos 0)
                   (remaining positions))
        (if (null? remaining)
            floats
            (let* ((coordinates (car remaining))
                   (current-position (vector (position-col coordinates) 
                                             (position-row coordinates))))
              (map (lambda (x y) 
                     (vector-set! floats (+ pos x) (exact->inexact (vector-ref current-position y))))
                   positions-in-floats
                   new-coord-order)
              (kernel (+ pos 6) (cdr remaining))))))))

(define bezier-line-anchors->floats
  (guard-proc 'bezier-line-anchors->floats
              _bezier-line-anchors->floats
              (list 'position-list)
              (list position-list?)))

;;; Procedure:
;;;   path-bezier-curves!
;;; Parameters:
;;;   path, a path
;;;   points, a list of bezier positions (list of lists)
;;; Purpose:
;;;   creates a bezier curve that passes through each anchor point and is controlled by the given handles
;;; Produces:
;;;   curved-bezier, a subpath
;;; Preconditions:
;;;   bezier positions must be created in the order 
;;;     (anchor-x anchor-y handle1-x handle1-y handle2-x handle2-y), 
;;;       where handle1 controls the curve from the previous anchor and 
;;;       handle2 controls the curve to the next anchor
;;; Postconditions:
;;;   extend procedures extend the bezier curve from the anchor of the last position in the list 
;;;   in the direction of the last handle

(define _path-bezier-curves!
  (lambda (path points)
    (let ((floats (bezier-positions->floats points)))
      (path-hide! path)
      (car (gimp-vectors-stroke-new-from-points (path-id path)
                                                0 ;;sets bezier as type of stroke. Bezier is the only type of stroke.
                                                (vector-length floats)
                                                floats
                                                FALSE)) ;The curve is not closed
      (path-show! path))))

(define path-bezier-curves!
  (guard-proc 'path-bezier-curves!
              _path-bezier-curves!
              (list 'path 'bezier-position-list)
              (list path? bezier-position-list?)))

;;; Procedure:
;;;   path-curve-the-dots!
;;; Parameters:
;;;   path,  a path
;;;   dots, a list of positions
;;;   curviness, a real
;;; Purpose:
;;;   creates a bezier on path that passes through each position in dots with reasonable curves
;;; Produces:
;;;   
;;; Preconditions:
;;;   
;;; Postconditions:
;;;   

(define _path-curve-the-dots!
  (letrec (
           (compute-bezier-positions
            (lambda (prev current next curviness)
              (let* ((delta-col-1 
                      (- (position-col prev) (position-col current))) ;dist between x (prev) and x (current)
                     (delta-col-2 
                      (- (position-col next) (position-col current))) ;dist between x (next) and x (current)
                     (delta-row-1 
                      (- (position-row prev) (position-row current))) ;dist between y (prev) and y (current)
                     (delta-row-2 
                      (- (position-row next) (position-row current))) ;dist between y (next) and y (current)
                     (dist-1 
                      (sqrt (+ (square delta-col-1) (square delta-row-1)))) ;dist between prev and current
                     (dist-2 
                      (sqrt (+ (square delta-col-2) (square delta-row-2)))) ;dist between prev and current
                     (temp-col 
                      (* 0.5 (+ (/ delta-col-1 dist-1) (/ delta-col-2 dist-2)))) ;
                     (temp-row 
                      (* 0.5 (+ (/ delta-row-1 dist-1) (/ delta-row-2 dist-2)))) ;
                     (scale 
                      (sqrt (+ (square temp-col) (square temp-row)))) ;
                     (delta-col 
                      (/ temp-col scale)) ;
                     (delta-row 
                      (/ temp-row scale)) ;
                     (control-1 
                      (position-offset current 
                                       (* -20 delta-row) (* 20 delta-col)))
                     (control-2 
                      (position-offset current 
                                       (* 20 delta-row) (* -20 delta-col)))
                     (scale-1 (* 0.5 curviness dist-1))
                     (scale-2 (* 0.5 curviness dist-2))
                     (helper 
                      (if (< (position-distance control-1 prev)
                             (position-distance control-2 prev))
                          (cons 1 -1)
                          (cons -1 1)))
                     )
                (list 
                 (+ (* (car helper) scale-1 delta-col) (position-col current))
                 (+ (* (cdr helper) scale-1 delta-row) (position-row current))
                 (exact->inexact (car current))
                 (exact->inexact (cdr current))
                 (+ (* (cdr helper) scale-2 delta-col) (position-col current))
                 (+ (* (car helper) scale-2 delta-row) (position-row current))))))
           (dot->bezier-position
            (lambda (dot)
              (let ((col (position-col dot))
                    (row (position-row dot)))
                (map exact->inexact (list col row col row col row)))))
           (dots->bezier
            (lambda (dots curviness)
              (let ((bezier-vec (make-vector (* 6 (length dots))))
                    (list-6 (iota 6)))
                (map (lambda (k val) (vector-set! bezier-vec k val)) 
                     list-6 
                     (dot->bezier-position (car dots)))
                (let kernel ((pos 6)
                             (prev (car dots))
                             (current (cadr dots))
                             (remaining (cddr dots)))
                  (if (null? remaining)
                      (map (lambda (k val)(vector-set! bezier-vec (+ pos k) val)) 
                           list-6 
                           (dot->bezier-position current))
                      (let ((next (car remaining)))
                        (map (lambda (k val) (vector-set! bezier-vec (+ pos k) val)) 
                             list-6 
                             (compute-bezier-positions prev current next curviness))
                        (kernel (+ 6 pos) current (car remaining) (cdr remaining)))))
                bezier-vec))))
    
    (lambda (path dots curviness)
      (let ((positions (dots->bezier dots curviness)))
        (path-hide! path)
        (gimp-vectors-stroke-new-from-points (path-id path)
                                             0
                                             (* 6 (length dots))
                                             positions
                                             FALSE) ;;not closed
        (path-show! path)
        (display positions)
        (newline)))))

(define path-curve-the-dots!
  (guard-proc 'path-curve-the-dots!
              _path-curve-the-dots!
              (list 'path 'position-list 'real)
              (list path? position-list? real?)))

;;; Procedure:
;;;   path-bezier-lines
;;; Parameters:
;;;   path, a path
;;;   points, a list of positions
;;; Purpose:
;;;   draw a bezier that passes through each position in points and changes direction as an angle, not a curve
;;; Produces:
;;;   straight-bezier, a subpath
;;; Preconditions:
;;;   no additional preconditions
;;; Postconditions:
;;;   straight-bezier does not form a closed shape

(define _path-bezier-lines!
  (lambda (path points)
    (let ((floats (bezier-line-anchors->floats points)))
      (path-hide! path)
      (car (gimp-vectors-stroke-new-from-points (path-id path)
                                                0 ;;sets bezier as type of stroke. Bezier is the only type of stroke.
                                                (vector-length floats)
                                                floats
                                                FALSE)) ;;The curve is not closed
      (path-show! path))))

(define path-bezier-lines!
  (guard-proc 'path-bezier-lines!
              _path-bezier-lines!
              (list 'path 'position-list)
              (list path? position-list?)))

;;; Procedure:
;;;   path-bezier-polygon!
;;; Parameters:
;;;   path, a path
;;;   points, a list of positions
;;; Purpose:
;;;   create a closed bezier on path that passes through each point in points and doesn't curve
;;; Produces:
;;;   bezier-polygon, a closed bezier
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   (length points) = the number of sides on bezier-polygon

(define _path-bezier-polygon!
  (lambda (path points)
    (let ((floats (bezier-line-anchors->floats points)))
      (path-hide! path)
      (car (gimp-vectors-stroke-new-from-points (path-id path)
                                                0 ;;sets bezier as type of stroke. Bezier is the only type of stroke.
                                                (vector-length floats)
                                                floats
                                                TRUE)) ;;The bezier is closed
      (path-show! path))))

(define path-bezier-polygon!
  (guard-proc 'path-bezier-polygon!
              _path-bezier-polygon!
              (list 'path 'position-list)
              (list path? position-list?)))

;;; Procedure:
;;;   path-bezier-curves-closed!
;;; Parameters:
;;;   path, a path
;;;   points, a list of bezier positions
;;; Purpose:
;;;   create a closed bezier on path that passes through all anchor points in path and is controlled by the given handles
;;; Produces:
;;;   bezier-closed-curve, a bezier
;;; Preconditions:
;;;   bezier positions should be in the order (anchor-col anchor-row handle1-col handle1-row handle2-col handle2-row)
;;; Postconditions:
;;;   handle1 in the first position and handle2 in the last position direct the curve from the last anchor to the first

(define _path-bezier-curves-closed!
  (lambda (path points)
    (let ((floats (bezier-positions->floats points)))
      (path-hide! path)
      (car (gimp-vectors-stroke-new-from-points (path-id path)
                                                0 ;;sets bezier as type of stroke. Bezier is the only type of stroke.
                                                (vector-length floats)
                                                floats
                                                TRUE)) ;;The curve is closed
      (path-show! path))))

(define path-bezier-curves-closed!
  (guard-proc 'path-bezier-curves-closed!
              _path-bezier-curves-closed!
              (list 'path 'bezier-position-list)
              (list path? bezier-position-list?)))

;;; Procedure:
;;;   path-ellipse!
;;; Parameters:
;;;   path, a path
;;;   center, a position
;;;   xradius, a real
;;;   yradius, a real
;;;   angle, a real in radians
;;; Purpose:
;;;   creates a closed elliptical bezier centered at center with a radius of xradius 
;;;    along the x axis and a radius of yradius along the y axis. 
;;;    Its x axis is rotated by the given angle
;;; Produces:
;;;   ellipse-bezier, a bezier
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   


(define _path-ellipse!
  (lambda (path center xradius yradius angle)
    (path-hide! path)
    (gimp-vectors-bezier-stroke-new-ellipse (path-id path)
                                            (position-col center)
                                            (position-row center)
                                            xradius
                                            yradius
                                            angle)
    (path-show! path)
    ))

(define path-ellipse!
  (guard-proc 'path-ellipse!
              _path-ellipse!
              (list 'path 'position 'real 'real 'real)
              (list path? position? real? real? real?)))

;;; Procedure:
;;;   path-circle!
;;; Parameters:
;;;   path, a path
;;;   center, a position
;;;   radius, a real
;;; Purpose:
;;;   Creates a closed circular bezier centered at center on path
;;; Produces:
;;;   circle-bezier, a bezier 
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   

(define _path-circle!
  (lambda (path center radius)
    (path-hide! path)
    (gimp-vectors-bezier-stroke-new-ellipse (path-id path)
                                            (position-col center)
                                            (position-row center)
                                            radius ;x direction
                                            radius ;y direction
                                            0) ;angle of the x-axis
    (path-show! path)))


(define path-circle!
  (guard-proc 'path-circle!
              _path-circle!
              (list 'path 'position 'real)
              (list path? position? real?)))


;;; Procedure:
;;;   path-bezier-close!
;;; Parameters:
;;;   path, a path
;;;   bezier, a bezier
;;; Purpose:
;;;   closes bezier with a curve that follows the handles of the last and first anchors
;;; Produces:
;;;   nothing; called for side effects
;;; Preconditions:
;;;   bezier is open
;;; Postconditions:
;;;   bezier is considered a closed bezier and is unsuitable for procedures such as path-bezier-extend-line!

(define _path-bezier-close!
  (lambda (path bezier)
    (path-hide! path)
    (gimp-vectors-stroke-close (path-id path)
                               bezier)
    (path-show! path)))

(define path-bezier-close!
  (lambda (path bezier)
    (cond ((not (path? path))
           (error "path-bezier-close!: path must be a valid path name or id, given " path))
          ((not (path-valid-bezier? path bezier))
           (error "path-bezier-close!: bezier is not a valid bezier id on the given path. Valid beziers ids for the given path are " (path-list-beziers path)))
          ((not (_path-bezier-open? path bezier))
           (error "path-bezier-close!: bezier is already closed. bezier id: " bezier))
          (else
           (_path-bezier-close! path bezier)))))

;;; Procedure:
;;;   path-stroke!
;;; Parameters:
;;;   path, a path
;;; Purpose:
;;;   Strokes all beziers in the selected path using the active brush and foreground color
;;;      Note that this does not include stroking using a gradient.
;;; Produces:
;;;   nothing; called for side effects
;;; Preconditions:
;;;   none 

(define _path-stroke!
  (lambda (path)
    (gimp-edit-stroke-vectors (image-get-layer (path-get-image path))
                              (path-id path))
    (cond ((context-immediate-updates?) (context-update-displays!)))
    (gimp-vectors-get-image (path-id path))))

(define path-stroke!
  (guard-proc 'path-stroke!
              _path-stroke!
              (list 'path)
              (list path?)))

;;; Procedure:
;;;   path->selection!
;;; Parameters:
;;;   path, a path
;;;   selection type, one of the valid GIMP selection types
;;; Purpose:
;;;   create a selection shaped like path on the active layer of the image that path belongs to
;;; Produces:
;;;   path-selection, a selection
;;; Preconditions:
;;;   
;;; Postconditions:
;;;   When an open bezier is turned into a selection, the selection closes itself
;;;    with a straight line from the last anchor point to the first


(define _path->selection!
  (lambda (path selection-type)
    (gimp-vectors-to-selection (path-id path)
                               selection-type
                               TRUE ;;antialias is on to be consistent with the other select procedures
                               FALSE ;;no feathering
                               0 0) ;;no feathering means that feather radius is 0
    ))

(define path->selection!
  (lambda (path selection-type)
    (cond ((not (member selection-type (list ADD SUBTRACT REPLACE INTERSECT)))
           (error "selection-type must be ADD, SUBTRACT, REPLACE, or INTERSECT, given " 
                  (number->string selection-type)))
           ((not (path? path))
            (error "path must be a valid path name or id, given " path))
           (else
            (_path->selection! path selection-type)))))


;;; Procedure:
;;;   path-fill!
;;; Parameters:
;;;   path, a path
;;; Purpose:
;;;   fills the beziers in path with the current foreground color
;;; Produces:
;;;   nothing - caled for the side effects
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;    When an open bezier is filled, it behaves as if the bezier has been closed by 
;;;       a straight line from the last anchor to the first
;;;    If the image that path belongs to has any selections, they will be lost

(define _path-fill!
  (lambda (path)
    (path-fill-unabridged! path 
                           FOREGROUND-FILL
                           NORMAL-MODE ;;paint mode
                           100)))

(define path-fill!
  (guard-proc 'path-fill!
              _path-fill!
              (list 'path)
              (list path?)))

;;; Procedure:
;;;   path-fill-pattern!
;;; Parameters:
;;;   path, a path
;;; Purpose:
;;;   fills the beziers in path with the current pattern
;;; Produces:
;;;   nothing; called for side effects
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   When an open bezier is filled, it behaves as if the bezier has been closed by 
;;;       a straight line from the last anchor to the first
;;;    If the image that path belongs to has any selections, they will be lost

(define _path-fill-pattern!
  (lambda (path)
    (path-fill-unabridged! path 
                           PATTERN-FILL
                           NORMAL-MODE ;;paint mode
                           100))) ;;opacity

(define path-fill-pattern!
  (guard-proc 'path-fill-pattern!
              _path-fill-pattern!
              (list 'path)
              (list path?)))

;;; Procedure:
;;;   path-fill-opacity!
;;; Parameters:
;;;   path, a path
;;;   opacity, a real
;;; Purpose:
;;;   fills the beziers in path with the foreground color at the given opacity
;;; Produces:
;;;   nothing; called for side effects
;;; Preconditions:
;;;   0 <= opacity <=100
;;; Postconditions:
;;;   When an open bezier is filled, it behaves as if the bezier has been closed by 
;;;       a straight line from the last anchor to the first
;;;    If the image that path belongs to has any selections, they will be lost

(define _path-fill-opacity!
  (lambda (path opacity)
    (path-fill-unabridged! path 
                           FOREGROUND-FILL
                           NORMAL-MODE ;;paint mode
                           opacity)))

(define path-fill-opacity!
  (guard-proc 'path-fill-opacity!
              _path-fill-opacity!
              (list 'path 'opacity)
              (list path? opacity?)))

;;; Procedure:
;;;   path-fill-unabridged!
;;; Parameters:
;;;   path, a path
;;;   fill-mode, a valid fill mode
;;;   paint-mode, a valid GIMP paint mode
;;;   opacity, a real
;;; Purpose:
;;;   fills the beziers in path with the given settings
;;; Produces:
;;;   nothing; called for side effects
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   When an open bezier is filled, it behaves as if the bezier has been closed by 
;;;       a straight line from the last anchor to the first
;;;    If the image that path belongs to has any selections, they will be lost

(define _path-fill-unabridged!
  (lambda (path fill-mode paint-mode opacity)
    (let ((image (path-get-image path)))
      (path->selection! path REPLACE)
      (gimp-edit-bucket-fill (image-get-layer image)
                             fill-mode
                             paint-mode
                             opacity
                             255       ;Threshold - all of the selection will be filled regardless of what is currently on the layer
                             TRUE      ;uses the composite image, not just the current layer
                             0 0)      ;coordinates for the "bucket" fill - this doesn't really apply for selections
      (image-select-nothing! image))))

(define path-fill-unabridged!
  (lambda (path fill-mode paint-mode opacity)
    (cond ((not (path? path))
           (path? path)) ;;called for the built in error message
          ((not (member fill-mode (list FOREGROUND-FILL BACKGROUND-FILL PATTERN-FILL)))
           (error "fill-mode must be FOREGROUND-FILL, BACKGROUND-FILL, or PATTERN-FILL, given " fill-mode))
          ((not (paint-mode? paint-mode))
           (paint-mode? paint-mode)) ;;called for the built in error message
          ((not (opacity? opacity))
           (opacity? opacity)) ;;called for the built in error message
          (else 
           (_path-fill-unabridged! path fill-mode paint-mode opacity)))))

;;; Procedure:
;;;   path-raise!
;;; Parameters:
;;;   path, a path
;;; Purpose:
;;;   move path up once in the paths list
;;; Produces:
;;;   Nothing; called for the side effects
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   (path-get-position path) is one less than before the procedure is called

(define _path-raise!
  (lambda (path)
    (gimp-image-raise-vectors (path-get-image path)
                              (path-id path))))

(define path-raise!
  (lambda (path)
    (cond ((not (path? path))
           (error "path-raise!: path must be a valid path name or id, given " path))
          ((= 0 (path-get-position path))
           (error "path-raise!: path is already at the top of the paths list and cannot be raised further."))
          (else
           (_path-raise! path)))))

;;; Procedure:
;;;   path-raise-to-top!
;;; Parameters:
;;;   path, a path
;;; Purpose:
;;;   raises path to position 0 in the paths list of the image path belongs to
;;; Produces:
;;;   nothing; called for side effects
;;; Preconditions:
;;;   path is not already at the top
;;; Postconditions:
;;;   (path-get-position path) will return 0

(define _path-raise-to-top!
  (lambda (path)
    (gimp-image-raise-vectors-to-top (path-get-image path)
                                     (path-id path))))

(define path-raise-to-top!
  (lambda (path)
    (cond ((not (path? path))
           (error "path-raise-to-top!: path must be a valid path name or id, given " path))
          ((= 0 (path-get-position path))
           (error "path-raise-to-top!: path is already at the top position of the paths list."))
          (else
           (_path-raise-to-top! path)))))

;;; Procedure:
;;;   path-lower!
;;; Parameters:
;;;   path, a path
;;; Purpose:
;;;   moves path down once in the paths list of the image it belongs to
;;; Produces:
;;;   Nothing; called for side effects
;;; Preconditions:
;;;   path is not already at the bottom of the paths list
;;; Postconditions:
;;;   (path-get-position path) is one more than before

(define _path-lower!
  (lambda (path)
    (gimp-image-lower-vectors (path-get-image path)
                              (path-id path))))

(define path-lower!
  (lambda (path)
    (cond ((not (path? path))
           (error "path-lower!: path must be a valid path name or id, given " path))
          ((= (- 1 (car (gimp-image-get-vectors (path-get-image path)))) 
              (path-get-position path))
           (error "path-lower!: path is already at the bottom of the paths list and cannot be lowered any further."))
          (else
           (_path-lower! path)))))

;;; Procedure:
;;;   path-lower-to-bottom!
;;; Parameters:
;;;   path, a path
;;; Purpose:
;;;   lowers path to the bottom of the paths list of the image it belongs to
;;; Produces:
;;;   Nothing; called for side effects
;;; Preconditions:
;;;   path is not already at the bottom of the stack
;;; Postconditions:
;;;   path's position = number of paths belonging to the same image - 1

(define _path-lower-to-bottom!
  (lambda (path)
    (gimp-image-lower-vectors-to-bottom (path-get-image path)
                                        (path-id path))))

(define path-lower-to-bottom!
  (lambda (path)
    (cond ((not (path? path))
           (error "path-lower-to-bottom!: path must be a valid path name or id, given " path))
          ((= (- 1 (car (gimp-image-get-vectors (path-get-image path)))) 
              (path-get-position path))
           (error "path-lower-to-bottom!: path is already at the bottom of the paths list and cannot be lowered any further."))
          (else
           (_path-lower-to-bottom! path)))))

;;; Procedure:
;;;   path-set-position!
;;; Parameters:
;;;   path, a path
;;;   new-pos, an integer
;;; Purpose:
;;;   move path to the given position
;;; Produces:
;;;   none; called for side effects
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   new-pos <= 0; path is moved to the top of the paths list
;;;   new-pos >= the number of paths in the image; path is moved to the bottom of the paths list
;;;   else path is moved to new-pos in the paths list

(define _path-set-position!
  (lambda (path new-pos)
    (letrec (
             (kernel (lambda (current-pos goal-pos)
                     (when (< current-pos goal-pos) ;if path is currently higher than the goal position
                       (path-lower! path) ;move it down one
                       (kernel (+ current-pos 1) goal-pos)) ;and test again
                     (when (> current-pos goal-pos) ;if path is currently lower than the goal position
                       (path-raise! path) ;move it up one
                       (kernel (- current-pos 1) goal-pos)))) ;and test again
             )
      (cond ((<= new-pos 0)
             (_path-raise-to-top! path))
            ((>= new-pos (- (length (image-list-paths (path-get-image path))) 1)) ;new-pos is greater than the max position
             (_path-lower-to-bottom! path))
            (else 
             (kernel (path-get-position path) new-pos))))))

(define path-set-position!
  (guard-proc 'path-set-position!
              _path-set-position!
              (list 'path 'integer)
              (list path? integer?)))

;;; Procedure:
;;;   path-bezier-get-center
;;; Parameters:
;;;   path, a path
;;;   bezier, a bezier
;;; Purpose:
;;;   return the center of the given bezier
;;; Produces:
;;;   bezier-center, a position
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   center = ( average of the largest and smallest x coordinates of bezier's anchors
;;;              average of the largest and smallest y coordinates of bezier's anchors)
;;;    This means that in bezier curves with extreme handles, center may not be the exact center of bezier

(define path-bezier-get-center
  (lambda (path bezier)
    (letrec ((control-points-info (gimp-vectors-stroke-get-points (path-id path) bezier))
             (control-points-vector (caddr control-points-info))
             (control-points-length (cadr control-points-info))
             (num-anchor-points (/ control-points-length 6))
             (list-coordinate-type (lambda (pos coordinate-list)
                                     (if (> pos control-points-length)
                                         coordinate-list
                                         (list-coordinate-type (+ pos 6)
                                                               (cons (vector-ref control-points-vector pos)
                                                                     coordinate-list)))))
             (largest-of (lambda (largest remaining)
                           (if (null? remaining)
                               largest
                               (largest-of (max largest (car remaining))
                                           (cdr remaining)))))
             (smallest-of (lambda (smallest remaining)
                            (if (null? remaining)
                                smallest
                                (smallest-of (min smallest (car remaining))
                                             (cdr remaining)))))
             (right-bound (largest-of (vector-ref control-points-vector 2) ; 2 is the x coordinate of the first anchor
                                      (list-coordinate-type 2 null)))   
             (left-bound (smallest-of (vector-ref control-points-vector 2) 
                                      (list-coordinate-type 2 null)))
             (lower-bound (largest-of (vector-ref control-points-vector 3) ; 3 is the y coordinate of the first anchor
                                      (list-coordinate-type 3 null))) 
             (upper-bound (smallest-of (vector-ref control-points-vector 3) 
                                       (list-coordinate-type 3 null))))
      
      (position (* 0.5 (+ right-bound left-bound))
                (* 0.5 (+ lower-bound upper-bound))))))

;;; Procedure:
;;;   path-save
;;; Parameters:
;;;   path, a path
;;;   file-name, a string
;;; Purpose:
;;;   saves the path as an svg file
;;; Produces:
;;;   file-name.svg, a svg file
;;; Preconditions:
;;;   file-name must end with .svg
;;; Postconditions:
;;;   file-name.svg is saved to the home directory (?)

(define _path-save
  (lambda (path file-name)
    (gimp-vectors-export-to-file (path-get-image path)
                                 file-name
                                 (path-id path))))

(define path-save
  (lambda (path file-name)
    (cond ((not (path? path))
           (error "path-save: path must be a valid path name or id, given " path))
          ((not (string? file-name))
           (error "path-save: file-name must be a string, given " file-name))
          ((> 5 (string-length file-name))
           (error "path-save: file-name must contain at least one character and end with \".svg\", given " file-name))
          ((not (string-ci=? ".svg" (substring file-name 
                                         (- (string-length file-name) 4)
                                         (string-length file-name))))
           (error "path-save: file-name must end with the file extension \".svg\", given " file-name))
          (else
           (_path-save path file-name)))))

;;; Procedure:
;;;   image-path-saves
;;; Parameters:
;;;   image, an image
;;;   file-name, a string
;;; Purpose:
;;;   saves all paths in image as one svg file
;;; Produces:
;;;   file-name.svg, an svg file
;;; Preconditions:
;;;   file-name must end with ".svg"
;;;   image contains at least one path
;;; Postconditions:
;;;   file-name.svg is saved to the home directory (?)

(define _image-path-saves
  (lambda (image file-name)
    (gimp-vectors-export-to-file image
                                 (string-append file-name ".svg")
                                 0)))

(define image-path-saves
  (lambda (image file-name)
    (cond ((not (image? image))
           (error "image-path-saves: image must be a valid image, given " image))
          ((= 0 (car (gimp-image-get-vectors image)))
           (error "image-path-saves: image must contain at least one path."))
          ((not (string? file-name))
           (error "path-save: file-name must be a string, given " file-name))
          ((> 5 (string-length file-name))
           (error "path-save: file-name must contain at least one character and end with \".svg\", given " file-name))
          ((not (string-ci=? ".svg" (substring file-name 
                                               (- (string-length file-name) 4)
                                               (string-length file-name))))
           (error "path-save: file-name must with the file extension \".svg\", given " file-name))
          (else
           (_image-path-saves image file-name)))))

;;; Procedure:
;;;   image-path-load
;;; Parameters:
;;;   image, an image
;;;   svg-file, a string
;;; Purpose:
;;;   import an svg file into the image as a path
;;; Produces:
;;;   svg-path, the path (or paths) included in the svg file
;;; Preconditions:
;;;   svg-file is the filename of the SVG file to be imported (ie "svg-file.svg")
;;;   svg-file should be in the home directory (?)
;;; Postconditions:
;;;   if svg-file includes multiple paths, they will load into the image as separate paths
;;;   if svg-path is/are larger or smaller than image, it/they will not scale to the image size
;;;   svg-path is/are set to visible

(define _image-path-load
  (lambda (image svg-file)
    (let ((svg-path (cadr (gimp-vectors-import-from-file image
                                                         svg-file
                                                         FALSE ;don't merge
                                                         FALSE)))) ;don't scale
      (auto-rename-paths svg-path)
      (vector-map path-show! svg-path)
      (vector->list svg-path))))

(define image-path-load 
  (lambda (image svg-file)
    (cond ((not (image? image))
           (error "image-path-load: image does not exist, given " image))
          ((not (file-exists? svg-file))
           (error "image-path-load: no such file " svg-file))
          (else
           (_image-path-load image svg-file)))))

;;; Procedure:
;;;   image-path-load-scaled
;;; Parameters:
;;;   image, an image
;;;   svg-file, a string
;;; Purpose:
;;;   import an svg file into the image as a path
;;; Produces:
;;;   svg-path, the path (or paths) included in the svg file
;;; Preconditions:
;;;   svg-file is the filename of the SVG file to be imported (ie "svg-file.svg")
;;;   svg-file should be in the home directory (?)
;;; Postconditions:
;;;   if svg-file includes multiple paths, they will load into the image as separate paths
;;;   if svg-path is/are larger or smaller than image, it/they will scale to the image size
;;;   svg-path is/are set to visible

(define _image-path-load-scaled
  (lambda (image svg-file)
    (let ((svg-path (cadr (gimp-vectors-import-from-file image
                                                         svg-file
                                                         FALSE ;don't merge
                                                         TRUE)))) ;do scale
      (auto-rename-paths svg-path)
      (vector-map path-show! svg-path)
      svg-path)))

(define image-path-load-scaled 
  (lambda (image svg-file)
    (cond ((not (image? image))
           (error "image-path-load-scaled: image does not exist, given " image))
          ((not (file-exists? svg-file))
           (error "image-path-load-scaled: no such file " svg-file))
          (else
           (_image-path-load-scaled image svg-file)))))

;;; Procedure:
;;;   path->string
;;; Parameters:
;;;   path, a path
;;; Purpose:
;;;   return a string that hold a complete XML document repesenting path
;;; Produces:
;;;   svg-string, a %NUL-terminated string
;;; Preconditions:
;;;   path exists on an image
;;; Postconditions:
;;;   svg-string is %NUL-terminated and can be used to reproduce path

(define _path->string
  (lambda (path)
    (car (gimp-vectors-export-to-string (path-get-image path)
                                        (path-id path)))))

(define path->string
  (guard-proc 'path->string
              _path->string
              (list 'path)
              (list path?)))

;;; Procedure:
;;;   image-paths->string
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   return a string that is a full XML document that represents all paths belonging to image
;;; Produces:
;;;   paths-string, a string
;;; Preconditions:
;;;   image contains at least one path
;;; Postconditions:
;;;   svg-string is %NUL-terminated and can be used to reproduce all paths belonging to image

(define _image-paths->string
  (lambda (image)
    (car (gimp-vectors-export-to-string image
                                        0))))

(define image-paths->string
  (lambda (image)
    (cond ((not (image? image))
           (error "image-paths->string: image must be a valid image, given " image))
          ((= 0 (car (gimp-image-get-vectors image)))
           (error "image must contain at least one path."))
          (else
           (_image-paths->string image)))))


;;; Procedure:
;;;   image-string->path
;;; Parameters:
;;;   image, an image
;;;   svg-string, a string
;;; Purpose:
;;;   load the path(s) described by svg-string into image
;;; Produces:
;;;   string-path, a path/multiple paths
;;; Preconditions:
;;;   svg-string contains a full xml document
;;; Postconditions:
;;;   string-path exists on image
;;;   if svg-string describes multiple paths, they are loaded as separate paths
;;;   if string-path is/are larger or smaller than image, it/they will not scale to the image size
;;;   string-path is/are set to visible

(define _image-string->path
  (lambda (image svg-string)
    (let ((string-path (cadr (gimp-vectors-import-from-string image
                                                              svg-string
                                                              (string-length svg-string)
                                                              FALSE
                                                              FALSE))))
      (auto-rename-paths svg-path)
      (vector-map path-show! string-path)
      string-path)))

(define image-string->path
  (guard-proc 'image-string->path
              _image-string->path
              (list 'image 'string)
              (list image? string?)))

;;; Procedure:
;;;   image-scaled-string->path
;;; Parameters:
;;;   image, an image
;;;   svg-string, a string
;;; Purpose:
;;;   load the path(s) described by svg-string into image
;;; Produces:
;;;   string-path, a path/multiple paths
;;; Preconditions:
;;;   svg-string contains a full xml document
;;; Postconditions:
;;;   string-path exists on image
;;;   if svg-string describes multiple paths, they are loaded as separate paths
;;;   if string-path is/are larger or smaller than image, it/they will scale to the image size
;;;   string-path is/are set to visible

(define _image-scaled-string->path
  (lambda (image svg-string)
    (let ((string-path (cadr (gimp-vectors-import-from-string image
                                                        svg-string
                                                        (string-length svg-string)
                                                        FALSE
                                                        TRUE))))
      (auto-rename-paths svg-path)
      (vector-map path-show! string-path)
      string-path)))

(define image-scaled-string->path
  (guard-proc 'image-scaled-string->path
              _image-scaled-string->path
              (list 'image 'string)
              (list image? string?)))

;;; Procedure:
;;;   auto-rename-path
;;; Parameters:
;;;   path, a path id
;;; Purpose:
;;;   This is a helper procedure for procedures that create copies of paths and/orload them into different images 
;;;   It renames those paths so that they have unique names 
;;      for their current session.
;;; Produces:
;;;   Nothing; Called for side effects.
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   The path now has a unique name for the session
;;;   This name follows the template [image-id]-[instance]-[path-name], where instance is 
;;;     the number of times loaded-paths have been loaded before the current set
;;;   Note that images may have different ids in different sessions (i.e., if image 2 is saved and 
;;;    reopened first in another session, it is now image 1). This means that if you load a path 
;;;    that exists in the new image 1 into a new image 2, the instance will be greater than 0.

(define auto-rename-path
  (lambda (path image)
    (letrec (
             (path-name (get-path-name path))
             (already-prefixed? (if (and (char-numeric? (string-ref path-name 0))
                                         (char=? #\- (string-ref path-name 1))
                                         (char-numeric? (string-ref path-name 2))
                                         (char=? #\- (string-ref path-name 3)))
                                    #t
                                    #f))
             (temp-name (if already-prefixed?
                            (substring path-name 4 (string-length path-name))
                            path-name))
             (prefix (string-append (number->string image) "-"))
             (get-instance 
              (lambda (instance)
                (if (member? (string-append prefix
                                            (number->string instance)
                                            "-"
                                            temp-name)
                             (context-list-paths))
                    (get-instance (+ 1 instance))
                    (string-append (number->string instance) "-"))))
             )
      (path-rename! path (string-append prefix 
                                        (get-instance 0) 
                                        temp-name)))))

;;; Procedure:
;;;   auto-rename-paths
;;; Parameters:
;;;   paths, a vector of path ids
;;; Purpose:
;;;   give a unique name to each path in paths
;;; Produces:
;;;   Nothing; called for side effects
;;; Preconditions:
;;;   None
;;; Postconditions:
;;;   Each element of paths now has a unique name
;;;   This name follows the template [image-id]-[instance]-[path-name], where instance is 
;;;     the number of times loaded-paths have been loaded to the image before the current set
;;;   Note that images may have different ids in different sessions (i.e., if image 2 is saved and 
;;;    reopened first in another session, it is now image 1). This means that if you load a path 
;;;    that exists in the new image 1 into a new image 2, the instance will be greater than 0.

(define auto-rename-paths
  (lambda (paths)
    (vector-map (lambda (path) (auto-rename-path path (path-get-image path)))
                paths)))

;;; Procedure:
;;;   path-duplicate
;;; Parameters:
;;;   path, a path
;;; Purpose:
;;;   create a new path that contains the same beziers as path
;;; Produces:
;;;   duplicated-path, a path
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   

(define _path-duplicate
  (lambda (path)
    (let* ((image (path-get-image path))
          (duplicated-path (car (gimp-vectors-copy (path-id path))))
          (current-name (get-path-name duplicated-path)))
      (_path-rename! duplicated-path (substring current-name 0 (- (string-length current-name) 5)))
      (auto-rename-path duplicated-path image)
      (gimp-image-add-vectors image duplicated-path (path-get-position path))
      duplicated-path)))

(define path-duplicate
  (guard-proc 'path-duplicate
              _path-duplicate
              (list 'path)
              (list path?)))

;;; Procedure:
;;;   path-bezier-duplicate
;;; Parameters:
;;;   path, a path
;;;   bezier, a bezier id
;;; Purpose:
;;;   create a new path that contains only bezier
;;; Produces:
;;;   bezier-path, a path
;;; Preconditions:
;;;   path is a valid path name or id
;;;   bezier is a valid id NOTE: THIS IS NOT CHECKED IN THE GUARD-PROC VERSION 
;;; Postconditions:
;;;   bezier-path is named [image-id]-[instance]-(get-path-name path)-bezier-[bezier-id], where instance is 
;;;     the number of times bezier has been previously duplicated in the image.
(define _path-bezier-duplicate
  (lambda (path bezier)
    (let* (
           (temp-name (string-append (get-path-name path) "-bezier-" (number->string bezier)))
           (image (path-get-image path))
           (bezier-path (_image-path-new image temp-name))
           (bezier-info (gimp-vectors-stroke-get-points (path-id path) bezier))
           )
      (auto-rename-path bezier-path image)
      (gimp-vectors-stroke-new-from-points bezier-path 
                                           0
                                           (cadr bezier-info)   ; number of control points
                                           (caddr bezier-info)  ; control points
                                           (cadddr bezier-info)); closed?
      bezier-path)))

(define path-bezier-duplicate
  (guard-proc 'path-bezier-duplicate
              _path-bezier-duplicate
              (list 'path 'integer)
              (list path? integer?)))

;;; Procedure:
;;;   path-bezier-duplicate! 
;;; Parameters:
;;;   path, a path
;;;   bezier, a bezier id
;;; Purpose:
;;;   create a new bezier on path that shares the same control points as the given bezier
;;; Produces:
;;;   bezier-copy, a bezier
;;; Preconditions:
;;;   bezier is a valid id NOTE: THIS IS NOT CHECKED IN THE GUARD-PROC VERSION 
;;; Postconditions:
;;;   bezier-copy shares the same control points as bezier 
;;;     and has the id (+ 1 largest-bezier-id-before-procedure).

(define _path-bezier-duplicate!
  (lambda (path bezier)
    (let*(
          (image (path-get-image path))
          (bezier-info (gimp-vectors-stroke-get-points (path-id path) bezier))
          )
      (car (gimp-vectors-stroke-new-from-points (path-id path) 
                                           0
                                           (cadr bezier-info)   ; number of control points
                                           (caddr bezier-info)  ; control points
                                           (cadddr bezier-info))); closed?
      )))

(define path-bezier-duplicate!
  (guard-proc 'path-bezier-duplicate!
              _path-bezier-duplicate!
              (list 'path 'integer)
              (list path? integer?)))

;;; Procedure:
;;;   image-text->path NOTE: THIS DOESN'T SEEM TO WORK ANYMORE AS OF 11/15/2012
;;; Parameters:
;;;   image, an image
;;;   str, a string
;;;   font, a string
;;;   size, a positive number
;;;   top-left, a position
;;; Purpose:
;;;   Creates a new path containing beziers that form the text of str in the given font and size,
;;;    with the top left corner of the text at topleft
;;; Produces:
;;;   text-path, a path 
;;; Preconditions:
;;;   font is the name of a font recognized in the GIMP
;;; Postconditions:
;;;   text-path is named 

(define _image-text->path
  (lambda (image str font size top-left)
    (let* (
           (text-layer (car (gimp-text-fontname image
                                                -1 ;we want a new layer
                                                (position-col top-left)
                                                (position-row top-left)
                                                str
                                                0 ;no border
                                                TRUE ;antialiasing on
                                                size
                                                0 ; size refers to pixels
                                                font)))
           (text-path (car (gimp-vectors-new-from-text-layer image
                                                             text-layer)))
           )
      (gimp-image-add-vectors image text-path -1)
      (gimp-image-remove-layer 1 text-layer)
      (auto-rename-path text-path image))))

(define image-text->path
  (guard-proc 'image-text->path
              _image-text->path
              (list 'image 'string 'font 'integer 'position)
              (list image? string? font? integer? position?)))
