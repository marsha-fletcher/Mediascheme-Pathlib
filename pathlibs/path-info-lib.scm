;;;Path Procedures for getting or checking information (context and predicates)

;;; Procedure:
;;;   image-list-path-ids
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Lists the IDs for all path drawables in an image
;;; Produces:
;;;   path-list, a list of integers
;;; Preconditions:
;;;   none 

(define _image-list-path-ids
  (lambda (image)
    (vector->list (cadr (gimp-image-get-vectors image)))))

(define image-list-path-ids
  (guard-proc 'image-list-path-ids
              _image-list-path-ids
              (list 'image)
              (list image?)))

;;; Procedure:
;;;   context-list-path-ids
;;; Parameters:
;;;   none
;;; Purpose:
;;;   Lists the ids of all paths in all open images
;;; Produces:
;;;   path-ids, a list
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   path-ids is null if no images are open or no paths exist

(define context-list-path-ids
  (lambda ()
    (let helper ((path-list null)
                 (image-list (vector->list (cadr (gimp-image-list)))))
      (if (null? image-list)
          path-list
          (helper (append path-list
                          (image-list-path-ids (car image-list)))
                  (cdr image-list))))))

;;; Procedure:
;;;   path-id->path-name
;;; Parameters:
;;;   id, an int
;;; Purpose:
;;;   Return the name of the given path
;;; Produces:
;;;   path-name, a path name (string)
;;; Preconditions:
;;;   id is a valid path   

(define path-id->path-name
  (lambda (id)
    (car (gimp-vectors-get-name id))))

;;; Procedure:
;;;   path-get-image
;;; Parameters:
;;;   path
;;; Purpose:
;;;   return the image that path belongs to
;;; Produces:
;;;   image, an image
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   

(define path-get-image
  (lambda (path)
    (car (gimp-vectors-get-image (path-id path)))))

;;; Procedure:
;;;   image-list-paths
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   lists the names of all paths in image
;;; Produces:
;;;   path-list, a list of strings
;;; Preconditions:
;;;   none  

(define _image-list-paths
  (lambda (image)
    (map path-id->path-name (image-list-path-ids image))))

(define image-list-paths
  (guard-proc 'image-list-paths
              _image-list-paths
              (list 'image)
              (list image?)))

;;; Procedure:
;;;   context-list-paths
;;; Parameters:
;;;   none
;;; Purpose:
;;;   Lists the names of all paths in all open images
;;; Produces:
;;;   paths, a list
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   paths is null if no images are open or no paths exist

(define context-list-paths
  (lambda ()
    (map path-id->path-name (context-list-path-ids))))

;;; Procedure:
;;;   path-name?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine if val is a valid path name
;;; Produces:
;;;   is-path-name? a Boolean
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   if val is the name of a path in an open image, is-path-name? is true (#t)
;;;   if val is not the name of a path in an open image, is-path-name? is false (#f)


(define path-name?
  (lambda (val)
    (and (string? val) 
         (member? val (context-list-paths)))))

;;; Procedure:
;;;   path-name->path-id
;;; Parameters:
;;;   name, a string
;;; Purpose:
;;;   To return the id of the path with the given name
;;; Produces:
;;;   path-id, an int (also a path)
;;; Preconditions:
;;;   name is the name of a valid path
;;; Postconditions:
;;;   If more than one open image has a path named "name," 
;;;    the id of the most recently created path is returned

;;based on image-name->image-id in gimplib.scm
(define path-name->path-id
  (lambda (name)
    (and (string? name)
         (let ((paths (list->vector (context-list-path-ids)))
               (len (length (context-list-path-ids))))
           (let kernel ((pos 0))
             (cond
               ((= pos len) #f)
               ((equal? name (path-id->path-name (vector-ref paths pos)))
                (vector-ref paths pos))
               (else (kernel (+ pos 1)))))))))

;;; Procedure:
;;;   path?
;;; Parameters:
;;;   val, a scheme value
;;; Purpose:
;;;   Determine if val can refer to a path in an open image.
;;;   is-path?  a Boolean
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   if val is a valid path name or id, is-path? is true (#t)
;;;   if val is not a valid path name or id, is-path? is false (#f)


(define path?
  (lambda (val)
    (if (or (and (string? val)
                 (path-name? val))
            (and (integer? val) (= 1 (car (gimp-vectors-is-valid val)))))
        #t
        (error "path must be a valid path name or id, given " val))))

;;; Procedure:
;;;   path-id
;;; Parameters:
;;;   path, a path
;;; Purpose:
;;;   to return the id number of the path
;;; Produces:
;;;   path-id, the int that refers to path
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   if path is an int, path-id = path
;;;   if path is a string, path-id = (path-name->path-id path)
;;; Philosophy
;;;   All GIMP PDB procedures require the path-id. This helper procedure allows 
;;;   wrappers to take path names as well as ids without requiring the coder to 
;;;   put the same conditional statement in each wrapper.

(define _path-id
  (lambda (path)
    (if (string? path)
        (path-name->path-id path)
        path)))

(define path-id
  (guard-proc 'path-id
              _path-id
              (list 'path)
              (list path?)))

;;; Procedure:
;;;   path-name
;;; Parameters:
;;;   path, a path
;;; Purpose:
;;;   to return the name of the path
;;; Produces:
;;;   path-name, the string that refers to path
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   if path is an int, path-name = (path-id->path-name path)
;;;   if path is a string, path-name = path 

(define _get-path-name
  (lambda (path)
    (if (string? path)
        path
        (path-id->path-name path))))

(define get-path-name
  (guard-proc 'get-path-name
              _get-path-name
              (list 'path)
              (list path?)))

;;; Procedure:
;;;   path-name-free?
;;; Parameters:
;;;   val, a scheme value
;;; Purpose:
;;;   Determines if val can be used as a path name
;;; Produces:
;;;   is-free?, a Boolean if true or an error if false
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   If val is a string and is not already the name of a path in an open image, is-free? is true (#t)
;;;   Otherwise, is-free? is false and returns an error

(define path-name-free?
  (lambda (val)
    (if (and (string? val)
             (not (path-name? val)))
        #t
        (if (not (string? val))
            (error "name must be a string")
            (error "name may not be the same as an existing path in any open image, given " val)))))

;;; Procedure:
;;;   path-get-position!
;;; Parameters:
;;;   path, a path
;;; Purpose:
;;;   return the position of path in the paths list of the image it belongs to
;;; Produces:
;;;   position, an int
;;; Preconditions:
;;;   path belongs to an image
;;; Postconditions:
;;;   if path is at the top of the list, position = 0

(define _path-get-position
  (lambda (path)
    (car (gimp-image-get-vectors-position (path-get-image path)
                                          (path-id path)))))

(define path-get-position
  (guard-proc 'path-get-position
              _path-get-position
              (list 'path)
              (list path?)))

;;; Procedure:
;;;   path-list-beziers
;;; Parameters:
;;;   path, a path
;;; Purpose:
;;;   Lists the ids of all beziers that exist in path
;;; Produces:
;;;   bezier-list, a list
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   bezier-list is null if path contains no beziers

(define path-list-beziers
  (lambda (path)
    (vector->list (cadr (gimp-vectors-get-strokes (path-id path))))))

;;; Procedure:
;;;   path-valid-bezier?
;;; Parameters:
;;;   path, a path
;;;   val, a Scheme value
;;; Purpose:
;;;   Determines if val is the id of a list in path
;;; Produces:
;;;   is-valid? a Boolean
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   if val can refer to a bezier in path, is-valid? is true (#t)
;;;   if val can not refer to a bezier in path, is-valid? is false (#f)

(define path-valid-bezier?
  (lambda (path val)
    (if (and (integer? val) (member? val (path-list-beziers path)))
        #t
        #f)))

;;; Procedure:
;;;   path-bezier-get-last-anchor
;;; Parameters:
;;;   path, a path
;;;   bezier, a bezier in path
;;; Purpose:
;;;   return the position ofthe last anchor point in bezier
;;; Produces:
;;;   last-anchor, a position
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   last-anchor is always the most recently created anchor point. 
;;;     If the bezier is closed, last-anchor is the anchor that connects to the beginning

(define path-bezier-get-last-anchor
  (lambda (path bezier)
    (let* ((bezier-information (gimp-vectors-stroke-get-points (path-id path) bezier))
           (control-points (caddr bezier-information)))
      (position (vector-ref control-points 2)
                (vector-ref control-points 3)))))

;;; Procedure:
;;;   path-bezier-open?
;;; Parameters:
;;;   path, a path
;;;   bezier, a bezier
;;; Purpose:
;;;   determine if the bezier is open
;;; Produces:
;;;   bezier-is-open?, a boolean
;;; Preconditions:
;;;   
;;; Postconditions:
;;;   if the bezier is open, bezier-is-open? is true (#t)
;;;   if the bezier is closed, bezier-is-open? is false (#f)

(define _path-bezier-open?
  (lambda (path bezier)
    (if (= 0 (cadddr (gimp-vectors-stroke-get-points (path-id path) bezier)))
        #t
        #f)))

(define path-bezier-open?
  (lambda (path bezier)
    (cond ((not (path? path))
           (error "path-bezier-open?: path must be a valid path id or name, given " path))
          ((not (path-valid-bezier? path bezier))
           (error "path-bezier-open?: bezier is not a valid bezier id on the given path. Valid beziers ids for the given path are " (path-list-beziers path)))
          (else
           (_path-bezier-open? path bezier)))))

;;; Procedure:
;;;   position-list?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine if val is a list and if so, if all elements can be interpreted as positions
;;; Produces:
;;;   all-positions?, a boolean if true and an error message if false
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   if val is list and all positions can be interpreted as positions, all-positions? is true (#t)
;;;     else, all-positions? is false and returns an error

(define position-list?
  (lambda (val)
    (if (and (not (null? val)) (list? val) (all position? val))
        #t
        (error "expected a non-empty list of positions, given " val))))

;;; Procedure:
;;;   bezier-position?
;;; Parameters:
;;;   val, a scheme value
;;; Purpose:
;;;   to determine is val can be interpreted as a bezier position
;;; Produces:
;;;   is-bezier-position?, a boolean if true and an error message if false
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   if val is a list of 6 and all elements are real numbers, is-bezier-position? is true (#t)
;;;      else, is-bezier-position? is false and returns an error

(define bezier-position? 
  (lambda (val)
    (if (and (list? val) (= 6 (length val)) (all real? val))
        #t
        (error "bezier positions must be a list of 6 real numbers, given " val))))

;;; Procedure:
;;;   bezier-position-list?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   determine if val is a list that only contains elements that can be interpreted as bezier positions
;;; Produces:
;;;   is-bezier-position-list?, a boolean if true and an error message if false
;;; Preconditions:
;;;   none
;;; Postconditions:
;;;   if val is a list of lists, and each element of val has length 6 and only contains reals:
;;;      is-bezier-position-list? is true (#t)
;;;      else, is-bezier-position? is false and returns an error message

(define bezier-position-list?
  (lambda (val)
    (if (and (not (null? val)) (list val) (all list val) (all bezier-position? null))
        #t
        (error "expected a non-empty list of bezier positions, given " val))))
