;(load "/home/fletcher1/pathlibs/path-lib.scm")

;;; Procedure:
;;;   paths-merge!
;;; Parameters:
;;;   path1, a path
;;;   path2, a path
;;; Purpose:
;;;   "merges" the two paths by creating copies of all path2 beziers in path1 and then deleting path2
;;; Produces:
;;;   nothing; called for preconditions
;;; Preconditions:
;;;   
;;; Postconditions:
;;;   the ids of the beziers from path2 are now [original id]+[number of beziers previously in path1]
;;;   path2 is deleted

(define _paths-merge!
  (lambda (path1 path2)
    (letrec (
             (path1-id (path-id path1))
             (path2-id (path-id path2))
             (path2-beziers (path-list-beziers path2))
             (transfer-beziers (lambda (remaining)
                                 (when (not (null? remaining))
                                   ;;copy bezier from path2 to path1
                                   (let ((bezier-info (gimp-vectors-stroke-get-points path2-id (car remaining))))
                                     (gimp-vectors-stroke-new-from-points path1-id 
                                                                          0
                                                                          (cadr bezier-info) 
                                                                          (caddr bezier-info)
                                                                          (cadddr bezier-info)) 
                                     (transfer-beziers (cdr remaining))))))
             )
      (transfer-beziers path2-beziers)
      (path-remove! path2))))

(define paths-merge!
  (guard-proc 'paths-merge!
              _paths-merge!
              (list 'path 'path)
              (list path? path?)))

;;; Procedure:
;;;   image-merge-visible-paths!
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   merges all visible paths into one path containing all beziers
;;; Produces:
;;;   merged-path, a new path
;;; Preconditions:
;;;   
;;; Postconditions:
;;;    The beziers in all visible paths are transferred to the visible path highest

(define _image-merge-visible-paths!
  (lambda (image)
    (letrec (
             (all-paths (image-list-path-ids image))
             (find-visible-paths (lambda (paths)
                                   (cond ((null? paths)
                                          null)
                                         ((path-visible? (car paths))
                                          (cons (car paths) (find-visible-paths (cdr paths))))
                                         (else
                                          (find-visible-paths (cdr paths))))))
             (all-visible (find-visible-paths all-paths))
             (first-visible (car all-visible))
             (to-merge (cdr all-visible))
             )
      (map (lambda (merge-path) (_paths-merge! first-visible merge-path)) to-merge))))

(define image-merge-visible-paths!
  (lambda (image)
    (cond ((not (image? image))
           (error "image-merge-visible-paths!: invalid image, given " image))
          ((not (paths-any-visible? (image-list-path-ids image)))
           (error "image-merge-visible-paths!: image has no visible paths to merge."))
          (else
           (_image-merge-visible-paths! image)))))

