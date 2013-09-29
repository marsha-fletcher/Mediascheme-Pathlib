(load "/home/fletcher1/pathlibs/path-lib.scm")

(define canvas (image-show (image-new 500 500)))
(define newPath (car (image-path-load canvas "/home/fletcher1/pathlibs/svgpaths/spiky.svg")))
(path-bezier-hoffset! newPath 1 200)
(path-bezier-voffset! newPath 1 150)

(repeat! 11 path-bezier-duplicate! newPath 1)

(for-each (lambda (bezier) 
            (path-bezier-rotate-around! newPath bezier (cons 250 250) (* 30 (- bezier 1))))
          (map (l-s + 2) (iota 11)))

;;context-update-displays!
