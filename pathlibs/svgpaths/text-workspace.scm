(load "/home/fletcher1/pathlibs/path-lib.scm")

(define canvas (image-show (image-new 500 500)))

(image-text->path canvas
                  "Hello, World!"
                  "Charter"
                  32
                  (position-new 0 0))
