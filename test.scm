;; Heap test
;; Test (lambda x x)
;; Test definition interne
;; Test quoted vector ex. '#(1 2 "Hello" ...)
;; Test set-car, set-cdr + Check err
;; Test bind define
;; Test appel terminal
;; Test eq? eqv? equal?
;; Test list-ref
;; Test map
;; Test reverse
;; Test for-each
;; Test ERROR
;; Ajouter port au GC
;; Test open-input-file, read-char, eof-object ?
;; Test print symbol, print eof
;; +++ Check-err de tout ça

;; Todo : ajouter print port quand input-port? et output-port? sont implémentées

(define (rch port)
  (let ((r (read-char port)))
    (println (eof-object? r))
    (pp (char? r))
    (pp r)))

(let ((f (open-input-file "in")))
  (println f)
  (pp (port? f))
  (println "-----")
  (rch f)
  (rch f)
  (rch f)
  (rch f)
  (rch f)
  (rch f)
  (rch f)
  (rch f)
  (rch f)
  (rch f))