;;---------------------------------------------------------------------------
;;
;;  Copyright (c) 2015, Baptiste Saleil. All rights reserved.
;;
;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions are
;;  met:
;;   1. Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;   2. Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in the
;;      documentation and/or other materials provided with the distribution.
;;   3. The name of the author may not be used to endorse or promote
;;      products derived from this software without specific prior written
;;      permission.
;;
;;  THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR IMPLIED
;;  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
;;  NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;  NOT LIMITED TO PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;;---------------------------------------------------------------------------

(include "config.scm")

(define regalloc-regs #f)
(define regalloc-fregs #f)
(define lazy-code-flags #f)
(define mem-allocated-kind #f)
(define live-out? #f)
(define copy-permanent #f)
(define perm-domain #f)
(define opt-entry-points #f)
(define opt-const-vers #f)
(define const-versioned? #f)
(define opt-propagate-continuation #f)
(define opt-static-mode #f)
(define opt-float-unboxing #f)
(define opt-int-unboxing #f)

(define asc-cnnum-ctx-get #f)
(define asc-cnnum-lco-get #f)
(define asc-fnnum-ctx-get #f)
(define asc-fnnum-lco-get #f)
(define asc-fnnum-nbargs-get #f)
(define gen-version-first #f)
(define sort #f)

;;-----------------------------------------------------------------------------
;; Ctx

;; Compilation context
(define-type ctx
  constructor: make-ctx*
  stack     ;; virtual stack of types
  slot-loc  ;; alist which associates a virtual stack slot to a location
  free-regs ;; list of current free virtual registers
  free-mems ;; list of current free memory slots
  free-fregs ;; list of current free virtual float registers
  free-fmems ;; list of current free virtual float memory slots
  env       ;; alist which associates a variable symbol to an identifier object
  nb-actual ;;
  nb-args   ;; number of arguments of function of the current stack frame
  fs        ;; current frame size
  ffs       ;; curent float stack frame size
  fn-num    ;; fn-num of current function
)

;; check for alpha conversion:
;; Check that every id is unique when creating a ctx
(define (contains-dbl ids)
  (if (null? ids)
      #f
      (if (member (car ids) (cdr ids))
          (car ids)
          (contains-dbl (cdr ids)))))

(define (make-ctx stack slot-loc free-regs free-mems free-fregs free-fmems env nb-actual nb-args fs ffs fn-num)

  ;; Check alpha conversion
  ;; All operations on ctx must create a ctx with distinct variable names
  (assert
    (let* ((local-ids (map car env))
           (r         (contains-dbl local-ids)))
      (or (not r)
          (begin (println "WIP: alpha conversion needed for " r)
                 (pp local-ids)
                 #f)))
    "Internal error")

  (assert
    (let* ((used-fregs
            (foldr (lambda (sl r)
                     (if (and (cdr sl) (ctx-loc-is-fregister? (cdr sl)))
                         (cons (cdr sl) r)
                         r))
                   '()
                   slot-loc))
           (total (set-union used-fregs free-fregs)))
      (if (not (= (length total)
                  (length (ctx-init-free-fregs))))
          (begin
            (pp (make-ctx* stack slot-loc free-regs free-mems free-fregs free-fmems env nb-actual nb-args fs ffs fn-num))
            (error "CHECK FAILED")
            #f)
          #t))
    "Internal error")

  (let ((free-regs  (sort free-regs (lambda (a b) (< (cdr a) (cdr b)))))
        (free-mems  (sort free-mems (lambda (a b) (< (cdr a) (cdr b)))))
        (free-fregs (sort free-fregs (lambda (a b) (< (cdr a) (cdr b)))))
        (free-fmems (sort free-fmems (lambda (a b) (< (cdr a) (cdr b))))))

    (make-ctx* stack slot-loc free-regs free-mems free-fregs free-fmems env nb-actual nb-args fs ffs fn-num)))


(define (ctx-copy ctx #!optional stack slot-loc free-regs free-mems free-fregs free-fmems env nb-actual nb-args fs ffs fn-num)
  (make-ctx
    (or stack      (ctx-stack ctx))
    (or slot-loc   (ctx-slot-loc ctx))
    (or free-regs  (ctx-free-regs ctx))
    (or free-mems  (ctx-free-mems ctx))
    (or free-fregs (ctx-free-fregs ctx))
    (or free-fmems (ctx-free-fmems ctx))
    (or env        (ctx-env ctx))
    (or nb-actual  (ctx-nb-actual ctx))
    (or nb-args    (ctx-nb-args ctx))
    (or fs         (ctx-fs ctx))
    (or ffs        (ctx-ffs ctx))
    (or fn-num     (ctx-fn-num ctx))))

;; Return ctx that only contains regalloc information
(define (ctx-rm-regalloc ctx)
  (ctx-copy ctx #f 0 0 0 #f #f #f #f #f 0 0))

;; Generate initial free regs list
(define (ctx-init-free-regs)
  (build-list (length regalloc-regs) (lambda (i) (cons 'r i))))

;; Generate initial free fregs list
(define (ctx-init-free-fregs)
  (build-list (length regalloc-fregs) (lambda (i) (cons 'fr i))))

;; Create an empty context
(define (ctx-init)
  (make-ctx '()
            '()
            (ctx-init-free-regs)
            '()
            (ctx-init-free-fregs)
            '()
            '()
            #f
            -1
            0
            0
            #f))

;;---------------------------------------------------------------------------
;; META CTX TYPES

;; List of all registered tags
(define meta-tags '())
;; Table type symbol -> tags associated to this type
(define meta-type-tags (make-table))
;; Table type symbol -> dynamic tags associated to this type
(define meta-dynamic-type-tags (make-table))
;; Set the value of meta-(dynamic)-type-tags
(define (meta-type-tags-set table type-name tags)
  (assert (not (table-ref table type-name #f))
          "Internal error")
  (table-set! table type-name tags))
;; Ctx type utils
(define (ctx-make-type sym . dynamic-tags) (cons sym dynamic-tags))
(define (ctx-type-symbol type)   (car type))
(define (ctx-type-dtags type) (cdr type))
(define (ctx-type-tag type i) (list-ref type (+ i 1)))
(define (ctx-type-tag-set! type i val)
  (let loop ((i i) (lst (cdr type)))
    (if (= i 0)
        (set-car! lst val)
        (loop (- i 1) (cdr lst)))))
;; Generic type predicate
;; check if given tag exists in the set of tags associated to given type
(define (ctx-type-predicate tag)
  (lambda (type)
    (member tag (table-ref meta-type-tags (ctx-type-symbol type)))))
;; Generic type tag accessor (get)
;; check that given dynamic tag exists in the set of dynamic tags associated to given type
;; then, return the value associated to the tag from the type object
(define (ctx-type-accessor tag)
  (lambda (type)
    (let* ((dtags (table-ref meta-dynamic-type-tags (ctx-type-symbol type)))
           (idx (index-of tag dtags)))
      (assert idx "Internal error")
      (ctx-type-tag type idx))))
;; Generic type tag accessor (set)
;; check that given dynamic tag exists in the set of dynamic tags associated to given type
;; then, replace the value associated to the tag from the type object
(define (ctx-type-accessor! tag)
  (lambda (type val)
    (let* ((dtags (table-ref meta-dynamic-type-tags (ctx-type-symbol type)))
           (idx (index-of tag dtags)))
      (assert idx "Internal error")
      (ctx-type-tag-set! type idx val))))
;;---------------------------------------------------------------------------

;; Register type tags
(define-macro (meta-add-tags . tags)

  ;; Example with the call (meta-add-tags (cst)):
  ;;   (define ctx-type-cst? (ctx-type-predicate 'cst))
  ;;   (define ctx-type-cst  (ctx-type-accessor 'cst))
  ;;   (set! meta-tags '(cst))

  ;; pref is a str prefix, suf is a list of str suffix
  ;; Add prefix and suffix to the symbol 'sym' and return the new symbol
  (define (format-sym pref sym . suf)
    (let* ((sym-str (symbol->string sym))
           (str (string-append pref sym-str (apply string-append suf))))
      (string->symbol str)))

  ;; Generate predicate and accessor for each tag
  (define (generate-pred-accs tags)
    (if (null? tags)
        '()
        (let ((sym-pred (format-sym "ctx-type-" (car tags) "?"))
              (sym-accs (format-sym "ctx-type-" (car tags)))
              (sym-accss (format-sym "ctx-type-" (car tags) "-set!")))
          (append `((define ,sym-pred  (ctx-type-predicate ',(car tags)))
                    (define ,sym-accs  (ctx-type-accessor  ',(car tags)))
                    (define ,sym-accss (ctx-type-accessor! ',(car tags))))
                   (generate-pred-accs (cdr tags))))))

  `(begin
     ,@(generate-pred-accs tags)
     (set! meta-tags ',tags)))

;;---------------------------------------------------------------------------

;; Register a new type with a name, a list of tags and a list of dynamic (instantiated tags)
(define-macro (meta-add-type name tags dynamic-tags)

  ;; Example with the call (meta-add-type chac (cha cst) (cst)):
  ;;   (assert (null? (keep (lambda (el) (not (member el  meta-tags))) '(cha cst))) "Internal error")
  ;;   (assert (null? (keep (lambda (el) (not (member el '(cha cst)))) '(cst))) "Internal error")
  ;;   (meta-type-tags-set meta-type-tags 'chac '(cha cst))
  ;;   (meta-type-tags-set meta-dynamic-type-tags 'chac '(cst))
  ;;   (define make-ctx-tchac (lambda (cst) (ctx-make-type 'chac cst)))

  ;; pref is a str prefix, suf is a list of str suffix
  ;; Add prefix and suffix to the symbol 'sym' and return the new symbol
  (define (format-sym pref sym . suf)
    (let* ((sym-str (symbol->string sym))
           (str (string-append pref sym-str (apply string-append suf))))
      (string->symbol str)))

  ;; Generate various assertions
  (define (generate-asserts)
    `(;; Check tags exist
      (assert (null? (keep (lambda (el) (not (member el meta-tags))) ',tags))
              "Internal error")
      ;; Check dynamic tags exist for this type
      (assert (null? (keep (lambda (el) (not (member el ',tags))) ',dynamic-tags))
              "Internal error")))

  ;; Generate constructor
  (define (generate-constructor)
    (let ((sym (format-sym "make-ctx-t" name)))
      `(define (,sym ,@dynamic-tags) (ctx-make-type ',name ,@dynamic-tags))))

  `(begin
     ;; generate assertions
     ,@(generate-asserts)
     ;; add static tags
     (meta-type-tags-set meta-type-tags         ',name ',tags)
     (meta-type-tags-set meta-dynamic-type-tags ',name ',dynamic-tags)
     ;; generate constructor
     ,(generate-constructor)))

;;---------------------------------------------------------------------------

(meta-add-tags
    ;; Type tags
    unk cha voi nul ret int boo box pai vec fec str sym ipo flo opo clo
    ;; Other tags
    cst id)

(meta-add-type unk (unk) ())
(meta-add-type cha (cha) ())
(meta-add-type voi (voi) ())
(meta-add-type nul (nul) ())
(meta-add-type ret (ret) ())
(meta-add-type int (int) ())
(meta-add-type boo (boo) ())
(meta-add-type box (box) ())
(meta-add-type pai (pai) ())
(meta-add-type vec (vec) ())
(meta-add-type fec (fec) ())
(meta-add-type str (str) ())
(meta-add-type sym (sym) ())
(meta-add-type ipo (ipo) ())
(meta-add-type flo (flo) ())
(meta-add-type opo (opo) ())
(meta-add-type clo (clo) ())

(meta-add-type chac (cha cst) (cst))
(meta-add-type nulc (nul cst) (cst))
(meta-add-type intc (int cst) (cst))
(meta-add-type booc (boo cst) (cst))
(meta-add-type paic (pai cst) (cst))
(meta-add-type vecc (vec cst) (cst))
(meta-add-type fecc (fec cst) (cst))
(meta-add-type strc (str cst) (cst))
(meta-add-type symc (sym cst) (cst))
(meta-add-type floc (flo cst) (cst))
(meta-add-type cloc (clo cst) (cst))
(meta-add-type retc (ret cst) (cst))

(meta-add-type cloi (clo cst id) (cst))

(define (ctx-type-mem-allocated? type)
  (or (ctx-type-box? type)
      (ctx-type-pai? type)
      (ctx-type-vec? type)
      (ctx-type-fec? type)
      (ctx-type-str? type)
      (ctx-type-sym? type)
      (ctx-type-ipo? type)
      (ctx-type-flo? type)
      (ctx-type-opo? type)
      (ctx-type-clo? type)))

(define (ctx-string->tpred str)
  (define (is s) (string=? s str))
  (cond
    ((is "cha") ctx-type-cha?)
    ((is "voi") ctx-type-voi?)
    ((is "nul") ctx-type-nul?)
    ((is "int") ctx-type-int?)
    ((is "boo") ctx-type-boo?)
    ((is "vec") ctx-type-vec?)
    ((is "fec") ctx-type-fec?)
    ((is "str") ctx-type-str?)
    ((is "sym") ctx-type-sym?)
    ((is "flo") ctx-type-flo?)
    ((is "pai") ctx-type-pai?)
    ((is "clo") ctx-type-clo?)
    (else (error "Internal error"))))

(define (ctx-type-ctor type)
  (cond
    ((ctx-type-cha? type) make-ctx-tcha)
    ((ctx-type-voi? type) make-ctx-tvoi)
    ((ctx-type-nul? type) make-ctx-tnul)
    ((ctx-type-int? type) make-ctx-tint)
    ((ctx-type-boo? type) make-ctx-tboo)
    ((ctx-type-vec? type) make-ctx-tvec)
    ((ctx-type-fec? type) make-ctx-tfec)
    ((ctx-type-str? type) make-ctx-tstr)
    ((ctx-type-sym? type) make-ctx-tsym)
    ((ctx-type-flo? type) make-ctx-tflo)
    ((ctx-type-pai? type) make-ctx-tpai)
    ((ctx-type-clo? type) make-ctx-tclo)
    ((ctx-type-ret? type) make-ctx-tret)
    (else (error "Internal error"))))

;; Check if two ctx-type objects represent the same type
(define (ctx-type-teq? t1 t2)
  (eq? (ctx-type-symbol t1)
       (ctx-type-symbol t2)))

;; Check if t1 'is-a' t2
;; (t1 has the tag associated to t2)
(define (ctx-type-is-a? t1 t2)
  (let ((pred (ctx-type-predicate (ctx-type-symbol t2))))
    (pred t1)))

;; Return a new type instance without any constant information
(define (ctx-type-nocst t)
  (if (ctx-type-cst? t)
      (let ((ctor (ctx-type-ctor t)))
        (ctor))
      t))

;; Build and return a ctx type from a literal
(define (literal->ctx-type l)
  (if (##bignum? l)
      (set! l (exact->inexact l)))
  (if (and (##mem-allocated? l)
           (not (eq? (mem-allocated-kind l) 'PERM)))
      (set! l (copy-permanent l #f perm-domain)))
  (cond
    ((char?    l)   (make-ctx-tchac l))
    ((null?    l)   (make-ctx-tnulc l))
    ((fixnum?  l)   (make-ctx-tintc l))
    ((boolean? l)   (make-ctx-tbooc l))
    ((pair?    l)   (make-ctx-tpaic l))
    ((vector?  l)   (make-ctx-tvecc l))
    ((f64vector? l) (make-ctx-tfecc l))
    ((string?  l)   (make-ctx-tstrc l))
    ((symbol?  l)   (make-ctx-tsymc l))
    ((flonum?  l)   (make-ctx-tfloc l))
    (else (pp l) (error "Internal error (literal->ctx-type)"))))

;; CTX IDENTIFIER LOC
;; Return best loc for identifier. (Register if available, memory otherwise)
(define (ctx-identifier-loc ctx identifier)

  (define (get-best-loc slot-loc sslots mloc)
    (if (null? slot-loc)
        mloc
        (let ((sl (car slot-loc)))
          (if (member (car sl) sslots)
              (if (ctx-loc-is-register? (cdr sl))
                  (cdr sl)
                  (get-best-loc (cdr slot-loc) sslots (cdr sl)))
              (get-best-loc (cdr slot-loc) sslots mloc)))))

  (let ((sslots (identifier-sslots identifier)))
    (if (null? sslots)
        ;; Free var
        (begin
          (assert (eq? (identifier-kind identifier) 'free) "Internal error (ctx-identifier-loc) 2")
          (identifier-cloc identifier))
        (get-best-loc (ctx-slot-loc ctx) sslots #f))))

;;
(define (ctx-fs-inc ctx)
  (ctx-copy ctx #f #f #f #f #f #f #f #f #f (+ (ctx-fs ctx) 1)))

(define (ctx-fs-update ctx fs)
  (ctx-copy ctx #f #f #f #f #f #f #f #f #f fs))

(define (ctx-reset-nb-actual ctx)
  (ctx-copy ctx #f #f #f #f #f #f #f (ctx-nb-args ctx)))

;; Init a stack for a call ctx or a fn ctx
;; This function removes the csts not used for versioning from 'stack'
(define (ctx-init-stack stack nels add-suffix?)
  (let ((nstack
          (let loop ((stack stack) (nels nels))
            (if (or (null? stack)
                    (and (number? nels) (= nels 0)))
                '()
                (let ((type (car stack)))
                  (if (and (ctx-type-cst? type)
                           (not (const-versioned? type)))
                      ;; it's a non versioned cst, remove it
                      (cons (ctx-type-nocst type) (loop (cdr stack) (and nels (- nels 1))))
                      (cons type (loop (cdr stack) (and nels (- nels 1))))))))))
    (if add-suffix?
        (append nstack (list (make-ctx-tclo) (make-ctx-tret)))
        stack)))

;;
;; CTX INIT CALL
(define (ctx-init-call ctx nb-args)
  (ctx-copy
    (ctx-init)
    (ctx-init-stack (ctx-stack ctx) nb-args #t)))

;;
;; CTX INIT RETURN
(define (ctx-init-return ctx)
  (let ((type (ctx-get-type ctx 0)))
    (if (const-versioned? type)
        type
        (ctx-type-nocst type))))

;;
;; GENERIC
(define (ctx-generic ctx)

  (define slot-loc  (ctx-slot-loc ctx))
  (define free-regs (ctx-free-regs ctx))
  (define free-mems (ctx-free-mems ctx))
  (define fs (ctx-fs ctx))
  (define stack #f)

  (define (change-loc slot-loc slot loc)
    (if (null? slot-loc)
        (error "Internal error")
        (let ((sl (car slot-loc)))
          (if (eq? (car sl) slot)
              (cons (cons slot loc) (cdr slot-loc))
              (cons sl (change-loc (cdr slot-loc) slot loc))))))

  (define (set-new-loc! slot)
    (cond ((not (null? free-regs))
             (set! slot-loc (change-loc slot-loc slot (car free-regs)))
             (set! free-regs (cdr free-regs)))
          ((not (null? free-mems))
             (set! slot-loc (change-loc slot-loc slot (car free-mems)))
             (set! free-mems (cdr free-mems)))
          (else
             (set! slot-loc (change-loc slot-loc slot (cons 'm fs)))
             (set! fs (+ fs 1)))))

  ;; TODO: merge three functions
  (define (get-new-loc)
    (cond ((not (null? free-regs))
             (let ((r (car free-regs)))
               (set! free-regs (cdr free-regs))
               r))
          ((not (null? free-mems))
             (let ((m (car free-mems)))
               (set! free-mems (cdr free-mems))
               m))
          (else
             (set! fs (+ fs 1))
             (cons 'm (- fs 1)))))

  (define (compute-stack stack slot)
    (if (null? stack)
        '()
        (let* ((first (car stack))
               (ntype
                 (if (and (ctx-type-cst? first)
                          (not (ctx-type-id? first)))
                     (let ((ident (ctx-ident-at ctx (slot-to-stack-idx ctx slot))))
                       (if (and ident (identifier-cst (cdr ident)))
                           ;; It's a cst associated to a cst identifier
                           first
                           ;; If stack type is cst, and not associated to a cst identifier, then change loc
                           (begin
                               (set-new-loc! slot)
                               (trigger-type-lost first)
                               (make-ctx-tunk))))
                     (begin
                       (trigger-type-lost first)
                       (make-ctx-tunk)))))
          (cons ntype (compute-stack (cdr stack) (- slot 1))))))

  (define (compute-env env)
    (if (null? env)
        '()
        (let ((first (car env)))
          (if (identifier-stype (cdr first))
              ;; there is a stype in identifier, it's a free variable
              (let ((new-stype (make-ctx-tunk)))
                (trigger-type-lost (identifier-stype (cdr first)))
                (assert (eq? (identifier-kind (cdr first)) 'free) "Internal error")
                (cons (cons (car first)
                            (identifier-copy (cdr first) #f '() #f new-stype))
                      (compute-env (cdr env))))
              ;; no stype in identifier
              (let ((sslots (identifier-sslots (cdr first))))
                (assert (eq? (identifier-kind (cdr first)) 'local) "Internal error")
                (if (null? sslots)
                    ;; If slots is null, it's a special case used in letrec (letrec uses tmp binding with no slot)
                    (cons first (compute-env (cdr env)))
                    (cons (cons (car first)
                                (identifier-copy (cdr first) #f (list (list-ref sslots (- (length sslots) 1)))))
                          (compute-env (cdr env)))))))))

  (define (compute-slot-loc slot-loc)
    (if (null? slot-loc)
        '()
        (let ((first (car slot-loc)))
          (if (or (ctx-loc-is-fregister? (cdr first))
                  (ctx-loc-is-fmemory?   (cdr first))
                  (and (cdr first)
                       (find (lambda (el) (eq? (cdr el) (cdr first)))
                             (cdr slot-loc))))
              ;; Loc is also used by at least one other slot
              (let ((loc (get-new-loc)))
                (cons (cons (car first) loc)
                      (compute-slot-loc (cdr slot-loc))))
              ;;
              (cons first
                    (compute-slot-loc (cdr slot-loc)))))))

  (set! stack (compute-stack (ctx-stack ctx) (- (length (ctx-stack ctx)) 1)))

  (let ((env   (compute-env   (ctx-env ctx)))
        (slot-loc (compute-slot-loc slot-loc)))

    (ctx-copy ctx stack slot-loc free-regs free-mems (ctx-init-free-fregs) '() env #f #f fs 0)))

;constructor: make-ctx*
;stack     ;; virtual stack of types
;slot-loc  ;; alist which associates a virtual stack slot to a location
;free-regs ;; list of current free virtual registers
;free-mems ;; list of current free memory slots
;free-fregs ;; list of current free virtual float registers
;free-fmems ;; list of current free virtual float memory slots
;env       ;; alist which associates a variable symbol to an identifier object
;nb-actual ;;
;nb-args   ;; number of arguments of function of the current stack frame
;fs        ;; current frame size
;ffs       ;; curent float stack frame size
;fn-num    ;; fn-num of current function

(define (ctx-init-*-stack nb-args cn-num free-const stack from-inlined-call?)
  ;; stack suffix (clo ret)
  (define stack-suffix
    (if cn-num
        (list (make-ctx-tclo) (make-ctx-tretc cn-num))
        (list (make-ctx-tclo) (make-ctx-tret))))
  ;; const free vars
  (define (init-const-stack free-const)
    (define (init free-const)
      (if (null? free-const)
          '()
          (cons (cdar free-const)
                (init (cdr free-const)))))
    (reverse (init free-const)))
  ;;
  (append (cond (from-inlined-call? stack)
                (stack (ctx-init-stack stack #f #f))
                (else (make-list nb-args (make-ctx-tunk))))
          (init-const-stack free-const)
          stack-suffix))

;;
;; SLOT-LOC
(define (ctx-init-*-slot-loc cn-num nb-free nb-free-const get-slot-loc-local #!optional cloloc inlined-tail?)

  (define cont (if cn-num '(0 . #f) '(0 m . 0)))
  (define clo
          (begin
            (assert (not (and (= (- nb-free nb-free-const) 0)
                              cloloc
                              (ctx-loc-is-memory? (car cloloc))))
                    "Internal error") ;; closure pushed but not set to a mem loc in ctx
            (cond ((= (- nb-free nb-free-const) 0) '(1 . #f))
                  (cloloc
                    (if (and (ctx-loc-is-memory? (car cloloc))
                             (not inlined-tail?))
                        (if (ctx-loc-is-memory? (cdr cont))
                            '(1 m . 1)
                            '(1 m . 0))
                        (cons 1 (car cloloc))))
                  (else '(1 r . 2)))))

  (define (init-slot-loc-base)
    (append (reverse (build-list nb-free-const (lambda (n) (cons (+ n 2) #f))))
            (list clo cont)))

  (let ((mem 0))
    (if (not cn-num)                   (set! mem (+ mem 1)))
    (if (ctx-loc-is-memory? (cdr clo)) (set! mem (+ mem 1)))

    (let* ((fargs-regs (ctx-init-free-fregs))
           (sl-base  (init-slot-loc-base))
           (sl-local (get-slot-loc-local (+ nb-free-const 2) args-regs fargs-regs mem)))
      (append sl-local sl-base))))

(define (ctx-init-*-free-regs slot-loc)
  (set-sub (ctx-init-free-regs)
           (map cdr slot-loc)))

(define (ctx-init-*-free-fregs slot-loc)
  (set-sub (ctx-init-free-fregs)
           (map cdr slot-loc)))

;;
;; ENV
(define (ctx-init-*-env args free-const free-nconst bound-id)
  ;; Create env for !const free vars
  (define (init-env-free ids nvar)
    (if (null? ids)
        '()
        (let* ((id         (caar ids))
               (enc-type   (if opt-entry-points ;; We can't use free variable type to specialize ctx if opt-entry-points is #f
                               (or (cdar ids) (make-ctx-tclo)) ;; enclosing type, or #f if late
                               (make-ctx-tunk)))
               (identifier (make-identifier 'free '() '() enc-type (cons 'f nvar) #f (eq? id bound-id))))
          (cons (cons id identifier)
                (init-env-free (cdr ids) (+ nvar 1))))))
  ;; Create env for const free vars
  (define (init-env-free-const free-const slot)
    (if (null? free-const)
        '()
        (let ((first (car free-const)))
          (cons (cons (car first)
                      (make-identifier 'local (list slot) '() #f #f #t (eq? (car first) bound-id)))
                (init-env-free-const (cdr free-const) (+ slot 1))))))
  ;; Create env for !free vars
  (define (init-env-local ids slot)
    (if (null? ids)
        '()
        (let* ((id (car ids))
               (identifier
                 (make-identifier
                   'local (list slot) '() #f #f #f #f)))
          (cons (cons id identifier)
                (init-env-local (cdr ids) (+ slot 1))))))
  ;;
  (let ((nb-free-const (length free-const)))
    (append (init-env-free free-nconst 0)
            (init-env-free-const free-const 2)
            (init-env-local args (+ nb-free-const 2)))))

(define (ctx-init-fn-inlined-tco cn-num call-ctx call-nb-args enclosing-ctx args free-vars late-fbinds fn-num bound-id)
  ;; TODO WIP MAKE IT GLOBAL FOR BOTH CTX_INIT_FN_FUNCTIONS
  ;; Separate constant and non constant free vars
  ;; Return a pair with const and nconst sets
  ;; const contains id and type of all constant free vars
  ;; nconst contains id and type of all non constant free vars
  (define (find-const-free ids)
    (let loop ((ids ids)
               (const '())
               (nconst '()))
      (if (null? ids)
          (cons const (reverse nconst)) ;; Order is important for non cst free variables!
          (let* ((id (car ids))
                 (late? (member id late-fbinds))
                 (enc-identifier (and (not late?) (cdr (ctx-ident enclosing-ctx id))))
                 (enc-type       (and (not late?) (ctx-identifier-type enclosing-ctx enc-identifier)))
                 (enc-loc        (and (not late?) (ctx-identifier-loc enclosing-ctx enc-identifier)))
                 (cst?           (and (not late?) (ctx-type-cst? enc-type) (not enc-loc))))
            ;; If an enclosing identifer is cst, type *must* represent a cst
            (assert (or (not enc-identifier)
                        (not (identifier-cst enc-identifier))
                        (and (identifier-cst enc-identifier) cst?))
                    "Internal error")
            (if cst?
                (loop (cdr ids) (cons (cons id enc-type) const) nconst)
                (loop (cdr ids) const (cons (cons id enc-type) nconst)))))))

  (define (get-stack free-const free-nconst)
    (let* ((arg-types (list-head (ctx-stack call-ctx) call-nb-args))
           (arg-clo (list-ref (ctx-stack call-ctx) call-nb-args)))
      (ctx-init-*-stack call-nb-args cn-num free-const arg-types #t)))

  (define (get-slot-loc free-const free-nconst)
    (define (init-slot-loc-local slot regs fregs mem)
      (let loop ((sidx 0) (slot (+ slot call-nb-args -1)))
        (if (= sidx call-nb-args)
            '()
            (let ((loc (ctx-get-loc call-ctx sidx)))
              (cons (cons slot loc)
                    (loop (+ sidx 1) (- slot 1)))))))
    (let ((lcloloc (list (ctx-get-loc call-ctx call-nb-args))))
      (ctx-init-*-slot-loc cn-num (length free-vars) (length free-const) init-slot-loc-local lcloloc #t)))


  (let* ((r (find-const-free free-vars))
         (free-const (car r))
         (free-nconst (cdr r))
         ;;
         (stack (get-stack free-const free-nconst))
         (slot-loc (get-slot-loc free-const free-nconst))
         (fs  (ctx-fs call-ctx))
         (ffs (ctx-ffs call-ctx))
         (free-regs (ctx-init-*-free-regs slot-loc))
         (free-mems (set-sub (build-list fs (lambda (m) (cons 'm m)))
                             (map cdr slot-loc)))
         (free-fregs (ctx-init-*-free-fregs slot-loc))
         (free-fmems (set-sub (build-list ffs (lambda (m) (cons 'fm m)))
                              (map cdr slot-loc)))
         (env (ctx-init-*-env args free-const free-nconst bound-id))
         (nb-actual  call-nb-args)
         (nb-args    (length args)))

  (make-ctx stack slot-loc free-regs free-mems free-fregs free-fmems env nb-actual nb-args fs ffs fn-num)))

;; Called when a clo or ret constant information is lost
(define type-lost-list '())
(define (trigger-type-lost type)
  (if (and opt-static-mode
           (ctx-type-cst? type)
           (or (ctx-type-ret? type)
               (ctx-type-clo? type)))
      (set! type-lost-list (cons type type-lost-list))))

(define (apply-types-lost)
  (for-each apply-type-lost type-lost-list)
  (set! type-lost-list '()))

(define (apply-type-lost type)

  (define (clo-lost fn-num)
    (let* ((r (asc-fnnum-ctx-get fn-num))
           (nb-ncst-free (length (list-ref r 3)))
           (lco (asc-fnnum-lco-get fn-num nb-ncst-free))
           (nbargs (asc-fnnum-nbargs-get fn-num)))
      ;; If nb-args is #f, the function has rest param.
      ;; Functions with rest param are not optimized using static mode info
      ;; then, no need to compute info for the function here.
      (if nbargs
          (let* ((stack (build-list nbargs (lambda (e) (make-ctx-tunk))))
                 (ctx (apply ctx-init-fn (cons #f (cons stack r)))))
            (gen-version-first lco ctx)))))

  (define (ret-lost cn-num)
    (let ((lco (asc-cnnum-lco-get cn-num))
          (ctx (asc-cnnum-ctx-get cn-num)))
      (gen-version-first lco (ctx-push ctx (make-ctx-tunk) return-reg))))

  (if (and opt-static-mode (ctx-type-cst? type))
      (cond ;;
            ((ctx-type-ret? type) (ret-lost (ctx-type-cst type)))
            ;;
            ((ctx-type-clo? type) (clo-lost (ctx-type-cst type))))))

(define (ctx-init-fn-inlined-ntco cn-num call-ctx call-nb-args enclosing-ctx args free-vars late-fbinds fn-num bound-id)

  ;; TODO WIP MAKE IT GLOBAL FOR BOTH CTX_INIT_FN_FUNCTIONS
  ;; Separate constant and non constant free vars
  ;; Return a pair with const and nconst sets
  ;; const contains id and type of all constant free vars
  ;; nconst contains id and type of all non constant free vars
  (define (find-const-free ids)
    (let loop ((ids ids)
               (const '())
               (nconst '()))
      (if (null? ids)
          (cons const (reverse nconst)) ;; Order is important for non cst free variables!
          (let* ((id (car ids))
                 (late? (member id late-fbinds))
                 (enc-identifier (and (not late?) (cdr (ctx-ident enclosing-ctx id))))
                 (enc-type       (and (not late?) (ctx-identifier-type enclosing-ctx enc-identifier)))
                 (enc-loc        (and (not late?) (ctx-identifier-loc enclosing-ctx enc-identifier)))
                 (cst?           (and (not late?) (ctx-type-cst? enc-type) (not enc-loc))))
            ;; If an enclosing identifer is cst, type *must* represent a cst
            (assert (or (not enc-identifier)
                        (not (identifier-cst enc-identifier))
                        (and (identifier-cst enc-identifier) cst?))
                    "Internal error")
            (if cst?
                (loop (cdr ids) (cons (cons id enc-type) const) nconst)
                (loop (cdr ids) const (cons (cons id enc-type) nconst)))))))

  (define (get-stack free-const free-nconst)
    (let* ((arg-types (list-head (ctx-stack call-ctx) call-nb-args))
           (arg-clo (list-ref (ctx-stack call-ctx) call-nb-args)))
      (ctx-init-*-stack call-nb-args cn-num free-const arg-types #t)))

  (define (get-slot-loc free-const free-nconst)
    (define (init-slot-loc-local slot regs fregs mem)
      (let loop ((sidx 0) (slot (+ slot call-nb-args -1)) (mem mem))
        (if (= sidx call-nb-args)
            '()
            (let ((loc (ctx-get-loc call-ctx sidx)))
              (cond ((ctx-loc-is-memory? loc)
                       (cons (cons slot (cons 'm mem))
                             (loop (+ sidx 1) (- slot 1) (+ mem 1))))
                    ((ctx-loc-is-fmemory? loc) (error "e2"))
                    (else (cons (cons slot loc)
                                (loop (+ sidx 1) (- slot 1) mem))))))))
    (let ((lcloloc (list (ctx-get-loc call-ctx call-nb-args))))
      (ctx-init-*-slot-loc cn-num (length free-vars) (length free-const) init-slot-loc-local lcloloc)))

  (let* ((r (find-const-free free-vars))
         (free-const (car r))
         (free-nconst (cdr r))
         ;;
         (stack (get-stack free-const free-nconst))
         (slot-loc (get-slot-loc free-const free-nconst))
         (free-regs (ctx-init-*-free-regs slot-loc))
         (fs (foldr (lambda (sl fs)
                      (if (ctx-loc-is-memory? (cdr sl))
                          (max (+ (cddr sl) 1) fs)
                          fs))
                    0
                    slot-loc))
         (free-mems '()) ;; TODO: implementer quand mem
         (free-fregs (ctx-init-*-free-fregs slot-loc))
         (free-fmems '()) ;; TODO: implementer quand fmem
         (env (ctx-init-*-env args free-const free-nconst bound-id))
         (nb-actual  call-nb-args)
         (nb-args    (length args))
         (ffs 0)) ;; TODO: implementer quand fmem

  (make-ctx stack slot-loc free-regs free-mems free-fregs free-fmems env nb-actual nb-args fs ffs fn-num)))


;;
;; CTX INIT FN INLINING
(define (ctx-init-fn-inlined tail? cn-num call-ctx call-nb-args enclosing-ctx args free-vars late-fbinds fn-num bound-id)

  ;; TODO WIP MAKE IT GLOBAL FOR BOTH CTX_INIT_FN_FUNCTIONS
  ;; Separate constant and non constant free vars
  ;; Return a pair with const and nconst sets
  ;; const contains id and type of all constant free vars
  ;; nconst contains id and type of all non constant free vars
  (define (find-const-free ids)
    (let loop ((ids ids)
               (const '())
               (nconst '()))
      (if (null? ids)
          (cons const (reverse nconst)) ;; Order is important for non cst free variables!
          (let* ((id (car ids))
                 (late? (member id late-fbinds))
                 (enc-identifier (and (not late?) (cdr (ctx-ident enclosing-ctx id))))
                 (enc-type       (and (not late?) (ctx-identifier-type enclosing-ctx enc-identifier)))
                 (enc-loc        (and (not late?) (ctx-identifier-loc enclosing-ctx enc-identifier)))
                 (cst?           (and (not late?) (ctx-type-cst? enc-type) (not enc-loc))))
            ;; If an enclosing identifer is cst, type *must* represent a cst
            (assert (or (not enc-identifier)
                        (not (identifier-cst enc-identifier))
                        (and (identifier-cst enc-identifier) cst?))
                    "Internal error")
            (if cst?
                (loop (cdr ids) (cons (cons id enc-type) const) nconst)
                (loop (cdr ids) const (cons (cons id enc-type) nconst)))))))

  (define (init-slot-loc-local slot regs fregs mem)
    (let loop ((n    call-nb-args)
               (sl   (ctx-slot-loc call-ctx))
               (slot (+ slot call-nb-args -1))
               (mem  mem))
      (if (= n 0)
          '()
          (let ((loc (if (ctx-loc-is-memory? (cdar sl))
                         (let ((r (cons 'm mem)))
                           (set! mem (+ mem 1))
                           r)
                         (cdar sl))))
            (cons (cons slot loc)
                  (loop (- n 1) (cdr sl) (- slot 1) mem))))))

  (define (init-slot-loc-local-tail slot regs fregs mem)
    (let ((csl (list-head (ctx-slot-loc call-ctx) call-nb-args)))
      (let loop ((csl csl) (slot (+ slot call-nb-args -1)))
        (if (null? csl)
            '()
            (let ((sl (car csl)))
              (cons (cons slot (cdr sl))
                    (loop (cdr csl) (- slot 1))))))))


  (let* ((r (find-const-free free-vars))
         (free-const (car r))
         (free-nconst (cdr r))
         (types (list-head (ctx-stack call-ctx) call-nb-args))
         ;;
         (stack      (ctx-init-*-stack #f cn-num free-const types #t))
         (clo-loc    (list (cdr (list-ref (ctx-slot-loc call-ctx) call-nb-args))))
         (slot-loc   (if tail?
                         (ctx-init-*-slot-loc cn-num (length free-vars) (length free-const) init-slot-loc-local-tail clo-loc)
                         (ctx-init-*-slot-loc cn-num (length free-vars) (length free-const) init-slot-loc-local clo-loc)))
         (free-regs  (ctx-init-*-free-regs slot-loc))
         (free-fregs (ctx-init-*-free-fregs slot-loc))
         (env        (ctx-init-*-env args free-const free-nconst bound-id))
         (nb-actual  call-nb-args)
         (nb-args    (length args))
         (fs         (if tail?
                         (ctx-fs call-ctx)
                         (foldr (lambda (sl fs)
                                  (if (ctx-loc-is-memory? (cdr sl))
                                      (max fs (+ (cddr sl) 1))
                                      fs))
                                0
                                slot-loc)))
         (fn-num     fn-num)
         ;;
         (ffs        (if tail?
                         (ctx-ffs call-ctx)
                         0))
         (free-mems  (set-sub (build-list  fs (lambda (n) (cons  'm n)))
                              (map cdr slot-loc)))
         (free-fmems (set-sub (build-list ffs (lambda (n) (cons 'fm n)))
                              (map cdr slot-loc))))

    (make-ctx stack slot-loc free-regs free-mems free-fregs free-fmems env nb-actual nb-args fs ffs fn-num)))


;; Separate constant and non constant free vars
;; Return a pair with const and nconst sets
;; const contains id and type of all constant free vars
;; nconst contains id and type of all non constant free vars
(define (find-const-free ids late-fbinds enclosing-ctx)
  (let loop ((ids ids)
             (const '())
             (nconst '()))
    (if (null? ids)
        (cons const (reverse nconst)) ;; Order is important for non cst free variables!
        (let* ((id (car ids))
               (late? (member id late-fbinds))
               (enc-identifier (and (not late?) (cdr (ctx-ident enclosing-ctx id))))
               (enc-type       (and (not late?) (ctx-identifier-type enclosing-ctx enc-identifier)))
               (enc-loc        (and (not late?) (ctx-identifier-loc enclosing-ctx enc-identifier)))
               (cst?           (and (not late?) (ctx-type-cst? enc-type) (not enc-loc))))
          ;; If an enclosing identifer is cst, type *must* represent a cst
          (assert (or (not enc-identifier)
                      (not (identifier-cst enc-identifier))
                      (and (identifier-cst enc-identifier) cst?))
                  "Internal error")
          (if cst?
              (loop (cdr ids) (cons (cons id enc-type) const) nconst)
              (loop (cdr ids) const (cons (cons id enc-type) nconst)))))))

;;
;; CTX INIT FN
(define (ctx-init-fn cn-num stack args free-vars free-const free-nconst fn-num bound-id)

  ;; pick the next available loc:
  ;; pick the first reg if regs != () or the next mem if regs == ()
  ;; return (loc updated-regs updated-mem)
  (define (next-loc regs mem)
    (if (null? regs)
        (list (cons 'm mem) '() (+ mem 1))
        (list (car regs) (cdr regs) mem)))

  ;; Init slot-loc set if the context is generic (stack is #f)
  ;; Return (slot-loc . mem)
  (define (init-slot-loc-local-gen slot regs fregs mem)
    (let loop ((slot slot) (regs regs) (mem mem) (args args))
      (if (null? args)
          '()
          (let* ((r (next-loc regs mem))
                 (loc (car r))
                 (regs (cadr r))
                 (mem (caddr r)))
            (cons (cons slot loc)
                  (loop (+ slot 1) regs mem (cdr args)))))))

  ;; Init slot-loc set if the context is not generic (stack is *not* #f)
  ;; Return (slot-loc . mem)
  (define (init-slot-loc-local slot regs fregs mem)

    (define (init slot types regs fregs mem)

      (define (case-no-loc)
        (let ((r (init (+ slot 1) (cdr types) regs fregs mem)))
          (cons `(,slot . #f) r)))

      (define (case-freg)
        (if (null? fregs) (error "NYI"))
        (let ((r (init (+ slot 1) (cdr types) regs (cdr fregs) mem)))
          (cons `(,slot . ,(car fregs)) r)))

      (define (case-loc)
        (let* ((r (next-loc regs mem))
               (loc (car r))
               (r (init (+ slot 1) (cdr types) (cadr r) fregs (caddr r))))
          (cons `(,slot . ,loc) r)))

      ;; init
      (if (null? types)
          '()
          (let ((type (car types)))
            (cond ((and (const-versioned? type)
                        (not (ctx-type-id? type)))
                     (case-no-loc))
                  ((and opt-float-unboxing
                        (ctx-type-flo? type))
                     (case-freg))
                  (else
                     (case-loc))))))
    ;;
    (let ((types (reverse stack)))
      (reverse (init slot types regs fregs mem))))

  (let* ((slot-loc (ctx-init-*-slot-loc
                     cn-num
                     (length free-vars)
                     (length free-const)
                     (if stack init-slot-loc-local init-slot-loc-local-gen)))
         (new-stack (ctx-init-*-stack (length args) cn-num free-const stack #f))
         (fs (count slot-loc
                    (lambda (el) (ctx-loc-is-memory? (cdr el))))))

    ;;
    (make-ctx
      new-stack
      slot-loc
      (ctx-init-*-free-regs slot-loc)
      '()
      (ctx-init-*-free-fregs slot-loc)
      '()
      (ctx-init-*-env args free-const free-nconst bound-id)
      (and stack (length stack))
      (length args)
      fs
      0
      fn-num)))

;; TODO WIP MOVE
(define (ctx-loc-used ctx loc . excluded-idx)
  (let loop ((sls (ctx-slot-loc ctx)))
    (cond ((null? sls)
             #f)
          ((and (eq? (cdar sls) loc)
                (not (member (slot-to-stack-idx ctx (car sls))
                             excluded-idx)))
             #f)
          (else
             (loop (cdr sls))))))

;;
;; GET FREE FREG
(define (ctx-get-free-freg ast ctx succ nb-opnds)

  (define deep-opnd-reg #f)
    ; (let loop ((idx (- (or nb-opnds 0) 1)))
    ;   (if (< idx 0)
    ;       #f
    ;       (let ((r (ctx-get-loc ctx idx)))
    ;         ;; We keep the the loc associated to this opnd if
    ;         ;; (i) it's a fregister and (ii) this fregister is not used elsewhere
    ;         (if (and (ctx-loc-is-fregister? r)
    ;                  (not (ctx-loc-used ctx idx)))
    ;             r
    ;             (loop (- idx 1)))))))

  (define (get-spilled-freg)
    (let ((sl
            (foldr (lambda (el r)
                     (if (and (or (not r) (< (car el) (car r)))
                              (ctx-loc-is-fregister? (cdr el)))
                         el
                         r))
                   #f
                   (ctx-slot-loc ctx))))
      (assert sl "Internal error (ctx-get-free-freg)")
      (cdr sl)))

  (if deep-opnd-reg
      (list '() deep-opnd-reg ctx)
      (let ((free-fregs (ctx-free-fregs ctx)))
        (if (null? free-fregs)
            (let* ((moves/mloc/ctx (ctx-get-free-mem ctx))
                   (moves (car moves/mloc/ctx))
                   (mloc  (cadr moves/mloc/ctx))
                   (ctx   (caddr moves/mloc/ctx))
                   (spill-freg (get-spilled-freg))
                   (reg-slots (ctx-get-slots ctx spill-freg)))
              ;; 1: changer tous les slots pour r -> m
              (let ((ctx (ctx-set-loc-n ctx reg-slots mloc))
                    (moves (append moves
                                   (list (cons spill-freg mloc)))))
                (list moves spill-freg ctx)))
            (let ((reg (car free-fregs)))
              (list '() reg ctx))))))

(define (ctx-add-to-free ctx reg)
  (ctx-copy ctx #f #f (cons reg (ctx-free-regs ctx))))

;;
;; GET FREE REG
(define (ctx-get-free-reg ast ctx succ nb-opnds)

  ;; TODO: prefer 'preferred' to 'deep-opnd-reg' ?

  ;; Preferred register is used if it's member of free registers
  ;; 'return-reg' register is preferred if the successor lco is a return lco
  ;; TODO: also use preferred register if a register need to be spilled
  (define preferred
    (if (and succ
             (member 'ret (lazy-code-flags succ)))
        return-reg
        #f))

  (define deep-opnd-reg #f)
    ; (let loop ((idx (- (or nb-opnds 0) 1)))
    ;   (if (< idx 0)
    ;       #f
    ;       (let ((r (ctx-get-loc ctx idx)))
    ;         ;; We keep the the loc associated to this opnd if
    ;         ;; (i) it's a register and (ii) this register is not used elsewhere
    ;         (if (and (ctx-loc-is-register? r)
    ;                  (not (ctx-loc-used ctx idx)))
    ;             r
    ;             (loop (- idx 1)))))))

  (define (get-spilled-reg)
    (let ((sl
            (foldr (lambda (el r)
                     (if (and (or (not r) (< (car el) (car r)))
                              (ctx-loc-is-register? (cdr el)))
                         el
                         r))
                   #f
                   (ctx-slot-loc ctx))))
      (assert sl "Internal error (ctx-get-free-reg)")
      (cdr sl)))

  (if deep-opnd-reg
      (list '() deep-opnd-reg ctx)
      (let ((free-regs (ctx-free-regs ctx)))
        (if (null? free-regs)
            (let* ((ctx (ctx-free-dead-locs ctx ast))
                   (moves/mloc/ctx (ctx-get-free-mem ctx))
                   (moves (car moves/mloc/ctx))
                   (mloc  (cadr moves/mloc/ctx))
                   (ctx   (caddr moves/mloc/ctx))
                   (spill-reg (get-spilled-reg))
                   (reg-slots (ctx-get-slots ctx spill-reg)))
              ;; 1: changer tous les slots pour r -> m
              (let ((ctx (ctx-set-loc-n ctx reg-slots mloc))
                    (moves (append moves
                                   (list (cons spill-reg mloc)))))
                (list moves spill-reg ctx)))
            (let* ((r (and preferred (member preferred free-regs)))
                   (reg (if r (car r) (car free-regs)))
                   (free (set-sub free-regs (list reg))))

              (list '()
                    reg
                    (ctx-copy ctx #f #f free)))))))

;;
;;
(define (ctx-get-eploc ctx id)

  (let ((r (assoc id (ctx-env ctx))))
    (if r
        ;; Const closure
        (let ((type (ctx-identifier-type ctx (cdr r))))
          (and (ctx-type-clo? type)
               (ctx-type-cst? type)
               (let ((loc (ctx-identifier-loc ctx (cdr r))))
                 (cons (not loc) (ctx-type-cst type)))))
        ;; This id
        (and r
             (identifier-thisid (cdr r))
             (let ((r (ctx-fn-num ctx)))
               (if r
                   (cons #f r)
                   #f))))))

;;
;; TODO: change to bind-top
(define (ctx-id-add-idx ctx id stack-idx)
  (define slot (stack-idx-to-slot ctx stack-idx))
  (define (build-env env)
    (if (null? env)
        (error "Internal error")
        (if (eq? (caar env) id)
            (cons (cons id
                        (identifier-copy (cdar env) #f (cons slot (identifier-sslots (cdar env)))))
                  (cdr env))
            (cons (car env)
                  (build-env (cdr env))))))

  ;; Add idx only if the id exists.
  ;; The id could have been removed using liveness info.
  (let ((r (assoc id (ctx-env ctx))))
    (if r
        (let ((env (build-env (ctx-env ctx))))
          (ctx-copy ctx #f #f #f #f #f #f env))
        ctx)))

;; Take a ctx and an s-expression (ast)
;; Remove all dead ids and free their locs (unbind)
;; Return new context
(define (ctx-free-dead-locs ctx ast)

  (define (in-stack? slots)
    (if (null? slots)
        #f
        (let ((slot (car slots)))
          (or (>= slot (+ 2 (ctx-nb-args ctx)))
              (in-stack? (cdr slots))))))

  (define (free-deads ctx env)
    (if (null? env)
        ctx
        (let ((id (caar env))
              (identifier (cdar env)))
          (if (and (not (live-out? id ast))
                   (not (in-stack? (identifier-sslots identifier))))
              ;; the id is dead and not in stack, we can unbind it
              (let ((nctx (ctx-unbind-locals ctx (list id))))
                (free-deads nctx (cdr env)))
              (free-deads ctx (cdr env))))))

  (let ((env (ctx-env ctx)))
    (free-deads ctx env)))

;;
;; BIND CONSTANTS
;; cst-set: ((id cst fn?) (id2 cst fn?) ...)
(define (ctx-bind-consts ctx cst-set cst-id?)

  (define (bind cst-set env stack slot-loc)
    (if (null? cst-set)
        (list env stack slot-loc)
        (let* ((info (car cst-set))
               (id   (car  info))
               (cst  (cadr info))
               (fn?  (caddr info))
               (type
                 (if fn?
                     (make-ctx-tcloc cst)
                     (literal->ctx-type cst)))
               (slot (length stack))
               (stack (cons type stack)))
          (bind (cdr cst-set)
                (cons (cons id
                            (make-identifier 'local (list slot) '() #f #f cst-id? #f))
                      env)
                stack
                (cons (cons slot #f) slot-loc)))))

  (let ((r (bind cst-set (ctx-env ctx) (ctx-stack ctx) (ctx-slot-loc ctx))))
   (ctx-copy ctx (cadr r) (caddr r) #f #f #f #f (car r))))

;;
;; BIND LOCALS
;; Do not bind ctx ids with ctx-bind-locals, use ctx-bind-consts instead
(define (ctx-bind-locals ctx id-idx #!optional letrec-bind?)

  (define (clean-env env bound-slots)
    (if (null? env)
        '()
        (let* ((ident (car env))
               (idslots (identifier-sslots (cdr ident))))
          (cons (cons (car ident)
                      (identifier-copy (cdr ident) #f (set-sub idslots bound-slots)))
                (clean-env (cdr env) bound-slots)))))

  (define (gen-env env id-idx)
    (if (null? id-idx)
        env
        (let ((first (car id-idx)))
          (cons (cons (car first)
                      (make-identifier
                        'local   ;; symbol 'free or 'local
                        (if letrec-bind?
                            '()
                            (list (stack-idx-to-slot ctx (cdr first))))
                        '()
                        #f
                        #f
                        #f
                        #f))
                (gen-env env (cdr id-idx))))))

  (let* ((env
           (if letrec-bind?
               (ctx-env ctx)
               (clean-env (ctx-env ctx)
                          (map (lambda (el) (stack-idx-to-slot ctx el))
                               (map cdr id-idx)))))
         (env
           (gen-env env id-idx)))

    (ctx-copy ctx #f #f #f #f #f #f env)))

;; This is one of the few ctx function with side effect!
;; The side effect is used to update letrec constant bindings
(define (ctx-cst-fnnum-set! ctx id fn-num)
  (let* ((r (assoc id (ctx-env ctx))))
    (assert (and r
                 (= (length (identifier-sslots (cdr r))) 1)
                 (ctx-type-cst? (ctx-identifier-type ctx (cdr r)))
                 (ctx-type-clo?       (ctx-identifier-type ctx (cdr r))))
            "Internal error (ctx-cst-fnnum-set!)")
    (let ((stype (ctx-identifier-type ctx (cdr r))))
      (ctx-type-cst-set! stype fn-num)
      ctx)))

(define (ctx-change-continuation ctx cn-num)
  (let ((nstack
          (let loop ((stack (ctx-stack ctx)))
            (assert (not (null? stack)) "Unexpected stack")
            (if (null? (cdr stack))
                (begin
                  (assert (ctx-type-ret? (car stack)) "Unexpected stack")
                  (list (make-ctx-tretc cn-num)))
                (cons (car stack)
                      (loop (cdr stack)))))))
    (if (ctx-get-loc ctx (- (length nstack) 1))
        (ctx-set-loc (ctx-copy ctx nstack) 0 #f)
        (ctx-copy ctx nstack))))

;;
;; UNBIND LOCALS
(define (ctx-unbind-locals ctx ids)

  (define (gen-env env ids)
    (if (null? env)
        '()
        (let ((ident (car env)))
          (if (member (car ident) ids)
              (gen-env (cdr env) (set-sub ids (list (car ident))))
              (cons ident
                    (gen-env (cdr env) ids))))))

  (ctx-copy ctx #f #f #f #f #f #f (gen-env (ctx-env ctx) ids)))

;;
;; IDENTIFIER TYPE
(define (ctx-identifier-type ctx identifier)
  (let ((stype (identifier-stype identifier)))
    (if stype
        (begin
          (assert (eq? (identifier-kind identifier) 'free)
                  "Internal error")
          stype)
        (let* ((sslots (identifier-sslots identifier))
               (sidx   (slot-to-stack-idx ctx (car sslots))))
          (list-ref (ctx-stack ctx) sidx)))))

;;
;; SAVE CALL
;; Called when compiling a call site.
;; Compute moves required to save registers.
;; Returns:
;;  moves: list of moves
;;  ctx: updated ctx
(define (ctx-save-call ast octx idx-start)

  ;; pour chaque slot sur la vstack:
    ;; Si le slot appartient à une variable, et que cette variable à déjà un emplacement mémoire:
      ;; on remplace simplement la loc dans le slot loc, aucun mouvement à générer
    ;; Sinon:
      ;; on récupère un emplacement mémoire vide
      ;; on remplace la loc dans slot-loc
      ;; on retourne le mouvement reg->mem

  (define (save-one curr-idx ctx)
    (let ((loc (ctx-get-loc ctx curr-idx)))
      (if (or (not loc)
              (ctx-loc-is-memory? loc)
              (ctx-loc-is-fmemory? loc))
          ;; If loc associated to current index is a memory loc, nothing to do
          (cons '() ctx)
          ;; Loc is a register, we need to save it
          (let* ((ident (ctx-ident-at ctx curr-idx))
                 (mloc  (and ident (ctx-ident-mloc ctx ident))))
            (if (and ident mloc)
                ;; This slot is associated to a variable, and this variable already has a memory location
                ;; Then, simply update slot-loc set
                (cons '()
                      (ctx-set-loc ctx (stack-idx-to-slot ctx curr-idx) mloc))
                ;; Else, we need a new memory slot
                (let* ((type (ctx-get-type ctx curr-idx))
                       (r (ctx-get-free-mem ctx))
                       (moves (car r))
                       (mem (cadr r))
                       (ctx (caddr r))
                       (ctx (ctx-set-loc ctx (stack-idx-to-slot ctx curr-idx) mem)))
                  ;; Remove all 'fs moves
                  (cons (append (set-sub (set-sub moves (list (assoc 'ffs moves)))
                                         (list (assoc 'fs moves)))
                                (list (cons loc mem)))
                        ctx)))))))


  (define (save-all curr-idx moves ctx)
    (if (= curr-idx (length (ctx-stack ctx)))
        (let ((nfs  (- (ctx-fs ctx) (ctx-fs octx)))
              (nffs (- (ctx-ffs ctx) (ctx-ffs octx))))
          (list (append (list (cons 'fs nfs)
                              (cons 'ffs nffs))
                        moves)
                ctx))
        (let ((r (save-one curr-idx ctx)))
          (save-all
            (+ curr-idx 1)
            (append moves (car r))
            (cdr r)))))

  ;; Free dead locations
  (set! octx (ctx-free-dead-locs octx ast))



  (save-all idx-start '() octx))

;;
(define (ctx-rm-unused-mems ctx)
  (let ((fmems (ctx-free-mems ctx)))
    (ctx-copy ctx #f #f #f '() #f #f #f #f #f (- (ctx-fs ctx) (length fmems)))))

;;
;; PUSH
(define (ctx-push ctx type loc #!optional id)

  (define (get-env env id slot)
    (if (null? env)
        '()
        (let ((ident (car env)))
          (if (eq? (car ident) id)
              (cons (cons id
                          (identifier-copy
                            (cdr ident)
                            #f
                            (cons slot (identifier-sslots (cdr ident)))))
                    (cdr env))
              (cons ident
                    (get-env (cdr env) id slot))))))

  ;; If loc is #f, type must be a constant
  (assert (if (not loc)
              (ctx-type-cst? type)
              #t)
          "Internal error")

  ;; We do *NOT* want non permanent constant in ctx
  (assert (not (and (ctx-type-cst? type)
                    (##mem-allocated? (ctx-type-cst type))
                    (not (eq? (mem-allocated-kind (ctx-type-cst type)) 'PERM))))
          "Internal error")

  (let* ((slot (length (ctx-stack ctx))))

   (ctx-copy
     ctx
     (cons type (ctx-stack ctx))
     (cons (cons slot loc)
           (ctx-slot-loc ctx))
     (set-sub (ctx-free-regs ctx) (list loc))
     (set-sub (ctx-free-mems ctx) (list loc))
     (set-sub (ctx-free-fregs ctx) (list loc))
     (set-sub (ctx-free-fmems ctx) (list loc))
     (if id
         (get-env (ctx-env ctx) id slot)
         #f)
     #f
     #f
     #f)))

;; TODO: move
;; Is loc 'loc' used in slot-loc set ?
(define (loc-used? loc slot-loc)
  (if (null? slot-loc)
      #f
      (or (equal? (cdar slot-loc) loc)
          (loc-used? loc (cdr slot-loc)))))

;;
;; POP-N
(define (ctx-pop-n ctx n)
  (if (= n 0)
      ctx
      (ctx-pop-n
        (ctx-pop ctx)
        (- n 1))))

;;
;; POP
(define (ctx-pop ctx)

  ;; If one of the positions of an identifier is given slot, remove this slot.
  ;; If this slot is the only position, remove the identifier
  (define (env-remove-slot env slot)
    (if (null? env)
        '()
        (let ((ident (car env)))
          (if (member slot (identifier-sslots (cdr ident)))
              (cons (cons (car ident)
                          (identifier-copy
                            (cdr ident)
                            #f
                            (set-sub (identifier-sslots (cdr ident)) (list slot))))
                    (env-remove-slot (cdr env) slot))
              (cons ident (env-remove-slot (cdr env) slot))))))

  ;;
  (let* ((slot (- (length (ctx-stack ctx)) 1))
         (r (assoc-remove slot (ctx-slot-loc ctx)))
         (loc (and (car r) (cdar r)))
         (slot-loc (cdr r)))

    (define (free-loc loc ctx-loc-is*? locs-set)
      (if (and loc
               (not (loc-used? loc slot-loc))
               (ctx-loc-is*? loc))
          (cons loc locs-set)
          locs-set))

    (ctx-copy
      ctx
      (cdr (ctx-stack ctx))                        ;; stack: remove top
      slot-loc                                     ;; slot-loc: remove popped slot
      (free-loc loc ctx-loc-is-register?  (ctx-free-regs ctx))
      (free-loc loc ctx-loc-is-memory?    (ctx-free-mems ctx))
      (free-loc loc ctx-loc-is-fregister? (ctx-free-fregs ctx))
      (free-loc loc ctx-loc-is-fmemory?   (ctx-free-fmems ctx))
      (env-remove-slot (ctx-env ctx) slot))))      ;; env: remove popped slot from env

(define (ctx-const-continuation? ctx)
  (and (> (length (ctx-stack ctx)) 0)
       (let ((type (ctx-get-type ctx (- (length (ctx-stack ctx)) 1))))
         (and (ctx-fn-num ctx)
              (ctx-type-cst? type)))))

(define (ctx-const-continuation ctx)
  (assert (ctx-const-continuation? ctx) "Internal error")
  (let ((type (ctx-get-type ctx (- (length (ctx-stack ctx)) 1))))
    (ctx-type-cst type)))

;; Check if given stack idx belongs to an id
;; If so, remove this link
(define (ctx-remove-slot-info ctx stack-idx)
  (define (get-env env slot)
    (if (null? env)
        '()
        (let ((sslots (identifier-sslots (cdar env))))
          (if (member slot sslots)
              (cons (cons (caar env)
                          (identifier-copy (cdar env) #f (set-sub sslots (list slot))))
                    (cdr env))
              (cons (car env) (get-env (cdr env) slot))))))
  (let ((env (get-env (ctx-env ctx) (stack-idx-to-slot ctx stack-idx))))
    (ctx-copy ctx #f #f #f #f #f #f env)))

(define (ctx-remove-free-mems ctx)
  (let ((nfree-mems (length (ctx-free-mems ctx))))
    (ctx-copy ctx #f #f #f '() #f #f #f #f #f (- (ctx-fs ctx) nfree-mems))))

;;
;; GET LOC
(define (ctx-get-loc ctx stack-idx)
  (let* ((slot (stack-idx-to-slot ctx stack-idx))
         (r (assoc slot (ctx-slot-loc ctx))))
    (assert r "Internal error (ctx-get-loc)")
    (cdr r)))

;;
;; Get closure loc
(define (ctx-get-closure-loc ctx)
  (ctx-get-loc ctx (- (length (ctx-stack ctx)) 2)))

;;
;; Get retobj loc
(define (ctx-get-retobj-loc ctx)
  (ctx-get-loc ctx (- (length (ctx-stack ctx)) 1)))

;; Is register?
(define (ctx-loc-is-register? loc)
  (or (eq? loc 'tmp)
      (and (pair? loc)
           (eq? (car loc) 'r))))

;; Is memory ?
(define (ctx-loc-is-memory? loc)
  (and (pair? loc)
       (eq? (car loc) 'm)))

;; Is fregister ?
(define (ctx-loc-is-fregister? loc)
  (and (pair? loc)
       (eq? (car loc) 'fr)))

;; Is fmemory ?
(define (ctx-loc-is-fmemory? loc)
  (and (pair? loc)
       (eq? (car loc) 'fm)))

;; Is free variable loc ?
(define (ctx-loc-is-freemem? loc)
  (and (pair? loc)
       (eq? (car loc) 'f)))

;;
;; GET TYPE
(define (ctx-get-type ctx stack-idx)
  (list-ref (ctx-stack ctx) stack-idx))

;; GET TYPE FROM ID
;; Return #f if not found, type if found
(define (ctx-id-type ctx id)
  (let ((ident (assoc id (ctx-env ctx))))
    (if ident
        (ctx-identifier-type ctx (cdr ident))
        #f)))

;;
;; SET TYPE
;; Set type of data to 'type'
;; data could be a stack index
;; or an ident (id . identifier) object
(define (ctx-set-type ctx data type change-id-type)

  ;; We do *NOT* want non permanent constant in ctx
  (assert (not (and (ctx-type-cst? type)
                    (##mem-allocated? (ctx-type-cst type))
                    (not (eq? (mem-allocated-kind (ctx-type-cst type)) 'PERM))))
          "Internal error")

  (let ((ident (or (and (pair? data) (symbol? (car data)) (identifier? (cdr data)) data)
                   (ctx-ident-at ctx data))))
    (if (and change-id-type ident)
        ;; Change for each identifier slot
        (set-ident-type ctx ident type)
        ;; Change only this slot
        (ctx-copy ctx (stack-change-type (ctx-stack ctx) data type)))))

;;
;; GET CALL ARGS MOVES
;; r3 & r5 are not used because rdi and R11 are used for ctx, nb-args
(define args-regs '((r . 0) (r . 1) (r . 4) (r . 6) (r . 7) (r . 8))) ;; TODO move
(define nb-args-regs (length args-regs))
;; Return reg is one of the last registers to increase the chances it is chosen
;; if it is preferred register in ctx-get-free-reg (which is the case each time succ lco is a 'ret lco)
(define return-reg '(r . 8))
(define return-freg '(fr . 0))

;;
;;
;;
;;
;; PRIVATE module

;;
;; Return all slots associated to loc
(define (ctx-get-slots ctx loc)
  (foldr (lambda (sl r)
           (if (equal? (cdr sl) loc)
               (cons (car sl) r)
               r))
         '()
         (ctx-slot-loc ctx)))

;;
;; Return ident object from id
(define (ctx-ident ctx id)
 (let ((env (ctx-env ctx)))
   (assoc id env)))

;;
(define (ctx-set-loc-n ctx slots loc)
  (foldr (lambda (slot ctx)
           (ctx-set-loc ctx slot loc))
         ctx
         slots))

;; TODO: use stack idx to match public api
;; Change loc associated to given slot
;; change ONE SLOT only
;; TODO: use extra bool arg to change for associated identifier too  to match public api
;;       (see ctx-set-type)
(define (ctx-set-loc ctx slot loc #!optional change-id-loc)
  (if change-id-loc
      (let* ((ident (ctx-ident-at ctx (slot-to-stack-idx ctx slot)))
             (slots (and ident (identifier-sslots (cdr ident)))))
        (if slots
            (ctx-set-loc-n ctx slots loc)
            (ctx-set-loc-h ctx slot loc)))
      (ctx-set-loc-h ctx slot loc)))

(define (ctx-set-loc-h ctx slot loc)

  (define (get-slot-loc slot-loc)
    (if (null? slot-loc)
        '()
        (let ((sl (car slot-loc)))
          (if (eq? (car sl) slot)
              (cons (cons slot loc) (cdr slot-loc))
              (cons sl (get-slot-loc (cdr slot-loc)))))))

  (define (get-free-* slot-loc curr-free old-loc loc check-loc-type)
    (let* ((r (loc-used? old-loc slot-loc))
           (free-set
             (if (or (not (check-loc-type old-loc))
                     r)
                 curr-free
                 (cons old-loc curr-free))))
    (set-sub free-set (list loc))))

  (define (get-free-regs slot-loc curr-free old-loc loc)
    (get-free-* slot-loc curr-free old-loc loc ctx-loc-is-register?))
  (define (get-free-mems slot-loc curr-free old-loc loc)
    (get-free-* slot-loc curr-free old-loc loc ctx-loc-is-memory?))
  (define (get-free-fregs slot-loc curr-free old-loc loc)
    (get-free-* slot-loc curr-free old-loc loc ctx-loc-is-fregister?))
  (define (get-free-fmems slot-loc curr-free old-loc loc)
    (get-free-* slot-loc curr-free old-loc loc ctx-loc-is-fmemory?))

  (let* ((old-loc (cdr (assoc slot (ctx-slot-loc ctx))))
         (slot-loc (get-slot-loc (ctx-slot-loc ctx)))
         (free-regs  (get-free-regs  slot-loc (ctx-free-regs ctx)  old-loc loc))
         (free-mems  (get-free-mems  slot-loc (ctx-free-mems ctx)  old-loc loc))
         (free-fregs (get-free-fregs slot-loc (ctx-free-fregs ctx) old-loc loc))
         (free-fmems (get-free-fmems slot-loc (ctx-free-fmems ctx) old-loc loc)))
    (ctx-copy ctx #f slot-loc free-regs free-mems free-fregs free-fmems)))

;; Return a free memory slot
;; Return moves, mloc and updated ctx
;(moves/mem/ctx (ctx-get-free-mem ctx))
(define (ctx-get-free-mem ctx)
  (if (not (null? (ctx-free-mems ctx)))
      ;; An existing memory slot is empty, use it
      (list '() (car (ctx-free-mems ctx)) ctx)
      ;; Alloc a new memory slot
      (let ((mloc (cons 'm (ctx-fs ctx))))
        (list (list (cons 'fs 1))
              mloc
              (ctx-copy ctx #f #f #f (cons mloc (ctx-free-mems ctx)) #f #f #f #f #f (+ (ctx-fs ctx) 1))))))

;; Return memory location associated to ident or #f if no memory slot
(define (ctx-ident-mloc ctx ident)

  (define (get-mloc slot-loc sslots)
    (if (null? sslots)
        #f
        (let ((r (assoc (car sslots) slot-loc)))
          (if (or (ctx-loc-is-memory? (cdr r))
                  (ctx-loc-is-fmemory? (cdr r)))
              (cdr r)
              (get-mloc slot-loc (cdr sslots))))))

  (let ((sslots (identifier-sslots (cdr ident))))
    (get-mloc (ctx-slot-loc ctx) sslots)))

(define (stack-change-type stack stack-idx type)
  (append (list-head stack stack-idx) (cons type (list-tail stack (+ stack-idx 1)))))

(define (stack-idx-to-slot ctx stack-idx)
  (- (length (ctx-stack ctx)) stack-idx 1))

(define (slot-to-stack-idx ctx slot)
  (- (length (ctx-stack ctx)) slot 1))

;; Change type for each slots of identifier
(define (set-ident-type ctx ident type)

  (define (change-stack stack sslots)
    (if (null? sslots)
        stack
        (change-stack
          (stack-change-type stack (slot-to-stack-idx ctx (car sslots)) type)
          (cdr sslots))))

  (assert (not (eq? (ctx-identifier-type ctx (cdr ident)) 'free))
          "Internal error")

  (let ((sslots (identifier-sslots (cdr ident))))
    (ctx-copy
      ctx
      (change-stack (ctx-stack ctx) sslots))))

(define (ctx-ident-at ctx stack-idx)
  (define (ident-at-slot env slot)
    (if (null? env)
        #f
        (let ((ident (car env)))
          (if (member slot (identifier-sslots (cdr ident)))
              ident
              (ident-at-slot (cdr env) slot)))))
  (ident-at-slot
    (ctx-env ctx)
    (stack-idx-to-slot ctx stack-idx)))

;;
(define (ctx-ids-keep-non-cst ctx ids)
  (keep (lambda (el)
          (let* ((identifier (cdr (assoc el (ctx-env ctx))))
                 (loc (ctx-identifier-loc ctx identifier)))
            loc))
        ids))

;;-----------------------------------------------------------------------------
;; Ctx

;; Identifier object
(define-type identifier
  kind   ;; symbol 'free or 'local
  sslots ;; list of stack slots where the variable is
  flags  ;; list of variable flags
  stype  ;; ctx type (copied to virtual stack)
  cloc   ;; closure slot if free variable
  cst    ;; is this id constant ?
  thisid ;;
)

(define (identifier-copy identifier #!optional kind sslots flags stype cloc cst thisid)
  (make-identifier
    (or kind   (identifier-kind identifier))
    (or sslots (identifier-sslots identifier))
    (or flags  (identifier-flags identifier))
    (or stype  (identifier-stype identifier))
    (or cloc   (identifier-cloc identifier))
    (or cst    (identifier-cst identifier))
    (or thisid (identifier-thisid identifier))))


;;-----------------------------------------------------------------------------
;; Merge code
;;-----------------------------------------------------------------------------

;; Compute and returns moves needed to merge reg alloc from src-ctx to dst-ctx
(define (ctx-regalloc-merge-moves src-ctx dst-ctx)

  (define (add-boxes moves all-moves)
    (if (null? moves)
        '()
        (let ((move (car moves)))
          (if (and (not (ctx-loc-is-fregister? (cdr move)))
                   (not (ctx-loc-is-fmemory?   (cdr move)))
                   (not (eq? 'rtmp (cdr move)))
                   (or (ctx-loc-is-fregister?  (car move))
                       (ctx-loc-is-fmemory?    (car move))
                       (and (pair? (car move))
                            (eq?   (caar move) 'const)
                            (flonum? (cdar move)))))
              (let ((move (cons (cons 'flbox (car move)) (cdr move))))
                (cons move
                      (add-boxes (cdr moves) all-moves)))
              (begin
                ;; Assert that if the move is fr -> rtmp or fm -> rtmp,
                ;; then there is a rtmp -> fr or rtmp -> fm move
                (assert (if (and (or (ctx-loc-is-fregister? (cdr move))
                                     (ctx-loc-is-fmemory?   (cdr move)))
                                 (eq? 'rtmp (car move)))
                            (let ((m (assoc 'rtmp all-moves)))
                              (or (ctx-loc-is-fregister? (cdr m))
                                  (ctx-loc-is-fmemory?   (cdr m))))
                            #t)
                        "Internal error")
                (cons move (add-boxes (cdr moves) all-moves)))))))

  (define (get-req-moves)

    (define sl-dst (ctx-slot-loc dst-ctx))

    (let loop ((sl (ctx-slot-loc src-ctx)))
      (if (null? sl)
          '()
          (let ((first (car sl)))
            (if (cdr first)
                ;; Loc is not #f
                (let ((src (cdr first))
                      (dst (cdr (assoc (car first) sl-dst))))
                  (if (equal? src dst)
                      (loop (cdr sl))
                      (cons (cons src dst) (loop (cdr sl)))))
                ;; Loc is #f
                (let* ((slot (car first))
                       (type (list-ref (ctx-stack src-ctx) (slot-to-stack-idx src-ctx slot))))

                  (let* ((dst
                           (cdr (assoc slot (ctx-slot-loc dst-ctx))))
                         (src
                           (cond ((not (ctx-type-cst? type))
                                    #f)
                                 ((ctx-type-clo? type)
                                    (cons 'constfn (ctx-type-cst type)))
                                 ((ctx-type-ret? type)
                                    (cons 'constcont (ctx-type-cst type)))
                                 ((ctx-type-int? type)
                                    (cons 'intbox (cons 'const (ctx-type-cst type))))
                                 (else
                                    (cons 'const (ctx-type-cst type))))))
                    (assert (not (and (not src) dst)) "Internal error")
                    (if src
                        (cons (cons src dst) (loop (cdr sl)))
                        (loop (cdr sl))))))))))

  (let* ((req-moves (get-req-moves))
         (moves (steps req-moves))
         (moves (add-boxes moves moves))
         (fs-move
           (let ((diff (- (ctx-fs dst-ctx) (ctx-fs src-ctx))))
             (if (= diff 0) '() `((fs . ,diff)))))
         (ffs-move
           (let ((diff (- (ctx-ffs dst-ctx) (ctx-ffs src-ctx))))
             (if (= diff 0) '() `((ffs . ,diff))))))
    (append fs-move ffs-move moves)))

(define (ctx-get-call-args-moves ast ctx nb-args cloloc contloc tail? generic-entry? inlined-call?)

  (define clomove  (and cloloc (cons cloloc  '(r . 2))))
  (define contmove (and tail? contloc (not (equal? contloc '(m . 0))) (cons contloc '(m . 0))))

  (define (get-req-moves curr-idx rem-regs rem-fregs moves pushed)

    (define (next-nothing)
      (get-req-moves (- curr-idx 1) rem-regs rem-fregs moves pushed))
    (define (next-float from)
      (let ((moves (cons (cons from (car rem-fregs)) moves)))
        (get-req-moves (- curr-idx 1) rem-regs (cdr rem-fregs) moves pushed)))
    (define (next-other from)
      (if (null? rem-regs)
          (get-req-moves (- curr-idx 1) '() rem-fregs moves (cons from pushed))
          (let ((moves (cons (cons from (car rem-regs)) moves)))
            (get-req-moves (- curr-idx 1) (cdr rem-regs) rem-fregs moves pushed))))


    (if (< curr-idx 0)
        (cons (reverse pushed) moves)
        (let* ((type (ctx-get-type ctx curr-idx))
               (loc  (ctx-get-loc ctx curr-idx)))

          (cond
            ;; Type is cst, cst is versioned, and we do not use generic ep
            ((and opt-entry-points
                  (const-versioned? type)
                  (not (ctx-type-id? type))
                  (not generic-entry?))
               (next-nothing))
            ;; Type is cst
            ((not loc)
               (cond ((ctx-type-clo? type)
                        (next-other
                          (cons 'constfn (ctx-type-cst type))))
                     ((ctx-type-flo? type)
                        (if (or (and (not opt-entry-points)
                                     (not inlined-call?))
                                generic-entry?
                                (not opt-float-unboxing))
                            (next-other
                              (cons 'flbox (cons 'const (ctx-type-cst type))))
                            (next-float
                              (cons 'const (ctx-type-cst type)))))
                     ((ctx-type-int? type)
                        (if (or (and (not opt-entry-points)
                                     (not inlined-call?))
                                generic-entry?
                                (not opt-int-unboxing))
                            (next-other (cons 'intbox (cons 'const (ctx-type-cst type))))
                            (next-other (cons 'const (ctx-type-cst type)))))
                     (else
                        (if (and (ctx-type-int? type) (not opt-int-unboxing))
                            (next-other (cons 'intbox (cons 'const (ctx-type-cst type))))
                            (next-other (cons 'const (ctx-type-cst type)))))))
            ;; Type is float !cst
            ((and (ctx-type-flo? type)
                  (or (and (not opt-entry-points)
                           (not inlined-call?))
                      generic-entry?))
               (next-other
                 (cons 'flbox loc)))
            ;; Others
            (else
               (if (and (ctx-type-flo? type)
                        opt-float-unboxing)
                   (next-float loc)
                   (next-other loc)))))))

  (let ((pushed/moves (get-req-moves (- nb-args 1) args-regs (ctx-init-free-fregs) '() '())))

    (if contmove
        (set! pushed/moves
              (cons (car pushed/moves)
                    (cons contmove (cdr pushed/moves)))))

    (cons (car pushed/moves)
          (if clomove
              (steps (append (cdr pushed/moves) (list clomove)))
              (steps (cdr pushed/moves))))))

(define (steps required-moves)

  (let loop ((real-moves '())
             (req-moves required-moves))
    (if (null? req-moves)
        real-moves
        (let ((r (step '() req-moves '())))
          (loop (append real-moves (car r)) (cdr r))))))

;; This function takes all required moves (without considering values overwriting)
;; and returns a list of real moves ex. ((r0 . r4) (r4 . r5)) for one step,
;; and the list of moves that remain to be processed.

;; visited-locs: list of visited nodes (source node of a move) during the step
;; moves: list of moves required to merge ctx
;; pending-moves: list of currently computed real moves
(define (step visited req-moves pending-moves #!optional src-sym)

  ;; A loc is available if it is not a source of a move in required moves
  ;; We can then directly overwrite its content
  (define (loc-available loc)
    (not (assoc loc req-moves)))

  ;; Update list of required moves, replace each src by its new position
  (define (update-req-moves next-req-moves step-real-moves)
    (if (null? next-req-moves)
        '()
        (let* ((move (car next-req-moves))
               (src (car move))
               (r (assoc src step-real-moves))
               (updated-move
                 (cond ((and r (eq? (cdr r) 'rtmp))
                          (let ((r (assoc 'rtmp step-real-moves)))
                            (cons (cdr r) (cdr move))))
                       ((and r
                             (not (eq? (caar move) 'const))
                             (not (eq? (caar move) 'constfn)))
                          (cons (cdr r) (cdr move)))
                       (else
                          move))))
            (cons updated-move
                  (update-req-moves (cdr next-req-moves) step-real-moves)))))

  (let* ((move
           (cond ((null? req-moves) #f)
                 (src-sym           (assoc src-sym req-moves))
                 (else              (car req-moves))))
         (src (and move (car move)))
         (dst (and move (cdr move))))

    (cond ;; Case 0: No more move, validate pending moves
          ((not move)
             (cons pending-moves
                   '()))
          ;; Case 2: dst has already been visited, it's a cycle.
          ;;         validate pending moves using temporary register
          ((member dst visited)
             (let ((real-moves
                     (append (cons (cons src 'rtmp)
                                   pending-moves)
                             (list (cons 'rtmp dst))))
                   (req-moves (set-sub req-moves (list move))))
               (cons real-moves
                     (update-req-moves req-moves real-moves))))
          ;; Case 1: Destination is free, validate pending moves
          ((loc-available dst)
             (let ((real-moves (cons move
                                     pending-moves))
                   (req-moves  (set-sub req-moves (list move))))
               (cons real-moves
                     (update-req-moves req-moves real-moves))))
          ;; Case 2: src is dst
          ((equal? src dst)
             (step (cons src visited)
                   (set-sub req-moves (list move))
                   (cons move pending-moves)))
          ;; Case 3: dst is an src of an other move,
          ;;         add current move to pending moves and continue with next move
          (else
             (step (cons src visited)
                   (set-sub req-moves (list move))
                   (cons move pending-moves)
                   dst)))))

;; Compute gc map descriptor from ctx
;; Descriptor is:
;; | ...56 bits... | ...8 bits... |
;; | frame objects |      fs      |
;; each bit in 'frame objects' represents an entry of the stack frame
;; if bit is 0, the object must be scanned
;; if bit is 1, the object must *not* be scanned
(define (ctx->gc-map-desc ctx)

  (define (slot-from-loc loc)
    (let ((sl (ctx-slot-loc ctx)))
      (let loop ((sl sl))
        (if (null? sl)
            #f
            (if (equal? (cdar sl) loc)
                (caar sl)
                (loop (cdr sl)))))))

  (if (and opt-float-unboxing (> (ctx-fs ctx) 56))
      (error "NYI 56"))

  (let* ((fs (ctx-fs ctx))
         (fo (if (not opt-float-unboxing)
                 0
                 (let loop ((cur-idx 0) (r 0))
                   (if (= cur-idx fs)
                       r
                       (let ((slot (slot-from-loc (cons 'm cur-idx))))
                         (if (not slot)
                             (loop (+ cur-idx 1) (bitwise-ior r (expt 2 (+ 8 cur-idx))))
                             (let ((type (ctx-get-type ctx (slot-to-stack-idx ctx slot))))
                               (if (ctx-type-flo? type)
                                   (loop (+ cur-idx 1) (bitwise-ior r (expt 2 (+ 8 cur-idx))))
                                   (loop (+ cur-idx 1) r))))))))))
  (+ fo fs)))

(define (call-stack->gc-map-desc stack)
  (let* ((nargs (length stack))
         (fs (+ (max (- nargs (length args-regs)) 0) 1))
         (fo (if (not opt-float-unboxing)
                 0
                 (let loop ((stack stack) (n fs) (r 0))
                   (if (= n 1)
                       (bitwise-ior r 256)
                       (let ((type (car stack)))
                         (if (ctx-type-flo? type)
                             (loop (cdr stack)
                                   (- n 1)
                                   (bitwise-xor r
                                                (expt 2 (+ 7 n))))
                             (loop (cdr stack) (- n 1) r))))))))
    (+ fo fs)))

(define (ret-gc-map-desc)
  0)
