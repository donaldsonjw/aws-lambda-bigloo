(module bootstrap-main
   (library aws-lambda-bigloo)
   (main main))

;; (library module procedure)
(define (parse-handler-info str)
   (string-split str "|"))

(define (handler-library-or-file handler-info)
   (car handler-info))

(define (handler-module handler-info)
   (cadr handler-info))

(define (handler-procedure handler-info)
   (caddr handler-info))

(define (compiled-handler?::bool handler-info)
   (pregexp-match ".*\.so" (handler-library-or-file handler-info)))

(define (make-compiled-export-name name)
   (string-append name "-env"))

(define (get-compiled-handler handler-info task-root)
   (let* ((handler-lib-dir (make-file-path (dirname task-root) "task/lib"))
         (full-lib-path (find-file/path (handler-library-or-file handler-info)
                           (list handler-lib-dir))))
      
      (set! *dynamic-load-path* (cons handler-lib-dir *dynamic-load-path*))
      (log-info "main" "*dynamic-load-path*: ~s" *dynamic-load-path*)
      
      (log-info "main" "dynamic-load results: ~a" (dynamic-load full-lib-path #f
          (string->symbol (handler-module handler-info))))

      (log-info "main" "successfully loaded ~a" full-lib-path)

      (let ((handler-symbol (dynamic-load-symbol full-lib-path
                               (make-compiled-export-name (handler-procedure handler-info))
                               (handler-module handler-info))))
         (log-info "main" "handler-symbol: ~a" handler-symbol)
         (if (and handler-symbol
                  (procedure? (dynamic-load-symbol-get handler-symbol)))
             (begin
                (log-info "main" "successfully loaded symbol ~a" handler-symbol)
                (dynamic-load-symbol-get handler-symbol))
             (begin
                (log-error "aws-lambda-bigloo" "could not find handler procedure - module: ~a, procedure: ~a"
                   (handler-module handler-info) (handler-procedure handler-info))
                (exit -1))))))

(define (get-interpreted-handler handler-info task-root)
   (let* ((handler-src-dir (make-file-path (dirname task-root) "task/src")))

      (set! *load-path* (cons handler-src-dir *load-path*))
      (loadq (handler-library-or-file handler-info))
      (let ((handler-proc (eval `(@ ,(string->symbol (handler-procedure handler-info))
                                    ,(string->symbol (handler-module handler-info))))))
         (if (and handler-proc (procedure? handler-proc))
             handler-proc
             (begin
                (log-error "aws-lambda-bigloo" "could not find source handler - module: ~a, procedure: ~a"
                   (handler-module handler-info) (handler-procedure handler-info))
                (exit -1))))))

(define (get-handler handler-info task-root)
   (if (compiled-handler? handler-info)
       (get-compiled-handler handler-info task-root)
       (get-interpreted-handler handler-info task-root))
   )

;; compiled-handler: libhelloworld_s-0.2.so|hello|handler
;; billed duration: 65ms  duration: 2.87 ms
;; max memory 36 MB
;; interpreted-handler: hello.scm|hello2|handler
;; billed duration 57ms duration: 1.83 ms
;; max memory 36 MB
(define (main args)
   (log-level-set! 'debug)
   (let* ((handler-info (parse-handler-info (get-handler-info)))
          (task-root (get-lambda-task-root))
          (handler-proc (get-handler handler-info task-root)))
      (let loop ()
         (run-handler handler-proc)
         (loop))))
