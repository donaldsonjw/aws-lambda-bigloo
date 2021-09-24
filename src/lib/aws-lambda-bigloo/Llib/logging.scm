(module aws-lambda-bigloo-logging
   (export (log-error tag::bstring msg::bstring . args)
           (log-info tag::bstring msg::bstring . args)
           (log-debug tag::bstring msg::bstring . args)
           (log-warn tag::bstring msg::bstring . args)
           (log-level-set! level::symbol)
           (log-level)))

(define +log-levels+ '((error . "[ERROR]")
                       (warn . "[WARN]")
                       (info . "[INFO]")
                       (debug . "[DEBUG]")))

(define (level-prefix level::symbol)
   (let ((res (assq level +log-levels+)))
      (if res
          (cdr res)
          (error "level-prefix" "unknown log level" level))))

(define +log-level-vals+ '((error . 0)
                           (warn . 1)
                           (info . 2)
                           (debug . 3)))

(define +vals->level+
   (map (lambda (p) (cons (cdr p) (car p)))
      +log-level-vals+))

(define (level-val level::symbol)
   (let ((res (assq level +log-level-vals+)))
      (if res
          (cdr res)
          (error "level-val" "unknown level" level))))

(define *log-level* (level-val 'error))

(define (log-level-set! level::symbol)
   (set! *log-level* (level-val level)))

(define (log-level)
   (cdr (assq *log-level* +vals->level+)))

(define (log-error tag::bstring msg::bstring . args)
   (when (<= 0 *log-level* )
      (log 'error tag msg args)))

(define (log-warn tag::bstring msg::bstring . args)
   (when (<= 1 *log-level*)
      (log 'warn tag msg args)))

(define (log-info tag::bstring msg::bstring . args)
   (when (<= 2 *log-level*)
      (log 'info tag msg args)))

(define (log-debug tag::bstring msg::bstring . args)
   (when (<= 3 *log-level*)
      (log 'debug tag msg args)))

(define (log level::symbol tag::bstring msg::bstring args::pair-nil)
   (let ((fmt-msg (apply format (cons msg args))))
      (printf "~a [~a] ~a ~a~%" (level-prefix level)
         (current-milliseconds) tag fmt-msg)))