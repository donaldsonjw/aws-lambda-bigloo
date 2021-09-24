(module aws-lambda-bigloo-context
   (export (abstract-class <context>)
           (make-context::<context> #!key function-name function-version
              invoked-function-arn runtime-deadline-ms memory-limit-in-mb
              aws-request-id log-group-name
              log-stream-name (client-context::string "")
              (cognito-identity::string ""))
           (generic context-function-name::string context::<context>)
           (generic context-function-version::string context::<context>)
           (generic context-invoked-function-arn::string context::<context>)
           (generic context-remaining-time-ms::long context::<context>)
           (generic context-memory-limit-in-mb::long context::<context>)
           (generic context-log-group-name::string context::<context>)
           (generic context-log-stream-name::string context::<context>)
           (generic context-client-context::string context::<context>)
           (generic context-aws-request-id::string context::<context>)
           (generic context-cognito-identity::string context::<context>))
   
   (static (final-class <context-impl>::<context>
              function-name::string
              function-version::string
              invoked-function-arn::string
              runtime-deadline-ms::long
              memory-limit-in-mb::long
              aws-request-id::string
              log-group-name::string
              log-stream-name::string
              client-context::string
              cognito-identity::string)))


;;;; context protocol
(define-generic (context-function-name::string context::<context>))

(define-generic (context-function-version::string context::<context>))

(define-generic (context-invoked-function-arn::string context::<context>))

(define-generic (context-remaining-time-ms::long context::<context>))

(define-generic (context-memory-limit-in-mb::long context::<context>))

(define-generic (context-log-group-name::string context::<context>))

(define-generic (context-log-stream-name::string context::<context>))

(define-generic (context-client-context::string context::<context>))

(define-generic (context-aws-request-id::string context::<context>))

(define-generic (context-cognito-identity::string context::<context>))


;;;; <context-impl> implementation
(define (make-context::<context> #!key function-name function-version
           invoked-function-arn runtime-deadline-ms memory-limit-in-mb 
           aws-request-id log-group-name
           log-stream-name (client-context::string "")
           (cognito-identity::string ""))
   (define (get-missing-args-msg)
      (map cdr (filter (lambda (v) (car v))
                  `((,function-name . "function-name")
                    (,function-version . "function-version")
                    (,invoked-function-arn . "inoked-function-arn")
                    (,memory-limit-in-mb . "memory-limit-in-mb")
                    (,runtime-deadline-ms . "runtime-deadline-ms")
                    (,aws-request-id . "aws-request-id")
                    (,log-group-name . "log-group-name")
                    (,log-stream-name . "log-stream-name")))))

   (if (and function-name function-version invoked-function-arn memory-limit-in-mb
            runtime-deadline-ms aws-request-id log-group-name log-stream-name)
       (instantiate::<context-impl> (function-name function-name)
                               (function-version function-version)
                               (invoked-function-arn invoked-function-arn)
                               (runtime-deadline-ms runtime-deadline-ms)
                               (memory-limit-in-mb memory-limit-in-mb)
                               (aws-request-id aws-request-id)
                               (log-group-name log-group-name)
                               (log-stream-name log-stream-name)
                               (client-context client-context)
                               (cognito-identity cognito-identity))    
       (error "make-context" "missing arguments: "
          (get-missing-args-msg))))

(define-method (context-function-name::string context::<context-impl>)
   (-> context function-name))

(define-method (context-function-version::string context::<context-impl>)
   (-> context function-version))

(define-method (context-invoked-function-arn::string context::<context-impl>)
   (-> context invoked-function-arn))

(define-method (context-remaining-time-ms::string context::<context-impl>)
   (- (-> context runtime-deadline-ms)
      (current-milliseconds)))

(define-method (context-memory-limit-in-mb::long context::<context-impl>)
   (-> context memory-limit-in-mb))

(define-method (context-aws-request-id::string context::<context-impl>)
   (-> context aws-request-id))

(define-method (context-log-group-name::string context::<context-impl>)
   (-> context log-group-name))

(define-method (context-log-stream-name::string context::<context-impl>)
   (-> context log-stream-name))

(define-method (context-client-context::string context::<context-impl>)
   (-> context client-context))

(define-method (context-cognito-identity::string context::<context-impl>)
   (-> context cognito-identity))