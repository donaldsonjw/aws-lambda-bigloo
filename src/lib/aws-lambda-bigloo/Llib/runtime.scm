(module aws-lambda-bigloo-runtime
   (include "let-values.sch")
   (import aws-lambda-bigloo-context
           aws-lambda-bigloo-http-utils
           aws-lambda-bigloo-logging)
   (export (run-handler hander::procedure)
           (get-handler-info)
           (get-lambda-task-root)))

(define +log-tag+ "lambda-runtime")

(define +max-retries+ 3)

;;;; AWS Lambda reserved environment variables
(define +handler+ "_HANDLER")
(define +x-amzn-trace-id+ "_X_AMZN_TRACE_ID")
(define +aws-region+ "AWS_REGION")
(define +aws-execution-env+ "AWS_EXECUTION_ENV")
(define +aws-lambda-function-name+ "AWS_LAMBDA_FUNCTION_NAME")
(define +aws-lambda-function-memory-size+ "AWS_LAMBDA_FUNCTION_MEMORY_SIZE")
(define +aws-lambda-function-version+ "AWS_LAMBDA_FUNCTION_VERION")
(define +aws-lambda-initialization-type+ "AWS_LAMBDA_INITIALIZATION_TYPE")
(define +aws-lambda-log-group-name+ "AWS_LAMBDA_LOG_GROUP_NAME")
(define +aws-lambda-log-stream-name+ "AWS_LAMBDA_LOG_STREAM_NAME")
(define +aws-access-key-id+ "AWS_ACCESS_KEY_ID")
(define +aws-secret-access-key+ "AWS_SECRET_ACCESS_KEY")
(define +aws-session-token+ "AWS_SESSION_TOKEN")
(define +aws-lambda-runtime-api+ "AWS_LAMBDA_RUNTIME_API")
(define +lambda-task-root+ "LAMBDA_TASK_ROOT")
(define +lambda-runtime-dir+ "LAMBDA_RUNTIME_DIR")
(define +tz+ "TZ")

;;;; AWS lambda header constants
(define +request-id-header+ "lambda-runtime-aws-request-id")
(define +trace-id-header+ "lambda-runtime-trace-id")
(define +client-context-header+ "lambda-runtime-client-context")
(define +cognito-identity-header "lambda-runtime-cognito-identity")
(define +deadline-ms-header+ "lambda-runtime-deadline-ms")
(define +function-arn-header+ "lambda-runtime-invoked-function-arn")

;;;; AWS API endpoint suffixes
(define +init-path-suffix+ "/2018-06-01/runtime/init/error")
(define +next-path-suffix+ "/2018-06-01/runtime/invocation/next")
(define +result-path-suffix+ "/2018-06-01/runtime/invocation/")

(define (get-lambda-runtime-api-endpoints)
   (let ((endpoint-prefix (string-append "http://" (getenv +aws-lambda-runtime-api+))))
      (values
         (string-append endpoint-prefix +init-path-suffix+)
         (string-append endpoint-prefix +next-path-suffix+)
         (string-append endpoint-prefix +result-path-suffix+))))

(define (success? status-code::long)
   (and (>= status-code 200)
        (<= status-code 209)))

(define (safe-getenv str)
   (or (getenv str) ""))

(define (get-handler-info)
   (safe-getenv +handler+))

(define (get-lambda-task-root)
   (safe-getenv +lambda-task-root+))

(define (get-context resp::<http-response>)
   (let ((function-name (safe-getenv +aws-lambda-function-name+))
         (function-version (safe-getenv +aws-lambda-function-version+))
         (invoked-function-arn (http-response-get-header resp +function-arn-header+))
         (runtime-deadline-ms (string->integer (http-response-get-header resp +deadline-ms-header+)))
         (memory-limit-in-mb (string->integer (safe-getenv +aws-lambda-function-memory-size+)))
         (log-group-name (safe-getenv +aws-lambda-log-group-name+))
         (log-stream-name (safe-getenv +aws-lambda-log-group-name+))
         (client-context (http-response-get-header resp +client-context-header+))
         (aws-request-id (http-response-get-header resp +request-id-header+))
         (cognito-identity (http-response-get-header resp +cognito-identity-header)))
      (make-context :function-name function-name :function-version function-version
         :invoked-function-arn invoked-function-arn :runtime-deadline-ms runtime-deadline-ms
         :memory-limit-in-mb memory-limit-in-mb :aws-request-id aws-request-id
         :log-group-name log-group-name :log-stream-name log-stream-name
         :client-context client-context
         :cognito-identity cognito-identity)))

(define (get-next url::bstring)
   (with-handler
      (lambda (e)
         (log-error +log-tag+ "get-next: failed to get next invocation. exception: ~a" e)
         (values #f #f))
      (let* ((resp (http-get :url url))
             (status-code (http-response-status resp)))
         (if (success? status-code)
             (values (get-context resp) (http-response-body resp))
             (begin
                (log-error +log-tag+ "get-next: failed to get next invocation request. response: ~a" resp)
                (values #f #f))))))


(define (post-success! url::bstring request-id::bstring  content-type::bstring result::bstring)
   (with-handler
      (lambda (e)
         (log-error +log-tag+ "post-success!: failed to send handler success response. exception: ~a" e)
         #f)
      (let* ((url (string-append url request-id "/response"))
             (resp (http-post :url url :headers `((content-type: . ,content-type)
                                                  (expect: . "")
                                                  ;(transfer-encoding: "identity")
                                                  )
                      :body result))
             (status-code (http-response-status resp)))
         (if (success? status-code)
             #t
             (begin
                (log-error +log-tag+ "post-success!: failed to send handler success response: ~a" resp)
                #f )))))

(define (post-failure! url::bstring request-id::bstring  error-msg::bstring error-type::bstring)
   (with-handler
      (lambda (e)
         (log-error +log-tag+ "post-failure!" "failed to send handler failure response. exception: ~a" e)
         #f)
      (let* ((url (string-append url request-id "/error"))
           (resp (http-post :url url :headers `((content-type: "application/json")
                                                (expect: "")
                                                ;(transfer-encoding: "identity")
                                                )
                    :body (format "{\"errorMessage\": ~s, \"errorType\": ~s}"
                             error-msg error-type)))
           (status-code (http-response-status resp)))
       (if (success? status-code)
           #t
           (begin
              (log-error +log-tag+ "post-failure!" "failed to send handler success response: ~a" resp)
              #f )))))


(define (with-retry thunk::procedure success?::procedure max-tries::long  
           attribution::symbol)
   (let loop ((tries 0))
      (if (= tries max-tries)
          (log-error "call-with-retry" "exhausted all retries"
             attribution)
          (let ((res (call-with-values thunk list)))
             (if (apply success? res)
                 (apply values res)
                 (loop (+ tries 1)))))))

(define (all-true? . args)
    (every (lambda (v) (and v)) args))

(define (run-handler handler::procedure)
   (let-values (((error-url next-url result-url)
                 (get-lambda-runtime-api-endpoints)))
      
      (log-debug +log-tag+ "init-url: ~s, next-url: ~s, result-url: ~s"
         error-url next-url result-url)
      
      (let*-values (((context event) (with-retry (lambda () (get-next next-url))
                                        all-true? 3 'get-next))
                    ((content-type response) (handler context event)))
         (if response
             (with-retry (lambda ()  (post-success! result-url (context-aws-request-id context)
                                   content-type response))
                all-true? 3 'post-success!)
             (with-retry (lambda () (post-failure! result-url (context-aws-request-id context)
                                  content-type response))
                all-true? 3 'post-failure!)))))
