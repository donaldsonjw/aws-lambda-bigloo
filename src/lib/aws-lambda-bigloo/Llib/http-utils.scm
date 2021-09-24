(module aws-lambda-bigloo-http-utils
   (import aws-lambda-bigloo-logging)
   (export (abstract-class <http-response>)
           (http-get::<http-response> #!key url (params '()) (headers '()))
           (generic http-response-status::long response::<http-response>)
           (generic http-response-content-type::string response::<http-response>)
           (generic http-response-get-header response::<http-response> name::string)
           (generic http-response-headers-for-each response::<http-response> proc::procedure)
           (generic http-response-headers-map response::<http-response> proc::procedure)
           (generic http-response-body::string response::<http-response>)
           (http-post::<http-response> #!key url (params '()) (headers '())
              (form '()) (body #f)))
   (static (final-class <http-response-impl>::<http-response>
              status::long
              content-type::bstring
              transfer-encoding::bstring
              headers::pair-nil
              body::string)))

;;;; <http-response> protocol
(define-generic (http-response-status::long response::<http-response>))
(define-generic (http-response-content-type::string response::<http-response>))
(define-generic (http-response-get-header response::<http-response> name::string))
(define-generic (http-response-headers-for-each response::<http-response> proc::procedure))
(define-generic (http-response-headers-map response::<http-response> proc::procedure))
(define-generic (http-response-body::string response::<http-response>))

;;;; <http-response-impl>
(define-method (http-response-status::long response::<http-response-impl>)
   (-> response status))

(define-method (http-response-content-type::string response::<http-response-impl>)
   (-> response content-type))

(define-method (http-response-get-header response::<http-response-impl> name::string)
   (let ((res (assoc (string->keyword name) (-> response headers))))
      (or (and res (cdr res)) "")))

(define-method (http-response-headers-for-each response::<http-response-impl>
                  proc::procedure)
   (for-each (lambda (nv) (proc (car nv) (cdr nv))) (-> response headers)))

(define-method (http-response-headers-map response::<http-response-impl>
                  proc::procedure)
   (map (lambda (nv) (proc (car nv) (cdr nv))) (-> response headers)))

(define-method (http-response-body::string response::<http-response-impl>)
   (-> response body))

(define (uinfo-name uinfo)
   (car (string-split uinfo "@")))

(define (uinfo-password uinfo)
   (cadr (string-split uinfo "@")))

(define (format-query-params params::pair-nil)
   (if (pair? params)
       (format "?~(&)" (map (lambda (nv)
                               (format "~(=)"
                                  (map uri-encode-component nv))) params))
       ""))

(define (get-header::bstring headers::pair-nil name::keyword)
   (let* ((name (keyword->string name))
          (res (find (lambda (nv) (string-ci=? name (keyword->string (car nv)))) headers)))
      (if res
          (cdr res)
          "")))


(define (parse-http-response::<http-response> input status headers content-length transfer-encoding)

   (log-info "http-utils" "status: ~a, headers: ~a, content-length: ~a, transfer-encoding: ~a~%" status headers content-length transfer-encoding)
   (instantiate::<http-response-impl> (status status)
                                      (headers headers)
                                      (content-type (get-header headers :content-type))
                                      (transfer-encoding (or transfer-encoding ""))
                                      (body (read-chars content-length input))))

(define (http-get::<http-response> #!key url (params '()) (headers '()))
   (when (or (not url) (string=? url ""))
      (error "http-get" "an url must be provided" url))
   (multiple-value-bind (protocol uinfo host port path)
      (url-parse url)
      (let* ((path+params (string-append path (format-query-params params)))
             (content-type (get-header headers :content-type))
             (sock (http :protocol (string->symbol protocol)                                                                                                                                 
                      :host host :port port :path path+params
                      :username (if uinfo (uinfo-name uinfo))
                      :password (if uinfo (uinfo-password uinfo))
                      :content-type content-type
                      :header headers
                      :method 'get))
             (ip (socket-input sock))
             (op (socket-output sock)))
         (with-handler
            (lambda (e)
               (if (isa? e &http-redirection)
                   (with-access::&http-redirection e (url)
                      (http-get :url url :params params :headers headers))
                   (raise e)))
            (http-parse-response ip op parse-http-response)))))

(define (http-post::<http-response> #!key url (params '()) (headers '())
           (form '()) (body #f))
   (when (or (not url) (string=? url ""))
      (error "http-post" "an url must be provided" url))
   (multiple-value-bind (protocol uinfo host port path)
      (url-parse url)
      (let* ((path+params (string-append path (format-query-params params)))
             (content-type (get-header headers :content-type))
             (sock (http :protocol (string->symbol protocol)                                                                                                                                 
                      :host host :port port :path path+params
                      :username (if uinfo (uinfo-name uinfo))
                      :password (if uinfo (uinfo-password uinfo))
                      :header headers
                      :content-type content-type
                      :body body
                      :args form
                      :method 'post))
             (ip (socket-input sock))
             (op (socket-output sock)))
         (with-handler
            (lambda (e)
               (if (isa? e &http-redirection)
                   (with-access::&http-redirection e (url)
                      (http-get :url url :params params :headers headers))
                   (raise e)))
            (http-parse-response ip op parse-http-response)))))