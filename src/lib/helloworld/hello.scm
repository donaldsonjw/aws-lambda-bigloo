(module hello
   (library aws-lambda-bigloo)
   (export (handler context event)))


(define (handler context event)
   (values "application/json" "{\"hello\": \"World!\"}"))

