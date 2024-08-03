;; api-contract-simulator.scm
;; Metadata
;; =========
;; Author: [Your Name]
;; Date: 2024-08-03
;; Description: API Contract Simulator in Scheme, simulates an API contract using OpenAPI nomenclature and tests it with malformed requests or responses.
;; Notes:
;; This simulator is a direct implementation of the OpenAPI specification for an API contract.
;; It uses the OpenAPI framework to model the API's behavior.
;; The simulator starts with a valid API contract and tests it with malformed requests or responses.

(use-modules (openapi-demo api-framework)
             (srfi srfi-1)) ; for `every`

(define (api-contract-simulator)
  (define initial-api-contract
    '((openapi . "3.0.0")
      (info . ((title . "My API")
               (description . "My API description")
               (version . "1.0.0")))
      (paths . ((/users . ((get . ((responses . ((200 . ((description . "Users list")))))))))
                (/users/{userId} . ((get . ((responses . ((200 . ((description . "User details")))))))))))
      (definitions . ((User . ((type . "object")
                                (properties . ((name . ((type . "string")))
                                               (email . ((type . "string"))))))))))))
  (define (simulate-api-contract api-contract)
    (define (test-api-contract path)
      (let ((method (get-method api-contract path)))
        (if method
            (let ((request-body (generate-malformed-request-body api-contract method)))
              (test-api-contract-with-request-body api-contract method request-body))
            (format #t "No method found for path ~a~%" path))))
    (every test-api-contract (api-contract-paths api-contract)))
  (define (generate-malformed-request-body api-contract method)
    (let ((schema (get-schema api-contract method)))
      (if schema
          (generate-malformed-json schema)
          '())))
  (define (generate-malformed-json schema)
    (case (json-schema-type schema)
      ((object) (generate-malformed-json-object schema))
      ((array) (generate-malformed-json-array schema))
      ((string) (generate-malformed-json-string schema))
      ((integer) (generate-malformed-json-integer schema))
      ((number) (generate-malformed-json-number schema))
      (else '())))
  (simulate-api-contract initial-api-contract))
