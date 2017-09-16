;;;; cl-gestures
;;;; ric@rmhsilva.com

(asdf:defsystem #:cl-gestures
  :version      "0.1.0"
  :description  "Multitouch gesture handling with MTIF"
  :author       "Ric da Silva <ric@rmhsilva.com>"
  :license      "MIT"

  :components ((:file "package")
               (:file "utils")
               (:file "cl-gestures")
               (:file "gestures-imp"))

  :depends-on (#:cffi
               #:mtif))
