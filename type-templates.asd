#|
 This file is a part of type-templates
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem type-templates
  :version "4.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A library for defining and expanding templated functions"
  :homepage "https://Shinmera.github.io/type-templates/"
  :bug-tracker "https://github.com/Shinmera/type-templates/issues"
  :source-control (:git "https://github.com/Shinmera/type-templates.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "type")
               (:file "template")
               (:file "documentation"))
  :depends-on (:documentation-utils)
  :in-order-to ((asdf:test-op (asdf:test-op :type-templates-test))))
