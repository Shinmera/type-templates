(asdf:defsystem type-templates
  :version "4.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A library for defining and expanding templated functions"
  :homepage "https://shinmera.com/docs/type-templates/"
  :bug-tracker "https://shinmera.com/project/type-templates/issues"
  :source-control (:git "https://shinmera.com/project/type-templates.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "type")
               (:file "template")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :alexandria
               :form-fiddle))
