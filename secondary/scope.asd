
(use-package '(:asdf))

(defsystem :scope
    :description "Scoped variables/functions.
Denest can do somewhat similar purpose."
    :components ((:file "scope")))
