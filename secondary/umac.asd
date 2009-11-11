
(use-package '(:asdf))

(defsystem :umac
    :description "Extendable looping/accumulating/declaring macro.
In my opinion :denest and iterate(other library) is much better! Use that!"
    :components ((:file "umac")
		 (:file "umac-basic" :depends-on ("umac"))))
