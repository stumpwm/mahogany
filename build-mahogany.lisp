(require 'asdf)

;; See https://asdf.common-lisp.dev/asdf.html#Configuration-DSL-1
;; for what this is doing.
;; Basically, we want our local copies of dependencies in the dependencies folder
;; to be used instead of anything that asdf can find in our environment
(asdf:initialize-source-registry
 `(:source-registry
   (:directory ,(uiop/os:getcwd))
   (:tree ,(merge-pathnames (uiop/os:getcwd) #P"dependencies"))
   ;; Use whatever the user has configured in their environment to find the rest.
   :inherit-configuration))

;; (asdf:find-system "mahogany/executable")
;; (asdf:clear-configuration)

(asdf:make "mahogany/executable")
