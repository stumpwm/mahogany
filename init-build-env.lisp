(declaim (optimize (debug 3)))

(cl:pushnew :hrt-debug *features*)

(let* ((root (uiop/os:getcwd))
       (asdf-cache (make-pathname
		    :directory (append
				(pathname-directory root)
				(list
				 "build"
				 "asdf-cache"
				 (uiop:implementation-identifier))))))
  ;; See https://asdf.common-lisp.dev/asdf.html#Configuration-DSL-1
  ;; for what this is doing.
  ;; Basically, we want our local copies of dependencies in the dependencies folder
  ;; to be used instead of anything that asdf can find in our environment
  (asdf:initialize-source-registry
   `(:source-registry
     (:directory ,root)
     (:tree ,(merge-pathnames root #P"dependencies"))
     ;; Use whatever the user has configured in their environment to find the rest.
     :inherit-configuration))

  ;; This puts all of the compiled fasl files into the build directory so that
  ;; a `make clean` will clear them out.
  (asdf:initialize-output-translations
   `(:output-translations
     :inherit-configuration
     (,(namestring root) ,(namestring asdf-cache)))))
