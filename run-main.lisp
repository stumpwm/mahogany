(asdf:initialize-source-registry
 `(:source-registry
   (:directory ,(uiop/os:getcwd))
   (:tree ,(merge-pathnames (uiop/os:getcwd) #P"dependencies"))
   ;; Use whatever the user has configured in their environment to find the rest.
   :inherit-configuration))

(asdf:load-system "mahogany")

(mahogany::main)
