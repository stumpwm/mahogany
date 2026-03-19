(in-package #:hrt)

(defun %build-callback-restarts (error-spec return-type)
  ;; I'm not sure if this is explicity needed with the :void return type,
  ;; but I'd rather not return junk data
  (macrolet ((when-val-present ((var place item) &body body)
               (let ((found-lst (gensym "FOUND-LST")))
                 `(alexandria:when-let ((,found-lst (member ,item ,place)))
                    (let ((,var (cadr ,found-lst)))
                      ,@body)))))
    (if (eql :void return-type)
        ;; To keep with non-void return type, establish a RETURN-VALUE restart,
        ;; except this one doesn't interactively take args
        (let ((restarts (list `(return-value ()
	                            :report "Exit the callback"
	                            nil))))
          (when-val-present (val error-spec :error-val)
            ;; let's be pendantic and require nil to make things a bit clearer:
            (unless (null val)
              (error "Default callback return value must be nil when the callback return type is :void"))
            (push `(default-value ()
	               :report "Return the default value from the callback (nil)"
	               nil)
                  restarts))
            restarts)
      (let ((restarts (list `(return-value ()
                                 :report "Exit the callback, returning the specified value"
                                 :interactive (lambda ()
		                                (format *query-io* "Return value: ")
                                                (force-output *query-io*)
		                                (list (read *query-io*)))))))
        (when-val-present (val error-spec :error-val)
          (push `(default-value ()
                     :report (lambda (s)
                               (format s "Return the value produced by evaluating ~S"
                                       (quote ,val)))
                     ,val)
                restarts))
        restarts))))

 (defmacro define-hrt-callback (name return-type args error-spec &body body)
   "Define a C callback that establishes restarts to exit the callback
cleanly.

ERROR-SPEC can be used to specify a default value that should be returned if
  there is an error. Use the RETURN-DEFAULT restart to make the callback
  return this value. Use it like (:error-val DEFAULT-VALUE). DEFAULT-VALUE will
  be evaluated to get the return value.

Restarts:
  RETURN-VALUE: Exits the callback, returning the provided value.
  ERROR-VALUE: Exits the callback, using a default value. Will not be present
    if ERROR-SPEC is not given."
   (multiple-value-bind (remaining-forms declarations doc-string)
       (alexandria:parse-body body :documentation t)
     `(cffi:defcallback ,name ,return-type ,args
        ,@(when doc-string
            (list doc-string))
        ,@declarations
        (restart-case (progn
                        ,@remaining-forms)
          ,@(%build-callback-restarts error-spec return-type)))))
