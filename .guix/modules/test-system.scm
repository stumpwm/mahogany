(define-module (test-system)
  #:use-module (guix gexp)
  #:use-module (gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu system)
  #:use-module (gnu tests)
  #:use-module (gnu services desktop)
  #:use-module (mahogany-package))

(operating-system
  (inherit %simple-os)
  (users (cons (user-account
                  (name "alice")
                  (comment "Bob's sister")
                  (group "users")
                  (supplementary-groups
                   '("wheel" "audio" "video"
                     ;; seatd needs the user to be part of seat group
                     "seat")))
                 %base-user-accounts))
  (packages
   (list coreutils ; Things like ls
         procps    ; For ps
         grep      ; For grep
         mahogany))
  (services
   (append
    (list
     (service seatd-service-type))
    %base-services)))
