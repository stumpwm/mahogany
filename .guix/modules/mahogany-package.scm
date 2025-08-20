;;; Mahogany --- StumpWM-like window manager written for Wayland
;;; Copyright © 2023 Raven Hallsby <karl@hallsby.com>
;;;
;;; This file is part of Mahogany.
;;;

;;; Commentary:
;;
;; GNU Guix development package.  To build and install, run:
;;
;;   guix package -f guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix shell -D -f guix.scm
;;
;; A note from 2025. The instructions above are outdated.
;; For now, to run an old mahogany commit (from 2023),
;; you can use guix time-machine which will use an old
;; guix commit (also from 2023).
;;
;;   guix time-machine \
;;        --commit=e38d6a9c2fba815ac34e74baa843f15e33846813 \
;;        --channels=/path/to/your/guix/channels.scm \
;;        -- \
;;        build --file=guix.scm
;;
;; Your channels.scm should contain only main guix channel.
;; In my case it was https://codeberg.org/guix/guix .
;;
;;; Code:

(define-module (mahogany-package)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system asdf)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages wm))

(define-public mahogany
  (package
    (name "mahogany")
    (version "0.0.0")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/stumpwm/mahogany")
            ;; old mahogany commit (from 2023)
            (commit "8328c9cbe90978826f28ef39d835e8b8b2511b06")
            (recursive? #true)))
      (file-name (git-file-name name version))
      (sha256
       (base32 "03yhxs6srpjj7awfqss8dkalh7brhyr69rh115wixkf3w5r4mrvv"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list sbcl-fiasco
           sbcl-prove))
    (inputs
     (list mahogany-heart
           sbcl-xkbcommon
           libxkbcommon
           sbcl-cl-wayland
           sbcl-alexandria
           sbcl-cl-ansi-text
           sbcl-terminfo
           sbcl-snakes
           sbcl-iterate
           sbcl-cffi ;; Provides cffi-grovel
           wayland
           wayland-protocols
           wlroots))
    (outputs '("out" "lib"))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'delete-submodules
            (lambda* (#:key outputs #:allow-other-keys)
              (delete-file-recursively "./dependencies")
              (delete-file-recursively "./heart")))
          (add-after 'delete-submodules 'fix-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "lisp/bindings/hrt-libs.lisp"
                (("libheart.so")
                 (search-input-file inputs
                                    "/lib/libheart.so"))
                (("libwlroots.so")
                 (search-input-file inputs
                                    "/lib/libwlroots.so")))))
          (add-after 'create-asdf-configuration 'build-program
            (lambda* (#:key outputs #:allow-other-keys)
              (build-program
               (string-append (assoc-ref outputs "out") "/bin/mahogany")
               outputs
               #:entry-program '((mahogany::run-server) 0))))
          (add-after 'build-program 'create-desktop-file
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (xsessions (string-append out "/share/xsessions"))
                     (wayland-sessions (string-append out "/share/wayland-sessions")))
                (define (desktop-file file)
                  (format file
                     "[Desktop Entry]~@
                      Name=mahogany~@
                      Comment=The Mahogany Window Manager~@
                      Exec=~a/bin/mahogany~@
                      TryExec=~@*~a/bin/mahogany~@
                      Icon=~@
                      Type=Application~%"
                     out))
                (mkdir-p xsessions)
                (call-with-output-file
                    (string-append xsessions "/mahogany.desktop")
                  desktop-file)
                (mkdir-p wayland-sessions)
                (call-with-output-file
                    (string-append wayland-sessions "/mahogany.desktop")
                  desktop-file)))))))
    (synopsis "Window manager for Wayland written in Common Lisp")
    (description
     "Mahogany is a tiling window manager for Wayland modeled after StumpWM.
While it is not a drop-in replacement for stumpwm, stumpwm users should be
very comfortable with Mahogany.")
    (home-page "https://github.com/stumpwm/mahogany")
    (license license:gpl2+)))

(define-public mahogany-heart
  (package
    (name "mahogany-heart")
    (version (package-version mahogany))
    (source (package-source mahogany))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list wlroots
           libxkbcommon))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _ (chdir "heart")))
          (add-after 'chdir 'delete-submodules
            (lambda* (#:key outputs #:allow-other-keys)
              (delete-file-recursively "./subprojects"))))))
    (synopsis "An alternative C backend to a Wayland compositor to use with Mahogany")
    (description
     "Mahogany-heart's task is to setup the initial state of the
compositor, render the output, and initially handle new connections to
the compositor. If needed, the backend will also wrap some wlroots
functions so that less foreign code needs to be called from the other
language. is a tiling window manager for Wayland modeled after
StumpWM.")
    (home-page (package-home-page mahogany))
    (license (package-license mahogany))))

mahogany
