FROM fedora:latest

ENV HOME /root/

RUN dnf -y install dnf-plugins-core sbcl curl make redhat-rpm-config
RUN dnf -y builddep wlroots

RUN curl -O https://beta.quicklisp.org/quicklisp.lisp \
    && sbcl --noinform --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" \
    && sbcl --noinform --load "/root/quicklisp/setup.lisp"  --eval "(progn (setf ql-util::*do-not-prompt* t)(ql:add-to-init-file))" \
    && sbcl --noinform --eval "(ql:quickload '("alexandria" "cl-ansi-text" "terminfo" "snakes" "iterate" "cffi" "cffi-grovel" "closer-mop"))" \
    && sbcl --noinform --eval "(ql:quickload '("prove" "fiasco"))" 

COPY . .

# RUN git submodule init
RUN make test
