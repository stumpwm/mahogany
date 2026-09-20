# Use the latest Fedora base image
FROM fedora:latest

ARG TARGETARCH

# Update repositories and install the requested packages
RUN dnf update -y && dnf install -y \
    git \
    dnf-plugins-core \
    redhat-rpm-config \
    sbcl \
    curl \
    make \
    wlroots-devel \
    cairo-devel \
    pango-devel \
    && dnf clean all

RUN dnf -y builddep wlroots

RUN set -x && \
    if [ "$TARGETARCH" = "amd64" ]; then \
        CCL_ARCH="linuxx86"; \
        BINARY_NAME="lx86cl64"; \
    elif [ "$TARGETARCH" = "arm64" ]; then \
        CCL_ARCH="linuxarm"; \
        BINARY_NAME="larmcl64"; \
    else \
        echo "Unsupported architecture: $TARGETARCH" && exit 1; \
    fi && \
    # Download the release archive containing the pre-compiled kernel/heap
    curl -L -o /tmp/ccl.tar.gz "https://github.com/Clozure/ccl/releases/download/v1.13/ccl-1.13-${CCL_ARCH}.tar.gz" && \
    # Extract directly into /usr/local/share
    tar -C /usr/local/share -xzf /tmp/ccl.tar.gz && \
    rm /tmp/ccl.tar.gz && \
    # Create a symlink in /usr/local/bin to map the correct binary to 'ccl'
    ln -s "/usr/local/share/ccl/${BINARY_NAME}" /usr/local/bin/ccl

# Now, install quicklisp:
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp \
    && sbcl --noinform --load quicklisp.lisp --eval "(quicklisp-quickstart:install)"

RUN sbcl --noinform --load "$HOME/quicklisp/setup.lisp" \
    --eval "(progn (setf ql-util::*do-not-prompt* t)(ql:add-to-init-file) (quit))"

RUN ccl -l "$HOME/quicklisp/setup.lisp" -e "(progn (setf ql-util::*do-not-prompt* t)(ql:add-to-init-file) (quit))"

# Set a default working directory for the CI pipeline
WORKDIR /workspace

# Default command (can be overridden by your CI runner)
CMD ["/bin/bash"]
