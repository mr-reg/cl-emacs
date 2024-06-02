FROM debian:bookworm
RUN apt-get update
RUN apt-get install -y aptitude
RUN aptitude install -y curl
ENV CCL_VERSION=1.12.2
RUN curl -OL https://github.com/Clozure/ccl/releases/download/v$CCL_VERSION/ccl-$CCL_VERSION-linuxx86.tar.gz \
    && tar xf ccl-$CCL_VERSION-linuxx86.tar.gz \
    && cd ccl \
    && curl -OL https://github.com/Clozure/ccl/releases/download/v$CCL_VERSION/linuxx86.tar.gz \
    && tar xf linuxx86.tar.gz
ENV OCICL_VERSION=1.0.19
RUN aptitude update \
    && aptitude install -y make less sbcl procps apt-file net-tools clang git
COPY src/docker/ocicl-patch.lisp /tmp/ocicl-patch.lisp

RUN curl -OL https://github.com/ocicl/ocicl/archive/refs/tags/v$OCICL_VERSION.tar.gz \
    && tar xf v$OCICL_VERSION.tar.gz \
    && cd ocicl-$OCICL_VERSION \
    && sed -i 's/defun find-asd-files/defun old-find-asd-files/g' ocicl.lisp \
    && cat /tmp/ocicl-patch.lisp >> ocicl.lisp \
    && make \
    && make install
RUN mkdir /cl-emacs \
    && mv /root/.local/bin/ocicl /root/.local/bin/ocicl-bin
COPY src/docker/ocicl /root/.local/bin/ocicl
RUN chmod +x /root/.local/bin/ocicl

ENV ASDF_VERSION=3.3.6.7 \
    ASDF_DIR=/asdf
RUN git clone --depth 1 -b $ASDF_VERSION https://gitlab.common-lisp.net/asdf/asdf.git /asdf \
    && cd /asdf \
    && make
COPY src/docker/ccl-init.lisp /root/ccl-init.lisp
COPY run-swank* /cl-emacs/
#/COPY src /cl-emacs/src
WORKDIR /cl-emacs
ENV PATH=/ccl/:/root/.local/bin/:$PATH

# RUN ASDF_ONLY=1 ./run-swank.sh
