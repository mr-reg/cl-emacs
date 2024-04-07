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
RUN aptitude install -y make less sbcl procps apt-file net-tools clang libzmq5 libzmq5-dev
RUN curl -OL https://github.com/ocicl/ocicl/archive/refs/tags/v$OCICL_VERSION.tar.gz \
    && tar xf v$OCICL_VERSION.tar.gz \
    && cd ocicl-$OCICL_VERSION \
    && make \
    && make install
VOLUME /cl-emacs
COPY run-swank* /cl-emacs/
COPY src /cl-emacs/src
WORKDIR /cl-emacs
ENV PATH=/ccl/:/root/.local/bin/:$PATH
RUN ASDF_ONLY=1 ./run-swank.sh


