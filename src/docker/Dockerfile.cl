FROM debian:10
RUN apt update
RUN apt install -y git clang
RUN apt install -y autoconf make texinfo 'libgnutls*-dev'
RUN apt install -y pkg-config mailutils xorg-dev libjpeg-dev libgif-dev libtiff-dev
RUN git clone --depth 1 --single-branch -b cl-emacs-29.1 https://github.com/mr-reg/emacs
WORKDIR /emacs
RUN ./autogen.sh
RUN CC=clang CFLAGS="-fPIC -g -gdwarf-4 -rdynamic" ./configure \
   --without-json \
   --without-xwidgets \
   --with-file-notification=inotify \
   --without-native-compilation \
   --with-included-regex \
   --without-xaw3d \
   --with-modules \
   --with-dumping=pdumper \
   --with-cairo \
   --without-gpm \
   --with-x-toolkit=no \
   --without-rsvg \
   --without-threads
ENV THREADS=12
RUN make -j$THREADS
WORKDIR /emacs/test
RUN make -j$THREADS check || echo errors ignored
RUN find . -name '*.log' -exec grep -Po "...unexpected" {} + | grep -v ' 0 unexpected' 
