FROM debian:stable AS build

# You can set the GHC version via a docker command line argument: --build-arg GHC_VERSION=9.4.6
ARG GHC_VERSION=9.6.7
ENV GHC_VERSION=${GHC_VERSION} \
    PATH="/root/.ghcup/bin:$PATH"

RUN apt update && \
    apt install -y --no-install-recommends \
    build-essential libffi-dev libffi8 libgmp-dev \ 
    libgmp10 libncurses-dev libncurses6 libtinfo6 pkg-config \
    zlib1g-dev libsqlite3-dev git bash curl ca-certificates 

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh && \
    ghcup install ghc --set ${GHC_VERSION} 

# Maybe improve with a svn checkout
COPY Tutorial.lhs tutorial.cabal cabal.project CHANGELOG.md LICENSE /workdir/
WORKDIR /workdir
RUN cabal install -f -logging --installdir=/workdir/

FROM httpd

COPY ./my-httpd.conf /usr/local/apache2/conf/httpd.conf
COPY --from=build "/workdir/tutorial" /usr/local/apache2/cgi-bin/tutorial.cgi
