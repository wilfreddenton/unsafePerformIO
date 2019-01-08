FROM ubuntu:18.04

RUN apt update
RUN apt install -y curl libgpgme-dev

RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack update

COPY unsafePerformIO.cabal .
COPY stack.yaml .

RUN stack install --only-dependencies

COPY . unsafePerformIO
WORKDIR unsafePerformIO

RUN stack test
RUN stack install

EXPOSE 8080
ENTRYPOINT /root/.local/bin/unsafe-perform-io --port 8080 \
                                              --sqlite prod.db \
                                              --init-sql init.sql \
                                              --gnupg-homedir .gnupg \
                                              --pgp-public-key wilfred.gpg

