FROM ubuntu:18.04 as builder

RUN apt-get update
RUN apt-get install -y curl libgpgme-dev libpcre3-dev

RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack update

COPY unsafePerformIO.cabal .
COPY stack.yaml .

RUN stack install --only-dependencies

COPY . unsafePerformIO
WORKDIR unsafePerformIO

RUN stack test
RUN stack install


FROM ubuntu:18.04

RUN apt-get update
RUN apt-get install -y libgpgme-dev libpcre3-dev

COPY --from=0 /unsafePerformIO .
COPY --from=0 /root/.local/bin/unsafe-perform-io /unsafePerformIO/

EXPOSE 8080
ENTRYPOINT /unsafePerformIO/unsafe-perform-io --port 8080 \
                                              --sqlite db/prod.db \
                                              --init-sql init.sql \
                                              --gnupg-homedir .gnupg \
                                              --pgp-public-key wilfred.gpg
