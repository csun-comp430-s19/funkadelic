# Use the existing Haskell image as our base
FROM ubuntu:16.04

RUN apt-get update && \
    apt-get install -y curl && \
    curl -sSL https://get.haskellstack.org/ | sh && \
    cd && mkdir .stack
