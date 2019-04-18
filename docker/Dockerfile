# Use the existing Haskell image as our base
FROM ubuntu:16.04

ADD . /Funkadelic/

RUN apt-get update && \
    apt-get install -y curl && \
    curl -sSL https://get.haskellstack.org/ | sh

WORKDIR Funkadelic

RUN stack build && \
    stack test
