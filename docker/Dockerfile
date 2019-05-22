# Use the existing Haskell image as our base
FROM ubuntu:16.04

ADD . /Funkadelic/

RUN apt-get update && \
    apt-get install -y curl && \
    curl -sSL https://get.haskellstack.org/ | sh && \
    curl -sL https://deb.nodesource.com/setup_10.x | bash - && \
    apt-get install -y nodejs && \
    npm install adt
    
WORKDIR Funkadelic

RUN stack build
