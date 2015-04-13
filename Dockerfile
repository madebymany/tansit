FROM ubuntu:14.04

ENV DEBIAN_FRONTEND noninteractive

RUN apt-key adv --keyserver 'keys.gnupg.net' --recv-keys '7F40EF0A' && \
    echo "deb http://apt.mxmdev.com unstable main" > /etc/apt/sources.list.d/apt.mxmdev.com.unstable.list && \
    apt-get update -q && \
    apt-get install -qy tansit python-pip && \
    pip install awscli

EXPOSE 49152

ENTRYPOINT ["tansit-kms", "--bind", "tcp://*:49152"]
