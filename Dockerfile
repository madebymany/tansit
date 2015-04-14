FROM ubuntu:14.04

ENV DEBIAN_FRONTEND noninteractive

RUN apt-key adv --keyserver 'keys.gnupg.net' --recv-keys '7F40EF0A' && \
    echo "deb http://apt.mxmdev.com unstable main" > /etc/apt/sources.list.d/apt.mxmdev.com.unstable.list && \
    apt-get update -q && \
    apt-get install -qy tansit python-pip ruby1.9.1 ruby1.9.1-dev zlib1g-dev libxml2-dev libxslt1-dev && \
    pip install awscli && \
    gem install deb-s3 --no-rdoc --no-ri

EXPOSE 49152

ENTRYPOINT ["tansit-kms", "--bind", "tcp://*:49152"]
