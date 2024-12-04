FROM --platform=linux/amd64 ubuntu:20.04

RUN apt-get update && apt-get install -y openjdk-17-jdk wget

RUN apt-get install -y libc6 libc6-dev libgtk-3-0 libxss1 libgconf-2-4 libnss3 libasound2 libxtst6 libxrender1 libxi6

RUN wget -c https://github.com/CARiSMA-Tool/carisma-tool/releases/download/build-20241114/carisma.product-build-20241114-linux.tar.gz -O /tmp/eclipse.tar.gz \
    && tar -zxvf /tmp/eclipse.tar.gz -C /opt/ \
    && rm /tmp/eclipse.tar.gz

RUN ln -s /opt/eclipse/eclipse /usr/local/bin/eclipse

RUN ls -l /opt/eclipse/eclipse

COPY . /CARiSMA-latest

WORKDIR /CARiSMA-latest

CMD ["eclipse"]