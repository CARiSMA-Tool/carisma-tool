# Make sure you have some X server running and that X connections are allowed (run `xhost +`)
FROM --platform=linux/amd64 docker.io/library/ubuntu:24.04

# Install Java, wget, openjdk. OpenJDK is required although "shipped with the product" (already checked)
RUN apt-get -y update && \
    apt-get install -y \
    openssh-server openjdk-21-jdk wget libc6 libgtk-3-0 libxss1 libnss3 libxtst6 libxrender1 libxi6 libgl1-mesa-dri
# are the following packages required? they could not be found in ubuntu 24.04: libgl1-mesa-glx gconf2 libasound2 libc6-dev

# clean up
RUN apt-get clean

# install carisma
RUN mkdir /opt/carisma \
    && wget -c https://github.com/CARiSMA-Tool/carisma-tool/releases/latest/download/carisma-product-linux.gtk.x86_64.tar.gz -O /tmp/eclipse.tar.gz \
    && tar -zxf /tmp/eclipse.tar.gz -C /opt/carisma \
    && rm /tmp/eclipse.tar.gz

# immediately launch carisma
CMD ["/opt/carisma/carisma-launcher"]
