# This docker file you create a ubuntu container that you can connect to via ssh and start CARiSMA from a remote server

ARG UBUNTU_VERSION="24.04"
FROM --platform=linux/amd64 docker.io/library/ubuntu:${UBUNTU_VERSION}

# Run command on the image to install Java, openjdk is required although "shipped with the product" (already checked)
RUN apt-get -y update && \
    apt-get install -y \
    openssh-server \
    openjdk-21-jdk \
    wget \
    libc6 libgtk-3-0 libxss1 libnss3 libxtst6 libxrender1 libxi6 libgl1-mesa-dri

# Configure SSH
RUN mkdir /var/run/sshd
RUN sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config
RUN sed -i 's/#PasswordAuthentication yes/PasswordAuthentication no/' /etc/ssh/sshd_config
RUN sed -i 's/#PubkeyAuthentication yes/PubkeyAuthentication yes/' /etc/ssh/sshd_config
RUN sed -i 's/#X11UseLocalhost yes/X11UseLocalhost no/' /etc/ssh/sshd_config
EXPOSE 22

# clean up
RUN apt-get clean

# install CARiSMA
ARG CARISMA_RELEASE_URL="https://github.com/CARiSMA-Tool/carisma-tool/releases/latest/download/carisma-product-linux.gtk.x86_64.tar.gz"
RUN mkdir /opt/carisma \
    && wget -c ${CARISMA_RELEASE_URL} -O /tmp/eclipse.tar.gz \
    && tar -zxf /tmp/eclipse.tar.gz -C /opt/carisma \
    && rm /tmp/eclipse.tar.gz

# start SSH daemon
CMD ["/usr/sbin/sshd", "-D", "-e"]
