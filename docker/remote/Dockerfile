# This docker file you create a ubuntu container that you can connect to via ssh and start CARiSMA from a remote server

FROM --platform=linux/amd64 docker.io/library/ubuntu:24.04

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
COPY ./authorized_keys /root/.ssh/authorized_keys
RUN chmod 600 /root/.ssh/authorized_keys
EXPOSE 22

# clean up
RUN apt-get clean

# install carisma
RUN mkdir /opt/carisma \
    && wget -c https://github.com/CARiSMA-Tool/carisma-tool/releases/latest/download/carisma-product-linux.gtk.x86_64.tar.gz -O /tmp/eclipse.tar.gz \
    && tar -zxf /tmp/eclipse.tar.gz -C /opt/carisma \
    && rm /tmp/eclipse.tar.gz

# start SSH daemon
CMD ["/usr/sbin/sshd", "-D"]
