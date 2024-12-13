FROM --platform=linux/amd64 ubuntu:20.04

# Run command on the image to install Java, openjdk is required although "shipped with the product" (already checked)
RUN apt-get update && apt-get install -y openjdk-21-jdk wget

RUN apt-get install -y libc6 libc6-dev libgtk-3-0 libxss1 libgconf-2-4 libnss3 libasound2 libxtst6 libxrender1 libxi6 libgl1-mesa-glx libgl1-mesa-dri

RUN apt-get clean

COPY deployment/carisma.product/target/products/carisma-product-linux.gtk.x86_64.tar.gz /tmp/eclipse.tar.gz
	
RUN mkdir /opt/carisma

RUN tar -zxvf /tmp/eclipse.tar.gz -C /opt/carisma \
    && rm /tmp/eclipse.tar.gz

CMD ["/opt/carisma/carisma-launcher"]
