FROM ghcr.io/cross-rs/armv7-unknown-linux-gnueabihf:edge

RUN apt-get update
RUN dpkg --add-architecture armhf && \
    apt-get update && \
    apt-get install -y libasound2-dev:armhf

ENV PKG_CONFIG_LIBDIR_armv7_unknown_linux_gnueabihf=/usr/lib/arm-linux-gnueabihf/pkgconfig
