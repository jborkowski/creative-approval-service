FROM fpco/stack-build:lts-14.22 as build
RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build --system-ghc

FROM debian:buster-slim
RUN mkdir -p /opt/creative-approval-service
WORKDIR /opt/creative-approval-service

RUN apt-get update && apt-get install -y \
	ca-certificates \
	libgmp-dev

RUN mkdir -p /usr/local/share/ca-certificates/cortb && chmod 755 /usr/local/share/ca-certificates/cortb
COPY internal-cortb-ca.crt /usr/local/share/ca-certificates/cortb/internal-cortb-ca.crt
RUN chmod 644 /usr/local/share/ca-certificates/cortb/internal-cortb-ca.crt
RUN update-ca-certificates

COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-14.22/8.6.5/bin .
COPY config /opt/creative-approval-service/config
CMD ["/opt/creative-approval-service/reative-approval-service"]
