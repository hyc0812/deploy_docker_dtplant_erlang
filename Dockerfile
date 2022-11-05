# Build stage 0
FROM erlang:24-alpine

#Set working directory
RUN mkdir /data
WORKDIR /data

#Copy src, include folders and rebar.config
COPY src src/
# COPY include include/
COPY rebar.config .
#Build the release
RUN rebar3 release

FROM alpine

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
     apk add --no-cache libstdc++ && \
      apk add --no-cache libgcc

# Install the released application
COPY --from=0 /data/_build/default/rel/docker_ex /docker_ex

# Expose relevant ports
EXPOSE 8080
EXPOSE 8443

CMD ["/docker_ex/bin/docker_ex", "foreground"]