# syntax=docker/dockerfile:1
 
FROM --platform=linux/amd64 node:18
WORKDIR /app

COPY src src
COPY spago.dhall spago.dhall

WORKDIR example/minimal

RUN npm install -g purescript@0.15.8
RUN npm install -g spago@0.21.0

ARG HYDRA_VERSION=1.2.0
RUN curl -LO https://github.com/input-output-hk/hydra/releases/download/${HYDRA_VERSION}/hydra-x86_linux-${HYDRA_VERSION}.zip
RUN unzip -d /usr/local/bin/ hydra-x86_linux-${HYDRA_VERSION}.zip
RUN chmod +x /usr/local/bin/hydra-node

COPY example/minimal/package.json example/minimal/package-lock.json .
RUN npm clean-install --production=false --loglevel=verbose

COPY example/minimal/packages.dhall example/minimal/spago.dhall .
RUN wget -O package-set.dhall "$(grep -oP 'https://raw[^ ]+' packages.dhall)"
RUN sed -i 's|https://raw[^ ]*|./package-set.dhall|' packages.dhall
RUN spago install

COPY example/minimal/src src
COPY example/minimal/app app
COPY example/minimal/protocol-parameters.json .
COPY example/minimal/blockfrost.txt .
RUN spago build

ENTRYPOINT [ "spago", "-q", "run", "--main", "HydraSdk.Example.Minimal.Main", "--exec-args"]
CMD ["--help"]
