# syntax=docker/dockerfile:1
 
FROM node:18
WORKDIR /app

COPY package.json package-lock.json .
RUN npm clean-install --production=false --loglevel=verbose
RUN npm install purescript@0.15.8
RUN npm install spago@0.21.0

COPY packages.dhall spago.dhall .
RUN npx --no-install spago install

COPY src src
COPY app app
COPY protocol-parameters.json .
RUN npx --no-install spago build

RUN curl -LO https://github.com/input-output-hk/hydra/releases/download/0.19.0/hydra-x86_64-linux-0.19.0.zip
RUN unzip -d /usr/local/bin/ hydra-x86_64-linux-0.19.0.zip
RUN chmod +x /usr/local/bin/hydra-node

ENTRYPOINT ["npx", "--no-install", "spago", "-q", "run", "--main", "HydraSdk.Example.Minimal.Main", "--exec-args"]
CMD ["--help"]
