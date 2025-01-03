FROM ghcr.io/flipstone/haskell-tools:debian-stable-ghc-9.4.7-2023-10-31-3286ef4

RUN apt-get update \
      && apt-get install -qq -y --no-install-recommends libphonenumber-dev libprotobuf-dev \
      && apt-get clean \
      && rm -rf /var/lib/apt/lists/*
