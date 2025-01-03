FROM ghcr.io/flipstone/haskell-tools:debian-stable-ghc-9.6.6-2024-12-10-9810a49

RUN apt-get update \
      && apt-get install -qq -y --no-install-recommends libphonenumber-dev libprotobuf-dev \
      && apt-get clean
    # && rm -rf /var/lib/apt/lists/*
