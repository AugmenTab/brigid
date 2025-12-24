FROM ghcr.io/flipstone/haskell-tools:debian-ghc-9.10.3-1e1d10c

RUN apt-get update \
      && apt-get install -qq -y --no-install-recommends libphonenumber-dev libprotobuf-dev \
      && apt-get clean
    # && rm -rf /var/lib/apt/lists/*
