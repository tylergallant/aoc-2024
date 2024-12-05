FROM haskell:9.4.8
RUN ln -s $(which ghc) /usr/bin/ghc-9.4.8
WORKDIR /usr/aoc-2024

COPY aoc2024.cabal ./
COPY cabal.project ./
RUN cabal update
RUN cabal build --only-dependencies

COPY . .
RUN cabal build

ENTRYPOINT [ "./day.sh" ]
