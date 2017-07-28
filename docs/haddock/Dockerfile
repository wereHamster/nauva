FROM haskell:8.0.2
MAINTAINER Tomas Carnecky <tomas.carnecky@gmail.com>

# Keep this same as the resolver in the stack config files. Otherwise
# builds will take forever.
ENV RESOLVER nightly-2017-07-25

RUN stack --no-terminal --color=never --resolver=$RESOLVER --install-ghc install --haddock \
    text aeson snap-core blaze-markup blaze-html mtl tagged haskell-src-meta \
    conduit th-lift-instances yaml siphash snap-blaze file-embed data-default \
    websockets-snap snap-server \
    ansi-terminal terminal-size fsnotify unix cmdargs extra css-text parsec \
    network-uri conduit-extra tagsoup utf8-string xml-types xml-conduit xss-sanitize \
    markdown

ENTRYPOINT ["stack"]
