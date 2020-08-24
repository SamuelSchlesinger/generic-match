# This is excessive and I accept that
GHC_MAJOR_VERSION=8
for GHC_MINOR_VERSIONS in 6.1 6.2 6.3 6.4 6.5 8.1 8.2 8.3 8.4 10.1 10.2
do
  GHC_VERSION="$GHC_MAJOR_VERSION.$GHC_MINOR_VERSIONS"
  echo "$GHC_VERSION"
  ghcup install "$GHC_VERSION"
  ghcup set ghc "$GHC_VERSION"
  cabal build
done
