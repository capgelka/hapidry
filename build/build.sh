#!/bin/bash

MACHINE=$(uname -m)
case "$MACHINE" in
  x86_64) ARCHITECTURE=amd64;;
  i686)   ARCHITECTURE=i386;;
  i386)   ARCHITECTURE=i386;;
esac

VERSION=$(grep -e '^version' hapidry.cabal | awk '{print $2}' | xargs)
BASE="hapidry-${VERSION}-${ARCHITECTURE}"
DIST=`pwd`/$BASE

DEST=$DIST/usr


mkdir -p $DEST/bin
mkdir -p $DEST/share/man/man1
mkdir -p $DEST/share/doc/hapidry

LIBC_DEPENDENCY="libc6 >= 2.13"
GMP_DEPENDENCY=libgmp10
URL="https://github.com/capgelka/hapidry"
PACKAGE=$(grep -e '^name' hapidry.cabal | awk '{print $2}' | xargs)
SECTION=net
PRIORITY=optional
# Architecture: ARCHITECTURE
# Installed-Size: INSTALLED_SIZE
# Depends: libc6 (>= 2.13), libgmp10
MAINTAINER="capgelka <ldyach@yandex.ru>"
DESCRIPTION="(haskell api diary) is a command line client for diary.ru api with interface in unix spirit. It supports most common actions available via API, and designed to make easy working with diary using powerfull unix utils."

stack install --install-ghc --stack-yaml stack.yaml --local-bin-path . hapidry


find $DIST -type d | xargs chmod 755
cp hapidry $DEST/bin/hapidry
cp build/first_time.sh $DEST/bin/hapidry-generate
cp man/hapidry.1 $DEST/share/man/man1/hapidry.1
gzip -9 -f $DEST/share/man/man1/hapidry.1

# INSTALLED_SIZE=$(du -B 1024 -s $DEST | awk '{print $1}')
# mkdir -p $DIST/build
# perl -pe "s/VERSION/$VERSION/" build/control.in | \
#   perl -pe "s/ARCHITECTURE/$ARCHITECTURE/" | \
#   perl -pe "s/INSTALLED_SIZE/$INSTALLED_SIZE/" \
#   > $DIST/build/control
# cp build/postinst $DIST/build/
# cp build/postrm $DIST/build/
# cp build/changelog $DIST/build/

# container=$(docker create tenzer/fpm:latest)
# docker cp $DEST "${container}:${DEST}"
# ls -l $DEST
# docker start $container- i ls -l $DEST
for pack_type in deb rpm pacman
    do
        docker run --rm -v "${DIST}:/app" tenzer/fpm:latest \
            --input-type dir \
            --name     hapidry \
            --version "${VERSION}" \
            --package /app \
            --architecture "${ARCHITECTURE}" \
            --maintainer   "${MAINTAINER}" \
            --description  "${DESCRIPTION}" \
            --deb-priority "${PRIORITY}" \
            --depends "${LIBC_DEPENDENCY}" \
            --depends "${GMP_DEPENDENCY}" \
            --category "${SECTION}" \
            --url "${URL}" \
            --after-install build/postinst \
            --after-remove build/postrm \
            --output-type $pack_type \
            --deb-changelog build/changelog \
            # --verbose \
            # --debug \
            --chdir /app \
            # --workdir app  \
            /app
            # "${DIST}"
    done
# docker rm $container

# fakeroot dpkg-deb --build $DIST
# rm -rf $dist
