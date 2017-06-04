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
MAINTAINER="capgelka <ldyach@yandex.ru>"
DESCRIPTION="(haskell api diary) is a command line client for diary.ru api with interface in unix spirit. It supports most common actions available via API, and designed to make easy working with diary using powerfull unix utils."

stack install --install-ghc --stack-yaml stack.yaml --local-bin-path . hapidry


find $DIST -type d | xargs chmod 755
cp hapidry $DEST/bin/hapidry
cp build/first_time.sh $DEST/bin/hapidry-generate
cp man/hapidry.1 $DEST/share/man/man1/hapidry.1
gzip -9 -f $DEST/share/man/man1/hapidry.1

mkdir -p $DIST/build

cp build/postinst $DIST/build
cp build/postrm $DIST/build
cp build/changelog $DIST/build

if [ -n $(docker images | grep hapidry/fpm-builder)]; then
    pushd docker
    docker build -t hapidry/fpm-builder .
    popd
fi

for pack_type in deb rpm pacman
    do
        docker run --rm -v "${DIST}:/app" hapidry/fpm-builder \
            --input-type dir \
            --name     hapidry \
            --version "${VERSION}" \
            --package "/app/" \
            --architecture "${ARCHITECTURE}" \
            --maintainer   "${MAINTAINER}" \
            --description  "${DESCRIPTION}" \
            --deb-priority "${PRIORITY}" \
            --depends "${LIBC_DEPENDENCY}" \
            --depends "${GMP_DEPENDENCY}" \
            --category "${SECTION}" \
            --url "${URL}" \
            --after-install /app/build/postinst \
            --after-remove /app/build/postrm \
            --output-type $pack_type \
            --deb-changelog /app/build/changelog \
            --chdir /app \
            --force \
            usr
    done

rm -rf "${DIST}/usr"
rm -rf "${DIST}/build"
