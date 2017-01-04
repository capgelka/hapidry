#!/bin/bash
MACHINE=$(uname -m)
case "$MACHINE" in
  x86_64) ARCHITECTURE=amd64;;
  i686)   ARCHITECTURE=i386;;
  i386)   ARCHITECTURE=i386;;
esac

LOCAL=$HOME/.local
VERSION=$(grep -e '^version' hapidry.cabal | awk '{print $2}' | xargs)
DEBVER=$VERSION
BASE=hapidry-$DEBVER-$ARCHITECTURE
DIST=`pwd`/$BASE
DEST=$DIST/usr
ME=$(whoami)
COPYRIGHT=$DEST/share/doc/hapidry/copyright

# We need this for hsb2hs:
PATH=$LOCAL/bin:$PATH

mkdir -p $DEST/bin
mkdir -p $DEST/share/man/man1
mkdir -p $DEST/share/doc/hapidry
mkdir -p $DEST/etc/bash_completion.d

cp debian/hapidry $DEST/


stack install --install-ghc --stack-yaml stack.yaml --local-bin-path . hsb2hs
stack install --install-ghc --stack-yaml stack.yaml --local-bin-path . hapidry


find $DIST -type d | xargs chmod 755
cp $HOME/.local/bin/hapidry $DEST/bin/
cp man/hapidry.1 $DEST/share/man/man1/hapidry.1
gzip -9 $DEST/share/man/man1/hapidry.1

INSTALLED_SIZE=$(du -B 1024 -s $DEST | awk '{print $1}')
mkdir -p $DIST/DEBIAN
perl -pe "s/VERSION/$DEBVER/" debian/control.in | \
  perl -pe "s/ARCHITECTURE/$ARCHITECTURE/" | \
  perl -pe "s/INSTALLED_SIZE/$INSTALLED_SIZE/" \
  > $DIST/DEBIAN/control
cp debian/postinst $DIST/DEBIAN/
cp debian/postrm $DIST/DEBIAN/
cp debian/templates $DIST/DEBIAN/
cp debian/config $DIST/DEBIAN/
echo $DIST

cat $DIST/DEBIAN/control

tree $DIST

fakeroot dpkg-deb --build $DIST
rm -rf $DIST
