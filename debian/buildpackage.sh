#! /bin/sh

TMPDIR=PACKAGES

VERSION=`grep -e "^VERSION =" version.make | sed "s/^.*= //"`
mkdir -p "$TMPDIR"
darcs get . "$TMPDIR/objective-cl-${VERSION}"
cd "$TMPDIR"
rm -rf `find objective-cl-${VERSION} -name '_darcs'`
cp -a ../Documentation "objective-cl-${VERSION}/"
cd "objective-cl-${VERSION}"
autoreconf
chmod +x debian/rules
cd ..
tar czf "cl-objective-cl_${VERSION}.orig.tar.gz" "objective-cl-${VERSION}"
cd "objective-cl-${VERSION}"
dpkg-buildpackage -rfakeroot
cd ..
rm -rf "objective-cl-${VERSION}"
cd ..
