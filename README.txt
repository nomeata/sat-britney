SAT-Britney
~~~~~~~~~~

© 2011 Joachim Breitner <nomeata@debian.org>

This package is distributed under the GPL version 2, see LICENSE.txt for
details.


About SAT-Britney
~~~~~~~~~~~~~~~~~

SAT-Britney is a tool meant to aide the migration of Debian packages from the
unstable to the testing repository, by identifying sets of packages that need
to migrate together. It does so by formulating all the requirements about the
migration as a large boolean formula, and then uses SAT and PMAX-SAT solvers to
determine the migrations.

Usage
~~~~~

For details on the flags, please consult SAT-Britney --help output. You need to
enable at lest one of the various outputs (e.g. --hints), otherwise you are
burning CPU cycles for nothing.

The directory passed through the --dir flag has look like a britney2 directory
file, e.g. /srv/release.debian.org/britney/var/data-b2/. In particular, the
following files are expected:

./testing
./testing/BugsV
./testing/Dates
./testing/Packages_amd64
./testing/Packages_i386
./testing/Packages_[..]
./testing/Urgency
./unstable
./unstable/BugsV
./unstable/Packages_amd64
./unstable/Packages_i386
./unstable/Packages_[..]
./unstable/Sources


If you see the error message
	mkTextEncoding: invalid argument (Invalid argument)
then try setting GCONV_PATH=/usr/lib64/gconv; such errors occur due to static
linking of the binary.

Supported SAT-Solvers
~~~~~~~~~~~~~~~~~~~~~

For SAT problems, picosat is always used. For the PMAX-SAT problems, it can
interfaces with msuncore, MiniMaxSat and clasp, of which only the latter is
Free Software. The selection is done in the file Picosat.hs.


Hacking
~~~~~~~

SAT-Britney is written in Haskell and developed on Debian unstable. To deploy
it, just copy the statically linked binary onto the machine you want to use it.
The sources can be obtained from git://git.nomeata.de/sat-britney.git and
viewed at http://git.nomeata.de/?p=sat-britney.git


Deficiencies, Bugs and TODOs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 * It might be interesting to run edost-distchek before and after our runs, to
   find identify wrongly migrated packages.
 * It might be more efficient to re-parse the Packages file and generate and
   resolve dependencies only per arch.


Failing test suite tests
~~~~~~~~~~~~~~~~~~~~~~~~

In the test suite at
http://anonscm.debian.org/gitweb/?p=collab-maint/britney-tests.git;a=summary
there are some tests that fail for known reasons in SAT-Britney. Here is an
explanation of these (at 596358c05b64dc1c3c13aeceaa2ba04c41827642):

 * Some hints are not yet supported:
   basic-block-arch-hint
   basic-force-hint
   basic-tpu-approve-hint
   basic-unblock-hint
   basic-unblock-multiple-types

Successful hints since SAT-Britney went live:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
2012.03.28 22:03:48: libgpiv/0.6.1-4 pygpiv/2.0.0-4
2012.03.31 22:03:55: pytables/2.3.1-3 tessa/0.3.1-6
2012.04.02 22:03:48: libgeotiff-dfsg/1.3.0+dfsg-3 liblas/1.2.1-5
2012.04.16 10:03:46: octave-ocs/0.1.3-1 octave-odepkg/0.8.0-1
2012.04.16 10:03:46: octave-communications/1.1.0-2 octave-control/2.3.50-1 octave-data-smoothing/1.3.0-2 octave-econometrics/1:1.0.8-6 octave-financial/0.3.2-3 octave-ga/0.9.8-3 octave-miscellaneous/1.1.0-1 octave-optim/1.0.17-2 octave-signal/1.1.2-1 octave-specfun/1.1.0-1 octave-statistics/1.1.0-1 octave-struct/1.0.9-2 octave-time/1.0.9-4 octave-vrml/1.0.11-2
2012.05.07 10:04:04: react/0.9.3-1 xen-api/1.3.2-5 xmlm/1.1.0-1 js-of-ocaml/amd64/1.1.1-1 js-of-ocaml/armel/1.1.1-1 js-of-ocaml/i386/1.1.1-1 js-of-ocaml/kfreebsd-amd64/1.1.1-1 js-of-ocaml/kfreebsd-i386/1.1.1-1 js-of-ocaml/powerpc/1.1.1-1 js-of-ocaml/sparc/1.1.1-1 obrowser/amd64/1.1.1+dfsg-1 obrowser/armel/1.1.1+dfsg-1 obrowser/i386/1.1.1+dfsg-1 obrowser/kfreebsd-amd64/1.1.1+dfsg-1 obrowser/kfreebsd-i386/1.1.1+dfsg-1 obrowser/powerpc/1.1.1+dfsg-1 obrowser/sparc/1.1.1+dfsg-1 ocaml-lastfm/amd64/0.3.0-2 ocaml-lastfm/armel/0.3.0-2 ocaml-lastfm/i386/0.3.0-2 ocaml-lastfm/kfreebsd-amd64/0.3.0-2 ocaml-lastfm/kfreebsd-i386/0.3.0-2 ocaml-lastfm/powerpc/0.3.0-2 ocaml-lastfm/sparc/0.3.0-2 lwt/amd64/2.3.2-1 lwt/armel/2.3.2-1 lwt/i386/2.3.2-1 lwt/kfreebsd-amd64/2.3.2-1 lwt/kfreebsd-i386/2.3.2-1 lwt/powerpc/2.3.2-1 lwt/sparc/2.3.2-1 obus/amd64/1.1.3-1 obus/armel/1.1.3-1 obus/i386/1.1.3-1 obus/ia64/1.1.3-1 obus/kfreebsd-amd64/1.1.3-1 obus/kfreebsd-i386/1.1.3-1 obus/mips/1.1.3-1 obus/mipsel/1.1.3-1 obus/powerpc/1.1.3-1 obus/s390/1.1.3-1 obus/sparc/1.1.3-1 ocsigen/amd64/1.3.4-2 ocsigen/armel/1.3.4-2 ocsigen/i386/1.3.4-2 ocsigen/kfreebsd-amd64/1.3.4-2 ocsigen/kfreebsd-i386/1.3.4-2 ocsigen/powerpc/1.3.4-2 ocsigen/sparc/1.3.4-2 ocsigenserver/amd64/2.0.4-1 ocsigenserver/armel/2.0.4-1 ocsigenserver/i386/2.0.4-1 ocsigenserver/kfreebsd-amd64/2.0.4-1 ocsigenserver/kfreebsd-i386/2.0.4-1 ocsigenserver/powerpc/2.0.4-1 ocsigenserver/sparc/2.0.4-1 ocaml-sqlexpr/amd64/0.4.1-1 ocaml-sqlexpr/armel/0.4.1-1 ocaml-sqlexpr/i386/0.4.1-1 ocaml-sqlexpr/kfreebsd-amd64/0.4.1-1 ocaml-sqlexpr/kfreebsd-i386/0.4.1-1 ocaml-sqlexpr/powerpc/0.4.1-1 ocaml-sqlexpr/sparc/0.4.1-1 ocaml-usb/amd64/1.2.0-2 ocaml-usb/armel/1.2.0-2 ocaml-usb/i386/1.2.0-2 ocaml-usb/kfreebsd-amd64/1.2.0-2 ocaml-usb/kfreebsd-i386/1.2.0-2 ocaml-usb/powerpc/1.2.0-2 ocaml-usb/sparc/1.2.0-2 xen-api-libs/amd64/0.5.2-3 xen-api-libs/i386/0.5.2-3 ocaml-xmlplaylist/amd64/0.1.3-1 ocaml-xmlplaylist/armel/0.1.3-1 ocaml-xmlplaylist/i386/0.1.3-1 ocaml-xmlplaylist/ia64/0.1.3-1 ocaml-xmlplaylist/kfreebsd-amd64/0.1.3-1 ocaml-xmlplaylist/kfreebsd-i386/0.1.3-1 ocaml-xmlplaylist/mips/0.1.3-1 ocaml-xmlplaylist/mipsel/0.1.3-1 ocaml-xmlplaylist/powerpc/0.1.3-1 ocaml-xmlplaylist/s390/0.1.3-1 ocaml-xmlplaylist/sparc/0.1.3-1 nurpawiki/amd64/1.2.3-5 nurpawiki/armel/1.2.3-5 nurpawiki/i386/1.2.3-5 nurpawiki/kfreebsd-amd64/1.2.3-5 nurpawiki/kfreebsd-i386/1.2.3-5 nurpawiki/powerpc/1.2.3-5 nurpawiki/sparc/1.2.3-5
2012.05.07 22:03:49: w3c-dtd-xhtml/1.2-3 w3c-sgml-lib/1.2-3
2012.05.09 10:04:04: playonlinux/4.0.14-1 winetricks/0.0+20120313+svn798-3
