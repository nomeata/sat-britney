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

 * Conflicts between dependencies are not handled to their full extend. In
   particular, Conflicts with an upper version bound and Breaks are enforced on
   the package level, i.e. conflicting package are not even allowed to enter
   testing, even if nothing depends on both, while unversioned Conflicts are
   ignored.
   It is not yet clear whether a proper encoding is still solvable in
   acceptable time, or whether this issue will stick around.
 * The dependencies of arch:all packages are only checked on i386, just as with
   britney itself. But then SAT-Britney assumes it is installable on all other
   architectures as well, which is not exactly what we want.
   A possible solution would treat an arch:all package as separate packages on
   each arch.
 * It might be interesting to run edost-distchek before and after our runs, to
   find identify wrongly migrated packages.
 * SAT-Britney does not read the existing hint files for britney and thus
   suggests to migrate packages that are blocked.
