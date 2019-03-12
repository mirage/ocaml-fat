### v0.12.3 (2019-03-11):
- port to dune from jbuilder (#79 @emillon)
- update opam metadata to 2.0 format (@avsm)
- test with OCaml 4.07 (@avsm)
- work with cstruct 1.7.0 by not using leading underscores
  in prefix fields (@avsm)

### 0.12.3 (2018-03-27):

* Autogenerate .travis.yml (#71 from @yomimono)
* Use Re.Str instead of deprecated Re_str (#72 from @ricarkol)
* Fix build with `-safe-string` (#73 from @djs55)

### 0.12.2 (2017-06-24):

* Include the `fat` command-line tool accidentally omitted in 0.12.1

### 0.12.1 (2017-06-17):

* Switch to jbuilder
* Add dependency on io-page-unix

### 0.12.0 (2017-01-20):

* Build against MirageOS 3 and drop support for previous versions.
* Use alcotest instead of ounit.
* Remove Fat_memoryIO and Fat.MemFS (if needed, use Mirage_block_lwt.Mem).
* Remove the IO_PAGE functor parameter.
* Remove Fat_KV_RO, which is now in mirage-fs.
* Use topkg instead of OASIS.

### 0.11.0 (09-Sep-2016):

* Use the ppx version of cstruct. (#44)

### 0.10.3 (10-Mar-2015):

* Add an explicit `connect` function to interfaces. (#39)
* MemoryIO.connect now takes an FS.t, not an FS.id. (#39)
* Use centralised Travis CI test scripts.
* Add local `opam` file for OPAM 1.2 pinning workflow.

### 0.10.2 (31-Jan-2015):

* Fixed destroy, which previously would overwrite the entry with
  uninitialised data, which might not set the deleted flag.
* Require only `get_buf` from `IO_PAGE`
* Fix travis.
* test: use OUnit's arg parser.
* return errors from size rather than raising Fs_error.

### 0.10.1 (01-Feb-2013):

* Use io-page.unix instead of io-page-unix.

### 0.10.0 (18-Dec-2013):

* speed up the 'fat' CLI tool by using buffered I/O by default.
  This requires mirage-block-unix.1.2.0

### 0.9.0 (15-Dec-2013):

* add `Fat.KV_RO` which is a read-only subset of the filesystem.
* Regenerate build files with OASIS 0.4.0.

### 0.8.0 (9-Dec-2013):

* cope with block devices whose sector size isn't 512 bytes
* allow 'fat create <filename>'

### 0.7.0 (9-Dec-2013):

* update to V1.BLOCK with 'id' interface
* fix 'make uninstall' (doesn't fail if configure hasn't
  been run)

### 0.6.1 (8-Dec-2013)

* install the 'fat' CLI tool by default

### 0.6.0 (7-Dec-2013)

* add a command-line tool for maniplating images ('fat')
* functorise over the mirage V1 BLOCK_DEVICE and IO_PAGE
* implements the mirage V1 type FS

### 0.5.0 (2-Dec-2013)

* Initial version of fat-filesystem
