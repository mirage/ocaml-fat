Pure OCaml implementation of the FAT filesystem
===============================================

This library has two purposes:
  1. primary: to allow the easy preparation of bootable disk images
     containing [Mirage](http://openmirage.org/) kernels
  2. secondary: to provide a simple key=value store for Mirage
     applications

Note that "filesystems" are inherently legacy systems which modern
Mirage applications will not use directly. The most likely use for this
library is in booting a Mirage application via some kind of disk image.
