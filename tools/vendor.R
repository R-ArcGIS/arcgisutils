#!/usr/bin/env Rscript
# we need to vendor the dependencies and then store the checksum
rextendr::vendor_pkgs()

# make the checksum
writeLines(tools::md5sum("src/rust/vendor.tar.xz"), "./tools/vendor.md5")
