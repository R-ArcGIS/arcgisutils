> If your configure script needs auxiliary files, it is recommended that you ship them in a tools directory (as R itself does).

This directory is used to handle vendoring of dependencies.

`tools/vendor.R` will use `rextendr::vendor_pkgs()` to vendor Rust crates and create a `vendor.md5` file wich stores the checksum of the vendored file. If `vendor.tar.xz` is larger than 5mb, it should be stored remotely and downloaded as needed. 

`tools/get-deps.R` will download the `vendor.tar.xz` from GitHub and then check the md5. If it doesn't match, an error is raised.
