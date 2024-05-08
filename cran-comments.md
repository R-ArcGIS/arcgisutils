## R CMD check results

0 errors | 0 warnings | 1 note

* C_clean and clean were not being called for Debian build resulting in NOTE for src/vendor/chrono/CITATION
    - This should be removed now via `rm -Rf $(VENDOR_DIR)` in the Makevars 
* The install size is < 2mb however the vendored dependencies are ~18mb making the source larger than the installation size.
* This is a new release.
