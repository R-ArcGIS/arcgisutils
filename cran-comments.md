## R CMD check results

0 errors | 0 warnings | 1 note

* Cannot replicate check note for src/vendor/chrono/CITATION, _however_ I have added an step in Makevars to remove the vendor directorys
* The install size is < 2mb however the vendored dependencies are ~18mb making the source larger than the installation size.
* This is a new release.
