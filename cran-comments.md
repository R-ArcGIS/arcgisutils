## R CMD check results

This is a maintenance update to publish small bug fixes. 

0 errors | 0 warnings | 2 notes

Suggests or Enhances not in mainstream repositories:
  arcgisbinding

Size of tarball: 6021580 bytes

- The tarball is 6mb because **Rust dependencies** are vendored within `src/rust/vendor.tar.xz` which is 5.9mb. The final installation size on Mac OS is 1.3mb.
- `{arcgisbinding}` is a Suggested package which is not available from a standard CRAN repository but installation instructions are referenced in the DESCRIPTION's Description.


## Test Environments 

GitHub Actions: 

- {os: macos-latest,   r: 'release'}
- {os: windows-latest, r: 'release'}
- {os: ubuntu-latest,   r: 'devel', http-user-agent: 'release'}
- {os: ubuntu-latest,   r: 'release'}
- {os: ubuntu-latest,   r: 'oldrel-1'}
- {os: ubuntu-latest,   r: 'oldrel-2'}

