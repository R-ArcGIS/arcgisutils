## R CMD check results

This is a maintenance release of {arcgisutils} with minor bug fixes and one small feature.

0 errors | 0 warnings | 0 notes

Suggests or Enhances not in mainstream repositories:
  arcgisbinding

- `{arcgisbinding}` is a Suggested package which is not available from a standard CRAN repository but installation instructions are referenced in the DESCRIPTION's Description.

### Use of \dontrun

This package interacts with API endpoint which require authentication or may be destructive. 
For this reason, functions that require credentials or perform destructive actions have examples gated by \dontrun.
