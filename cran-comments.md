## R CMD check results

0 errors | 0 warnings | 0 notes

Suggests or Enhances not in mainstream repositories:
  arcgisbinding

- `{arcgisbinding}` is a Suggested package which is not available from a standard CRAN
repository but installation instructions are referenced in the DESCRIPTION's Description.

### Use of \dontrun

This package interacts with API endpoint which require authentication or may be destructive. 
For this reason, functions that require credentials or perform destructive actions have examples gated by \dontrun.

## Test Environments 

GitHub Actions: 

- {os: macos-latest,   r: 'release'}
- {os: windows-latest, r: 'release'}
- {os: ubuntu-latest,   r: 'devel', http-user-agent: 'release'}
- {os: ubuntu-latest,   r: 'release'}
- {os: ubuntu-latest,   r: 'oldrel-1'}
- {os: ubuntu-latest,   r: 'oldrel-2'}

