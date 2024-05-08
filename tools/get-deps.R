# TODO none of this is required if it isn't on CRAN
# what environment variable can we check?

# this is where the vendored dependencies go as a zip file
out_path <- "src/rust/vendor.tar.xz"

# if the file doesn't exist, download it
if (!file.exists(out_path)) {
  # inform that the file is being downloaded
  message("vendored dependencies are being downloaded...")

  # download the file
  dl_res <- download.file(
    # TODO interpolate this path for rextendr
    "https://github.com/R-ArcGIS/arcgisutils/raw/main/src/rust/vendor.tar.xz",
    out_path
  )

  # error if the download failed
  if (dl_res != 0) {
    stop("Vendored dependencies failed to download. Check that you have an active internet connection.")
  } else {
    message("vendored dependencies downloaded successfully!")
  }
} else {
  message("vendor.tar.xz found")
}

# read in the known md5 checksum
known_md5 <- readLines("tools/vendor.md5")

# get the md5 for the existent vendor.tar.xz
local_md5 <- tools::md5sum(out_path)

# verify that the md5 are identical
is_known <- local_md5 == known_md5

# if the md5 dont match
if (!is_known) {
  # remove the downloaded file
  file.remove(out_path)

  # exit with informative error
  msg <- paste0(
    "Cannot verify the md5 of vendor.tar.xz\n",
    "Expected: ", known_md5,
    "\nFound:    ", local_md5
  )

  # throw error
  stop(msg)
} else {
  # inform that checksums matched
  message("vendor.tar.xz md5 matched the known checksum\ncontinuing with installation")
}
