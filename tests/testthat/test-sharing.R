test_that("arc_item() works", {
  skip_if_offline()

  arc_item("80eb92ffc89b4086abe8cedd58ab160c")
  test_cases <- c(
    webmap_app = "80eb92ffc89b4086abe8cedd58ab160c",
    storymap = "ad791fda858c46fdbe79636aa5f35dd8",
    instant_app = "f2dfd67d29ed4cabbb91e742e0297955",
    experience = "6e360741bfd84db79d5db774a1147815",
    webapp = "950b4eec577a4dc5b298a61adab41c06",
    notebook_item = "9a9fca3f09bb41dd856c9cd4239b8519",
    notebook = "9a9fca3f09bb41dd856c9cd4239b8519",
    webscene = "7b506043536246faa4194d4c3d4c921b",
    item_db = "84ba9c03786e462d960e3172bc1b2204",
    item_mapserver = "1d150c40d9f642cb8bd691017bf22cee",
    feature_collection = "24aa36ce1d7747c2b5a6aa57711d03fb"
  )

  expected_types <- c(
    "Web Mapping Application",
    "StoryMap",
    "Web Mapping Application",
    "Web Experience",
    "Web Mapping Application",
    "Notebook",
    "Notebook",
    "Web Scene",
    "Dashboard",
    "Map Service",
    "Feature Collection"
  )

  for (i in seq_along(test_cases)) {
    expect_equal(
      arc_item(test_cases[i])[["type"]],
      expected_types[i]
    )
  }
})
