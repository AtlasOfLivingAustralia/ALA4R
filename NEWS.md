# ALA4R 1.5

* updated code,examples and vignette for new ALA taxonomic backbone
* "offline" method added to `occurrences()`, allowing download of larger datasets with more fields
* `fieldguide()` function added
* fields="all" option added to `taxinfo_download()`
* `ala_list()` and `ala_lists()` functions added

# ALA4R 1.24

* better handling of text encoding (https://github.com/AtlasOfLivingAustralia/ALA4R/issues/15)

# ALA4R 1.23

* added `image_number` parameter to `image_info()`

# ALA4R 1.21

* added `image_info()` and `occurrence_details()` functions

* added `extra="all"` and `qa="all"` options to `occurrences()`

# ALA4R 1.20

* fixed `as_is` behaviour in `ala_fields()`.

* usage of stringr functions now compatible with stringr v1.0.

* parameter `occurrence_count` added to `search_names()`.

* tests added to tests/testthat.

# ALA4R 1.191

* improved parsing of field names in internal function `check_fq()` (https://github.com/AtlasOfLivingAustralia/ALA4R/issues/13).

# ALA4R 1.19

* internal function `check_fq()` throws a warning not an error if it thinks there is a problem with the `fq` string (https://github.com/AtlasOfLivingAustralia/ALA4R/issues/13).

# ALA4R 1.18

* for improperly classified taxa, the `result$classification` object returned by `species_info()` is now a data.frame (https://github.com/AtlasOfLivingAustralia/ALA4R/issues/11).

# ALA4R 1.18

* bugfix in `species_info()` (https://github.com/AtlasOfLivingAustralia/ALA4R/issues/11).
