# ALA4R 1.9.1
* `ALA4R` is deprecated and will be removed from CRAN at the end of 2021.
The package to replace `ALA4R`, `galah`, is now [available on CRAN](https://CRAN.R-project.org/package=galah).
 For an introduction to `galah`, visit the
[GitHub page](https://github.com/AtlasOfLivingAustralia/galah).
* Minor fixes to functions in response to ALA core infrastructure upgrade

# ALA4R 1.9.0
* Add ability to generate a DOI for data downloads in `occurrences()` with `generate_doi` parameter
* Add `email_notify` parameter to `occurrences()`, set to `FALSE` by default
* Set default download reason as "scientific research"

# ALA4R 1.8.0
* remove `method` parameter for `occurrences()` and only use `offline` method to make download more stable
* rename `image_info()` to `images()` and add option to download images
* `images()` makes use of new ALA images web service
* added `occurrence_images()` function to retrieve image information and download images using occurrence ids

# ALA4R 1.7.0
* remove dependency on RCurl 

# ALA4R 1.6.3
* changes to API urls (http to https)
* handle new sites_by_species web service

# ALA4R 1.6.2
* updates to handle ALA API field changes
* update vignette to be more robust to server outage

# ALA4R 1.6.0
* added workaround for `occurrences(..., method="offline")` to cope with server-side bug (see https://github.com/AtlasOfLivingAustralia/biocache-service/issues/221)
* package documentation via `pkgdown` (https://atlasoflivingaustralia.github.io/ALA4R/)
* refreshed package vignette
* change of package maintainer

# ALA4R 1.5.10
* bugfixes

# ALA4R 1.5.6
* added `biocache_version` parameter to server configuration, to support use of ALA4R with other server instances (e.g. other national installations using the ALA server infrastructure) 
* minor bugfixes

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
