# ALA4R 1.20

* `as_is` behaviour in `ala_fields()`.

* usage of stringr functions now compatible with stringr v1.0.

* tests added to tests/testthat.

# ALA4R 1.191

* improved parsing of field names in internal function `check_fq()` (https://github.com/AtlasOfLivingAustralia/ALA4R/issues/13).

# ALA4R 1.19

* internal function `check_fq()` throws a warning not an error if it thinks there is a problem with the `fq` string (https://github.com/AtlasOfLivingAustralia/ALA4R/issues/13).

# ALA4R 1.18

* for improperly classified taxa, the `result$classification` object returned by `species_info()` is now a data.frame (https://github.com/AtlasOfLivingAustralia/ALA4R/issues/11).

# ALA4R 1.18

* bugfix in `species_info()` (https://github.com/AtlasOfLivingAustralia/ALA4R/issues/11).
