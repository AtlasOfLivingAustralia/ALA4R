.onLoad <- function(libname, pkgname) {
    if (pkgname == "ALA4R") {
        ## populate the options slot
        ala_config() ## will set to default values if not already set

        ## also populate the server configuration info
        server_config <- list(
            max_occurrence_records = 500000,
            server_max_url_length = 8150, ## bytes, for Apache with default
            ## LimitRequestLine value of 8190, allowing 40 bytes wiggle room.
            ## Users will be warned of possible problems when URL exceeds this
            brand = "ALA4R", ## the package name that is shown to users in
            ## messages and warnings
            notify = "If this problem persists please notify the ALA4R
            maintainers by lodging an issue at
            https://github.com/AtlasOfLivingAustralia/ALA4R/issues/
            or emailing support@ala.org.au",
            support_email = "support@ala.org.au", ## contact email
            reasons_function = "ala_reasons", ## ala_reasons or equivalent func
            fields_function = "ala_fields", ## ala_fields or equivalent func
            ## the occurrences or equivalent function name
            occurrences_function = "occurrences",
            config_function = "ala_config", ##  ala_config or equivalent func
            base_url_spatial = "https://spatial.ala.org.au/ws/",
            base_url_bie = "https://bie.ala.org.au/ws/",
            base_url_biocache = "https://biocache-ws.ala.org.au/ws/",
            biocache_version = "2.1.16",
            base_url_alaspatial = "https://spatial.ala.org.au/alaspatial/ws/",
            base_url_images = "https://images.ala.org.au/",
            base_url_logger = "https://logger.ala.org.au/service/logger/",
            base_url_fieldguide = "https://fieldguide.ala.org.au/",
            base_url_lists = "https://lists.ala.org.au/ws/",
            base_url_collectory = "https://collections.ala.org.au/ws/"
        )
        if (!"ALA4R_server_config" %in% names(options())) {
            options(ALA4R_server_config = server_config)
        }
    }
}
