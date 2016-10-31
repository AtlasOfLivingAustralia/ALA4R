.onLoad <- function(libname,pkgname) {
    if (pkgname=="ALA4R") {
        ## populate the options slot
        ala_config() ## will set to default values if not already set

        ## also populate the server configuration info
        server_config <- list(max_occurrence_records=500000,
                              server_max_url_length=8150, ## bytes, for Apache with default LimitRequestLine value of 8190, allowing 40 bytes wiggle room. Users will be warned of possible problems when URL exceeds this length
                              brand="ALA4R", ## the package name that is shown to users in messages and warnings
                              notify="If this problem persists please notify the ALA4R maintainers by lodging an issue at https://github.com/AtlasOfLivingAustralia/ALA4R/issues/ or emailing support@ala.org.au", ## the string that will be displayed to users to notify the package maintainers
                              support_email="support@ala.org.au", ## contact email
                              reasons_function="ala_reasons", ## the ala_reasons or equivalent function name
                              fields_function="ala_fields", ## the ala_fields or equivalent function name
                              occurrences_function="occurrences", ## the occurrences or equivalent function name
                              config_function="ala_config", ## the ala_config or equivalent function name
                              base_url_spatial="http://spatial.ala.org.au/ws/", ## the base url for spatial web services
                              base_url_bie="http://bie.ala.org.au/ws/", ## the base url for BIE web services
                              base_url_biocache="http://biocache.ala.org.au/ws/", ## the base url for biocache web services
                              base_url_alaspatial="http://spatial.ala.org.au/alaspatial/ws/", ## the base url for older ALA spatial services
                              base_url_images="http://images.ala.org.au/", ## the base url for the images database. Set to NULL or empty string if not available
                              base_url_logger="http://logger.ala.org.au/service/logger/", ## the base url for usage logging webservices
                              base_url_fieldguide="http://fieldguide.ala.org.au/",
                              base_url_lists="http://lists.ala.org.au/ws/"
                              )
        if (!"ALA4R_server_config" %in% names(options())) options(ALA4R_server_config=server_config)
        
    }
}
