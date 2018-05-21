context("Test reconfiguration of ALA4R to work with other server installations")

test_that("occurrences works on a different server", {
    skip_on_cran()
    ## save config
    sv_caching <- ala_config()$caching
    sv_serverconfig <- getOption("ALA4R_server_config")

    ## reconfigure
    ala_config(caching="off")
    server_config <- sv_serverconfig
    server_config$biocache_version <- "1.8.2"
    server_config$base_url_biocache <- "https://records-ws.nbnatlas.org/" ## or "http://datos.gbif.es/biocache-service/"
    ## previously used http://recherche-ws.gbif.fr/ but this was down in May 2018, changed to NBN
    options(ALA4R_server_config=server_config)
    ## we make really really sure that the settings get put back to ALA ones here
    ## otherwise the remainder of the test suite tends to fail
    dotest <- function() {
        ## make sure that when we exit we reset the settings
        on.exit({
            ala_config(caching=sv_caching)
            options(ALA4R_server_config=sv_serverconfig)
        })
        try({
            ## previous search was for Apus apus, which has a lot of occurrences and was slow
            ## now searching Onygena apus, which should only have one occurrence on NBN, so hopefully quicker
            occx <- occurrences(taxon="Onygena apus", download_reason_id=10)
            expect_gt(nrow(occx$data), 0)
        }, silent=TRUE)
        ala_config(caching=sv_caching)
        options(ALA4R_server_config=sv_serverconfig)
    }
    dotest()
    ## reset configuration back to how it was
    ala_config(caching=sv_caching)
    options(ALA4R_server_config=sv_serverconfig)
})
