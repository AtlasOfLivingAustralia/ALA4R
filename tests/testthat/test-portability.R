context("Test reconfiguration of ALA4R to work with other server installations")

test_that("occurrences works on a different server", {
    skip_on_cran()
    ## save config
    sv_caching <- ala_config()$caching
    sv_serverconfig <- getOption("ALA4R_server_config")

    ## reconfigure
    ala_config(caching="off")
    server_config <- sv_serverconfig
    server_config$biocache_version <- "1.8.1"
    server_config$base_url_biocache <- "http://datos.gbif.es/biocache-service/" ##or "http://recherche-ws.gbif.fr/"
    options(ALA4R_server_config = server_config)
    
    occx <- occurrences(taxon="Apus apus",download_reason_id=10)
    expect_gt(nrow(occx$data),0)

    ## reset configuration back to how it was
    ala_config(caching=sv_caching)
    options(ALA4R_server_config = sv_serverconfig)    
})
