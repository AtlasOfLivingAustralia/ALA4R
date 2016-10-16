context("Test list-related functions")

thischeck=function() {
    test_that("ala_lists does stuff", {
        all_lists <- ala_lists()
        expect_is(all_lists,"data.frame")
        expect_true(all(c("dataResourceUid","listName","listType") %in% names(all_lists)))
        ## should be a few Field Guide lists
        expect_gt(nrow(all_lists[grep("Field Guide",all_lists$listName),]),1)
        
        l <- ala_lists(search_guids("Achatina fulica")$guid)
        ## these names are different to when no guid is supplied
        expect_equal(names(l),c("dataResourceUid","guid","list","kvpValues"))        
    })
}
check_caching(thischeck)


thischeck=function() {
    test_that("ala_list does stuff", {
        ## download the vertebrates field guide
        l <- ala_list(druid="dr1146")
        expect_is(l,"data.frame")
        expect_named(l,c("id","name","commonName","scientificName","lsid","kvpValues"))
        expect_is(l$kvpValues,"list")
        expect_named(l$kvpValues[[1]],c("key","value"))
    })
}
##check_caching(thischeck)
## skip this one temporarily: something wrong in list content preventing parsing
