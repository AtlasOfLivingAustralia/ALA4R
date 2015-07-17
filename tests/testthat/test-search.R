context("Test searching functions")

test_that("search_fulltext generally functions as expected", {
    expect_that(search_fulltext("red kangaroo"),has_names(c("meta","data")))
    expect_that(nrow(search_fulltext("bilbobaggins")$data),equals(0)) ## query that should not match anything
    expect_that(nrow(search_fulltext("red",page_size=20)$data),equals(20))
    expect_that(search_fulltext("red kangaroo",output_format="complete"),has_names(c("meta","data")))
    expect_that(search_fulltext("kingdom:Fungi",output_format="complete"),has_names(c("meta","data")))
})

test_that("search_fulltext start parm works as expected", {
    x1=search_fulltext("red",page_size=10)
    x2=search_fulltext("red",page_size=10,start=2)
    ## so row 1 of x2$data should equal row 2 of x1$data ... but columns may actually be different!
    temp=intersect(names(x1$data),names(x2$data))
    x1=x1$data[2,c(temp)]
    x2=x2$data[1,c(temp)]
    rownames(x1)=""
    rownames(x2)=""
    expect_equal(x1,x2)
})

#test_that("search_fulltext sort_by parm works as expected", {
#    expect_error(search_fulltext("red",page_size=10,sort_by="blurg"))
#    ## hmmm, other sort_by's don't work: sort_by="name" or "family" works, but not "scientificName" or others (500 error)
#    ## and in fact sort_by "name" doesn't seem to give strict reverse ordering when order="desc","asc" used
#    ## not quite sure what is going on here - seems to be issues at server end, not R end
#})

## not tested yet: S3method(print,search_fulltext)

test_that("search_layers generally works as expected", {
    expect_that(search_layers(type="all"),is_a('data.frame'))
    expect_that(search_layers(type="all",output_format="complete"),is_a('data.frame'))
    expect_that(nrow(search_layers(type="all")),is_more_than(400))
    expect_that(nrow(search_layers(type="all",query="bilbobaggins")),equals(0))
    expect_error(search_layers(type="bilbobaggins"))
})
## not tested yet: S3method(print,search_layers)

test_that("search_names can cope with factor inputs", {
    expect_equal(search_names(factor("Grevillea humilis")),search_names("Grevillea humilis"))
})

test_that("search_names can cope with all-unrecogized names", {
    expect_equal(nrow(search_names("fljkhdlsi")),1)
    expect_equal(nrow(search_names(c("fljkhdlsi","sdkhfowbiu"))),2)
    expect_true(all(is.na(search_names(c("fljkhdlsi","sdkhfowbiu"))$guid)))
})

## not tested yet: S3method(print,search_names)
          

## not tested yet: export(search_partial_name)
## not tested yet: S3method(print,search_partial_name)
