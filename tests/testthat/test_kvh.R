context("examples")
dexample=system.file("examples", package="kvh")
fexample=system(sprintf("ls -1 %s/*.kvh", dexample), intern=TRUE)
saved=readRDS("res.RData")
res=list()
test_that("example files",
    for (f in fexample) {
        bf=basename(f)
        res[[bf]]=kvh_read(f)
        expect_equal(res[[bf]], saved[[bf]], info=f)
    }
)
test_that("obj_by_keys",
    expect_equal(obj_by_keys(res[["hello_salut.kvh"]], c("salutation", "en")), "Hello, world!"),
    expect_equal(obj_by_keys(res[["hello_salut.kvh"]], c("salutation", "zz")), NULL)
)
