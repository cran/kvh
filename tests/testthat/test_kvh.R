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
