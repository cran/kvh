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
test_that("obj_by_keys", {
  expect_equal(kvh::obj_by_keys(res[["hello_salut.kvh"]], c("salutation", "en")), "Hello, world!")
  expect_null(kvh::obj_by_keys(res[["hello_salut.kvh"]], c("salutation", "zz")))
}
)

# create test files
wd=tempdir(TRUE)
fn=tempfile(pattern = "file", tmpdir = wd, fileext = ".kvh")
fc=file(fn, "wb")
cat("a\tb \n", file=fc)
close(fc)
res=kvh::kvh_read(fn, strip_white=TRUE)
test_that("strip_white",
  expect_equal(res[["a"]], "b", info="strip space")
)

unlink(fn)
