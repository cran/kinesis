Sys.setenv(LANGUAGE = "en") # Force locale
library("shiny")

# Import =======================================================================
path <- "fake.csv"
fake <- read.csv(path)

testServer(import_server, {
  session$setInputs(header = TRUE, sep = ",",
                    dec = ".", quote = "\"'", rownames = FALSE,
                    na.strings = "NA", skip = 0, comment = "#")

  session$setInputs(file = list(datapath = "path.txt"), go = 1)
  expect_null(values())

  session$setInputs(file = list(datapath = path), go = NULL)
  expect_null(values())

  session$setInputs(file = list(datapath = path), go = 1)
  expect_equal(values(), fake)

  dataset <- session$getReturned()
  expect_equal(dim(dataset()), c(50L, 7L))

  session$setInputs("rownames-names" = "id")
  dataset <- session$getReturned()
  expect_equal(dim(dataset()), c(50L, 6L))
})

testServer(kinesis:::import_server, args = list(demo = "iris"), {
  session$setInputs("demo" = 1)
  expect_equal(dim(values()), c(150L, 5L))
})
