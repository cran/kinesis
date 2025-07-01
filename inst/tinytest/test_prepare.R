Sys.setenv(LANGUAGE = "en") # Force locale
library("shiny")
using("tinysnapshot")

path <- system.file("tinytest", "fake.csv", package = "arkhe")
fake <- read.csv(path)
x <- reactiveVal(fake)

# remove_whitespace = FALSE
# remove_zero_row = FALSE
# remove_zero_column = FALSE
# remove_constant_column = FALSE
# all = FALSE
# zero_as_NA = FALSE
# remove = "none"

# Prepare ======================================================================
# TODO
# testServer(prepare_server, args = list(x = x, id = "foo"), {
#
# })

# Select =======================================================================
testServer(kinesis:::select_server, args = list(x = x), {
  session$setInputs("rownames-selected" = "", "colnames-selected" = colnames(fake))
  dataset <- session$getReturned()
  expect_equal(dataset(), fake)

  session$setInputs("colnames-selected" = c("doi", "color", "height"))
  session$elapse(2000)
  dataset <- session$getReturned()
  expect_equal(dataset(), fake[, c("doi", "color", "height")])
})

# Clean ========================================================================
testServer(kinesis:::clean_server, args = list(x = x), {
  dataset <- session$getReturned()
  expect_equal(dataset(), fake)

  session$setInputs(remove_zero_row = TRUE, all = FALSE)
  expect_equal(dataset(), arkhe::remove_zero(fake, margin = 1, all = FALSE))

  session$setInputs(remove_zero_row = FALSE)
  session$setInputs(remove_zero_column = TRUE, all = FALSE)
  expect_equal(dataset(), arkhe::remove_zero(fake, margin = 2, all = FALSE))
})

# Missing ======================================================================
testServer(kinesis:::missing_server, args = list(x = x), {
  dataset <- session$getReturned()
  expect_equal(dataset(), fake)

  session$setInputs(zero_as_NA = FALSE)
  session$setInputs(remove = "zero")
  expect_equal(dataset(), arkhe::replace_NA(fake, value = 0))

  session$setInputs(remove = "col")
  expect_equal(dataset(), arkhe::remove_NA(fake, margin = 2))

  session$setInputs(remove = "row")
  expect_equal(dataset(), arkhe::remove_NA(fake, margin = 1))

  session$setInputs(remove = "none")
})
