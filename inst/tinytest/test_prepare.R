Sys.setenv(LANGUAGE = "en") # Force locale

library("shiny")

path <- "fake.csv"
fake <- read.csv(path)
x <- reactiveVal(fake)

# Clean ========================================================================
testServer(kinesis:::clean_server, args = list(x = x), {
  dataset <- session$getReturned()
  expect_equal(dataset(), x())

  session$setInputs(remove_zero_row = TRUE, all = FALSE)
  expect_equal(no_zero_row(), arkhe::remove_zero(fake, margin = 1, all = FALSE))

  session$setInputs(remove_zero_row = FALSE)
  session$setInputs(remove_zero_column = TRUE, all = FALSE)
  expect_equal(no_zero_col(), arkhe::remove_zero(fake, margin = 2, all = FALSE))
})

# Missing ======================================================================
testServer(kinesis:::missing_server, args = list(x = x), {
  dataset <- session$getReturned()
  expect_equal(dataset(), x())

  session$setInputs(zero_as_NA = FALSE)
  session$setInputs(remove = "zero")
  expect_equal(no_missing(), arkhe::replace_NA(fake, value = 0))

  session$setInputs(remove = "col")
  expect_equal(no_missing(), arkhe::remove_NA(fake, margin = 2))

  session$setInputs(remove = "row")
  expect_equal(no_missing(), arkhe::remove_NA(fake, margin = 1))

  session$setInputs(remove = "none")
  expect_equal(no_missing(), fake)
})
