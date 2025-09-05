Sys.setenv(LANGUAGE = "en") # Force locale

library("shiny")

x <- reactiveVal(iris[, 1:4])

testServer(kinesis:::build_numeric_input, args = list(x = x), {
  session$setInputs("num_Sepal.Length" = 1,
                    "num_Sepal.Width" = 2,
                    "num_Petal.Length" = 3,
                    "num_Petal.Width" = 4)
  expect_equal(values(), c("num_Sepal.Length" = 1, "num_Sepal.Width" = 2,
                           "num_Petal.Length" = 3, "num_Petal.Width" = 4))
})

x <- reactiveVal(iris)

testServer(kinesis:::update_checkbox_colnames, args = list(x = x), {
  val <- session$getReturned()
  expect_equal(val(), NULL)

  session$setInputs(names = c("Sepal.Length", "Sepal.Width"))
  val <- session$getReturned()
  expect_equal(val(), c("Sepal.Length", "Sepal.Width"))
})

testServer(kinesis:::update_selectize_colnames, args = list(x = x), {
  val <- session$getReturned()
  expect_equal(val(), NULL)

  session$setInputs("names" = "species")
  val <- session$getReturned()
  expect_equal(val(), "species")
})

testServer(kinesis:::update_selectize_rownames, args = list(x = x), {
  val <- session$getReturned()
  expect_equal(val(), NULL)

  session$setInputs("names" = "species")
  val <- session$getReturned()
  expect_equal(val(), "species")
})
