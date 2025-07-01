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

testServer(kinesis:::update_selectize_variables, args = list(x = x), {
  val <- session$getReturned()
  session$elapse(2000)
  expect_equal(val(), NULL)

  session$setInputs("selected" = "species")
  val <- session$getReturned()
  session$elapse(2000)
  expect_equal(val(), "species")
})

x <- reactiveVal(colnames(iris))

testServer(kinesis:::update_selectize_values, args = list(x = x), {
  val <- session$getReturned()
  expect_equal(val(), NULL)

  session$setInputs("selected" = "species")
  val <- session$getReturned()
  expect_equal(val(), "species")
})

# TODO
# y <- reactiveVal("species")

# testServer(kinesis:::update_selectize_values, args = list(x = x, exclude = y), {
# })
