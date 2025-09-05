Sys.setenv(LANGUAGE = "en") # Force locale
library("shiny")

bronze <- read.csv("bronze.csv")
parts <- c("Cu", "Sn", "Pb", "Zn", "Au", "Ag", "As", "Sb")
coda <- nexus::as_composition(bronze, parts = parts)
x <- reactiveVal(coda)

testServer(kinesis:::logratio_server, args = list(x = x, method = "clr"), {
  expect_true(nexus::is_logratio(logratio()))
  expect_identical(dim(logratio()), c(369L, 8L))

  # title <- session$getOutput("title")
  # expect_identical(title, "Centered Log-Ratio")
})
testServer(kinesis:::logratio_server, args = list(x = x, method = "alr"), {
  expect_true(nexus::is_logratio(logratio()))
  expect_identical(dim(logratio()), c(369L, 7L))

  session$setInputs("pivot" = "Cu")
  expect_identical(
    labels(logratio()),
    c("Sn/Cu", "Pb/Cu", "Zn/Cu", "Au/Cu", "Ag/Cu", "As/Cu", "Sb/Cu")
  )

  # title <- session$getOutput("title")
  # expect_identical(title, "Additive Log-Ratio")
})
