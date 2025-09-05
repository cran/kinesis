Sys.setenv(LANGUAGE = "en") # Force locale
library("shiny")

bronze <- read.csv("bronze.csv")
parts <- c("Cu", "Sn", "Pb", "Zn", "Au", "Ag", "As", "Sb")
x <- reactiveVal(bronze)

testServer(kinesis:::ternary_server, args = list(x = x), {
  expect_equal(data_raw(), bronze)
  expect_equal(quanti(), bronze[, c("reference", "chronology", parts)])
  expect_equal(quali(), bronze[c("group", "dynasty")])

  session$flushReact()
  session$setInputs("axis1-names" = "Cu")
  session$setInputs("axis2-names" = "Sn")
  session$setInputs("axis3-names" = "Pb")
  expect_equal(data_tern(), bronze[, c("Cu", "Sn", "Pb")])

  session$setInputs("extra_quanti-names" = "Cu")
  expect_equal(extra_quanti(), bronze[["Cu"]])

  session$setInputs("extra_quali-names" = "dynasty")
  expect_equal(extra_quali(), bronze[["dynasty"]])
})
