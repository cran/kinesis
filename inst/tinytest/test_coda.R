Sys.setenv(LANGUAGE = "en") # Force locale
library("shiny")

bronze <- read.csv("bronze.csv")
path <- system.file("tinytest", "bronze.csv", package = "kinesis")
parts <- c("Cu", "Sn", "Pb", "Zn", "Au", "Ag", "As", "Sb")

testServer(kinesis:::coda_server, {
  session$setInputs("import-file" = list(datapath = path),
                    "import-header" = TRUE,
                    "import-sep" = ",", "import-dec" = ".",
                    "import-quote" = "\"'",
                    "import-na.strings" = "NA", "import-skip" = 0,
                    "import-comment" = "#", "import-go" = 1)

  expect_equal(data_raw(), bronze)
  session$setInputs("select-rownames-selected" = "", "select-colnames-selected" = parts, groups = "", condense = "")
  session$elapse(2000)
  expect_equal(dim(coda()), c(369L, 8L))
  expect_equal(dim(data_group()), c(369L, 8L))
  expect_equal(dim(data_condense()), c(369L, 8L))
  dataset <- session$getReturned()
  expect_false(nexus::is_grouped(dataset()))

  session$setInputs("group-selected" = "dynasty")
  session$elapse(2000)
  expect_equal(col_group(), c("dynasty"))
  dataset <- session$getReturned()
  expect_equal(dim(dataset()), c(369L, 8L))
  expect_true(nexus::is_grouped(dataset()))

  session$setInputs("condense-selected" = c("dynasty", "reference"))
  session$elapse(2000)
  expect_equal(col_condense(), c("dynasty", "reference"))
  dataset <- session$getReturned()
  expect_equal(dim(dataset()), c(300L, 8L))
  expect_true(nexus::is_grouped(dataset()))
})

# Zeros ========================================================================
fake <- data.frame(
  group = rep(c("A", "B", "C"), each = 3),
  Ca = c(7.72, 0, 3.11, 7.19, 7.41, 5, 0, 1, 4.51),
  Fe = c(6.12, 5.88, 5.12, 0, 6.02, 0, 0, 5.28, 5.72),
  Na = c(0.97, 1.59, 0, 0.86, 0.76, 0.51, 0.75, 0.52, 0.56)
)
coda <- nexus::as_composition(fake, parts = c(2, 3, 4), groups = 1)
imp <- nexus::replace_zero(coda, value = c(0.02, 0.1, 0.01) / 100, delta = 2/3)

x <- reactiveVal(coda)

testServer(kinesis:::coda_zero_server, args = list(x = x), {
  expect_equal(ids(), c("limit_Ca", "limit_Fe", "limit_Na"))
  dataset <- session$getReturned()
  expect_equal(dataset(), coda)

  session$setInputs(delta = 2/3, limit_Ca = 0.02,
                    limit_Fe = 0.1, limit_Na = 0.01, go = 1)
  dataset <- session$getReturned()
  expect_equal(dataset(), imp)
})
