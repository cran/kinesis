Sys.setenv(LANGUAGE = "en") # Force locale
library("shiny")

parts <- c("Cu", "Sn", "Pb", "Zn", "Au", "Ag", "As", "Sb")

testServer(kinesis:::coda_server, args = list(demo = "bronze"), {
  session$flushReact()
  session$setInputs("import-demo" = 1)
  expect_equal(dim(data_raw()), c(369L, 12L))

  session$setInputs("parts-names" = parts, "group-names" = NULL, "condense-names" = NULL)
  session$elapse(2000)
  expect_equal(var_parts(), parts)
  expect_equal(dim(data_coda()), c(369L, 8L))
  expect_null(var_group())
  expect_equal(dim(data_group()), c(369L, 8L))
  expect_null(var_condense())
  expect_equal(dim(data_condense()), c(369L, 8L))
  dataset <- session$getReturned()
  expect_false(nexus::is_grouped(data_valid()))

  session$setInputs("group-names" = "dynasty")
  expect_equal(var_group(), "dynasty")
  dataset <- session$getReturned()
  expect_equal(dim(dataset()), c(369L, 8L))
  expect_true(nexus::is_grouped(dataset()))

  session$setInputs("condense-names" = c("dynasty", "reference"))
  expect_equal(var_condense(), c("dynasty", "reference"))
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
