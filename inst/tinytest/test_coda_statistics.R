Sys.setenv(LANGUAGE = "en") # Force locale
library("shiny")

bronze <- read.csv("bronze.csv")
parts <- c("Cu", "Sn", "Pb", "Zn", "Au", "Ag", "As", "Sb")
coda <- nexus::as_composition(bronze, parts = parts)
x <- reactiveVal(coda)

testServer(kinesis:::coda_summary_server, args = list(x = x), {
  expect_identical(dim(data_loc()), c(1L, 8L))
  expect_identical(dim(data_quant()), c(5L, 8L))
  expect_identical(dim(data_cov()), c(8L, 8L))
  expect_identical(dim(data_pip()), c(8L, 8L))
  expect_identical(dim(data_var()), c(8L, 8L))
})

coda <- nexus::as_composition(bronze, parts = parts, group = "dynasty")
x <- reactiveVal(coda)

testServer(kinesis:::coda_summary_server, args = list(x = x), {
  expect_identical(dim(data_loc()), c(3L, 8L))
  expect_identical(dim(data_quant()), c(5L, 8L))
  expect_identical(dim(data_cov()), c(8L, 8L))
  expect_identical(dim(data_pip()), c(8L, 8L))
  expect_identical(dim(data_var()), c(8L, 8L))
})

if (at_home()) {
  using("tinysnapshot")
  source("helpers.R")

  testServer(kinesis:::coda_summary_server, args = list(x = x), {
    session$setInputs("hist_select-selected" = "Cu")
    plot_coda_hist <- plot_hist()
    expect_snapshot_plot(plot_coda_hist, "plot_coda_hist")
  })
}
