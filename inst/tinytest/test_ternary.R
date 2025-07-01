Sys.setenv(LANGUAGE = "en") # Force locale
library("shiny")

bronze <- read.csv("bronze.csv")
parts <- c("Cu", "Sn", "Pb", "Zn", "Au", "Ag", "As", "Sb")

x <- reactiveVal(bronze)

testServer(kinesis:::ternary_server, args = list(x = x), {
  expect_equal(data_raw(), bronze)
  expect_equal(quanti(), c("reference", "chronology", parts))
  expect_equal(quali(), c("group", "dynasty"))
  session$flushReact()

  session$setInputs("axis1-selected" = "Cu")
  session$setInputs("axis2-selected" = "Sn")
  session$setInputs("axis3-selected" = "Pb")
  expect_equal(data_tern(), bronze[, c("Cu", "Sn", "Pb")])

  session$setInputs("extra_quanti-selected" = "Cu")
  expect_equal(extra_quanti(), "Cu")

  session$setInputs("extra_quali-selected" = "dynasty")
  expect_equal(extra_quali(), "dynasty")
})

if (at_home()) {
  using("tinysnapshot")
  source("helpers.R")

  testServer(kinesis:::ternary_server, args = list(x = x), {
    expect_equal(data_raw(), bronze)
    expect_equal(quanti(), c("reference", "chronology", parts))
    expect_equal(quali(), c("group", "dynasty"))
    session$flushReact()

    session$setInputs("axis1-selected" = "Cu")
    session$setInputs("axis2-selected" = "Sn")
    session$setInputs("axis3-selected" = "Pb")
    session$setInputs(points = TRUE,
                      tile = "", bin = 12,
                      wrap = "", level = 0.95)

    session$setInputs("extra_quanti-selected" = "")
    session$setInputs("extra_quali-selected" = "")

    plot_ternary_default <- plot_ternary()
    expect_snapshot_plot(plot_ternary_default, "plot_ternary_default")

    session$setInputs("extra_quali-selected" = "dynasty")
    session$setInputs("par-col_quali" = "bright")

    plot_ternary_quali <- plot_ternary()
    expect_snapshot_plot(plot_ternary_quali, "plot_ternary_quali")
  })
}
