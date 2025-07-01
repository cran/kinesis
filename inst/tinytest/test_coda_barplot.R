Sys.setenv(LANGUAGE = "en") # Force locale
library("shiny")

bronze <- read.csv("bronze.csv")
parts <- c("Cu", "Sn", "Pb", "Zn", "Au", "Ag", "As", "Sb")
coda <- nexus::as_composition(bronze, parts = parts)
x <- reactiveVal(coda)

if (at_home()) {
  using("tinysnapshot")
  source("helpers.R")

  testServer(kinesis:::coda_barplot_server, args = list(x = x), {
    session$setInputs("select_major" = TRUE, "select_minor" = TRUE, "select_trace" = TRUE)
    expect_identical(data_bar(), coda)

    session$setInputs("par-col_quali" = "discreterainbow")
    barplot_coda_default <- plot_bar()
    expect_snapshot_plot(barplot_coda_default, "barplot_coda_default")
  })
}
