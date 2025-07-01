Sys.setenv(LANGUAGE = "en") # Force locale

library("shiny")
using("tinysnapshot")

plot_out <- kinesis:::output_plot("x", title = "Un titre", note = "Une note")
expect_snapshot_print(as.character(plot_out), label = "output_plot")
