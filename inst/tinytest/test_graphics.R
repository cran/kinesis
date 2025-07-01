Sys.setenv(LANGUAGE = "en") # Force locale

library("shiny")
using("tinysnapshot")

par_cex <- kinesis:::select_cex("x")
expect_snapshot_print(as.character(par_cex), label = "select_cex")

par_pch <- kinesis:::select_pch("x")
expect_snapshot_print(as.character(par_pch), label = "select_pch")

par_lty <- kinesis:::select_lty("x")
expect_snapshot_print(as.character(par_lty), label = "select_lty")

par_col <- kinesis:::select_color("x", label = "Color scheme")
expect_snapshot_print(as.character(par_col), label = "select_col")
