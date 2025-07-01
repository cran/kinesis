Sys.setenv(LANGUAGE = "en") # Force locale

library("shiny")
using("tinysnapshot")

cal_out <- kinesis:::select_calendar("x", default = "BP")
expect_snapshot_print(as.character(cal_out), label = "select_calendar")

testServer(kinesis:::get_calendar, {
  session$setInputs("calendar" = "CE")

  expect_equal(cal(), aion::CE())
})
