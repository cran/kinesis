Sys.setenv(LANGUAGE = "en") # Force locale

library("shiny")

testServer(kinesis:::get_calendar, {
  session$setInputs("calendar" = "CE")

  expect_equal(cal(), aion::CE())
})
