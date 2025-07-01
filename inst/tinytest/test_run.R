Sys.setenv(LANGUAGE = "en") # Force locale

## Configuration file ==========================================================
expect_error(get_config("pca", file = NA, active = NA),
             "Could not find the configuration file")
expect_error(get_config("pca", file = "X", active = NA),
             "Could not find the configuration file")

conf_default <- get_config("pca", file = NULL, active = NA)
expect_true(conf_default$production)
expect_false(conf_default$verbose)

conf_test <- get_config("pca", file = NULL, active = "test")
expect_false(conf_test$production)
expect_true(conf_test$verbose)
