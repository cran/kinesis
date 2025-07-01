Sys.setenv(LANGUAGE = "en") # Force locale

library("shiny")

expect_identical(
  kinesis:::make_file_name("my-file", ext = "zip", project = NULL, timestamp = "250601_120000"),
  "my-file_250601_120000.zip"
)
expect_identical(
  kinesis:::make_file_name("my-file", ext = "zip", project = "my-project", timestamp = "250601_120000"),
  "my-project_my-file_250601_120000.zip"
)

expect_identical(kinesis:::url_tesselle(), "https://www.tesselle.org/")
expect_identical(kinesis:::url_tesselle("tabula"), "https://packages.tesselle.org/tabula/")

citation_packages <- kinesis:::cite_package()
expect_length(citation_packages, 1)

citation_packages <- kinesis:::cite_package(c("tabula", "kairos"))
expect_length(citation_packages, 3)

cite_article_year <- kinesis:::cite_article(
  author = "Filzmoser et al.",
  year = "2009",
  text = TRUE,
  html = FALSE
)
expect_identical(
  cite_article_year,
  "<span>\n  \n  Filzmoser et al.\n  (<span>2009</span>)\n</span>"
)

cite_article_doi <- kinesis:::cite_article(
  author = "Filzmoser et al.",
  year = "2009",
  doi = "10.1016/j.scitotenv.2009.08.008",
  text = TRUE,
  html = FALSE
)
expect_identical(
  cite_article_doi,
  "<span>\n  \n  Filzmoser et al.\n  (<a href=\"https://doi.org/10.1016/j.scitotenv.2009.08.008\" target=\"_blank\" role=\"doc-biblioref\">2009</a>)\n</span>"
)

cite_article_parenthesis <- kinesis:::cite_article(
  author = "Filzmoser et al.",
  year = "2009",
  text = FALSE,
  html = FALSE
)
expect_identical(
  cite_article_parenthesis,
  "<span>(Filzmoser et al., <span>2009</span>)</span>"
)

info_article_year <- kinesis:::info_article(
  author = "Filzmoser et al.",
  year = "2009",
  html = FALSE
)
expect_identical(
  info_article_year,
  "<span>\n  <i class=\"fas fa-circle-info\" role=\"presentation\" aria-label=\"circle-info icon\"></i>\n  Filzmoser et al.\n  (<span>2009</span>).\n</span>"
)

info_article_doi <- kinesis:::info_article(
  author = "Filzmoser et al.",
  year = "2009",
  doi = "10.1016/j.scitotenv.2009.08.008",
  html = FALSE
)
expect_identical(
  info_article_doi,
  "<span>\n  <i class=\"fas fa-circle-info\" role=\"presentation\" aria-label=\"circle-info icon\"></i>\n  Filzmoser et al.\n  (<a href=\"https://doi.org/10.1016/j.scitotenv.2009.08.008\" target=\"_blank\" role=\"doc-biblioref\">2009</a>).\n</span>"
)
