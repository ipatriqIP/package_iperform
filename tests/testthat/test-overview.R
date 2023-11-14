test_that("overview works", {

  x <- rnorm(600, mean = 50, sd = 6.3)
  date <- seq.Date(as.Date("2022-01-01"), by = "day", length.out = 600)
  df <- data.frame(date, x)

  expect_equal(ncol(overview(df, date = "2023-04-25", x = "x")), 16)
  expect_equal(nrow(overview(df, date = "2023-04-25", x = "x")), 1)

  })