test_that("overview works", {

  x <- rnorm(600, mean = 50, sd = 6.3)
  date <- seq.Date(as.Date("2022-01-01"), by = "day", length.out = 600)
  df <- data.frame(date, x)

  expect_equal(ncol(overview(df, date = "2023-04-25", x = "x")), 13)
  expect_equal(nrow(overview(df, date = "2023-04-25", x = "x")), 1)

  expect_error(overview(df, date = "2023-05-05", x = "x", unite = c(2, 2)))

  })


