test_that("mean_m works", {

  x <- c(10, 13, 8, 12, 6, 15, 9, 11, 10)
  date <- seq.Date(as.Date("2023-01-01"), by = "day", length.out = 9)
  df <- data.frame(date, x)

  expect_equal(ncol(mean_m(df, x = "x")), 4)
  expect_equal(nrow(mean_m(df, x = "x")), 9)

  expect_error(mean_m(df, x = "y"))

  })
