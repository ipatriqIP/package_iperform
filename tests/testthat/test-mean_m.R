test_that("mean_m works", {

  x <- c(10, 13, 8, 12, 6, 15, 9, 11, 10)
  date <- seq.Date(as.Date("2023-01-01"), by = "day", length.out = 9)
  df <- data.frame(date, x)

  #expect_error(mean_m(data = x, x = "x"))
  expect_error(mean_m(df, x = "y"))

  #expect_equal(nrow(overview(df, x = "x")), 9)
  #expect_equal(ncol(overview(df, x = "x")), 4)

  })
