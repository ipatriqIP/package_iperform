test_that("wtd works", {

  x <- c(10, 13, 8, 12, 6, 15, 9, 11, 10)
  date <- c("2023-08-01", "2023-08-02", "2023-08-03", "2023-08-04", "2023-08-05", "2023-08-06", "2023-08-07", "2023-08-08", "2023-08-09")
  df <- data.frame(date, x)

  expect_identical(wtd(df, "2023-08-02", x = "x"), 23)
  expect_identical(wtd(df, "2023-08-09", w = 0, x = "x"), 45)

  expect_error(wtd(df, date = "2023-08-05", x = "x", unite = c(3, 5)))

  })
