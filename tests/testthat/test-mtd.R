test_that("mtd works", {

  x <- c(10, 13, 8, 12, 6, 15, 9, 11, 10)
  date <- c("2023-08-01", "2023-08-02", "2023-08-03", "2023-08-04", "2023-08-05", "2023-08-06", "2023-08-07", "2023-08-08", "2023-08-09")
  df <- data.frame(date, x)

  expect_identical(mtd(df, "2023-08-03", x = "x"), 31)
  expect_identical(mtd(df, "2023-08-03", m = 0, x = "x"), 31)
  expect_identical(mtd(df, "2023-08-09", m = 0, x = "x"), 94)

  expect_error(mtd(df, date = "2023-08-09", m = c(3, 5), x = "x"))
  expect_error(mtd(df, date = 2023-08-09, m = 1, x = "x"))

  })
