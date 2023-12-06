test_that("full_q works", {

  x <- c(10, 13, 8, 12, 6, 15, 9, 11, 10)
  date <- c("2023-06-28", "2023-06-29", "2023-06-30", "2023-07-01", "2023-07-02", "2023-07-03", "2023-07-04", "2023-07-05", "2023-07-06")
  df <- data.frame(date, x)

  expect_identical(full_q(df, date = "2023-06-30", x = "x"), 31)
  expect_identical(full_q(df, date = "2023-07-04", x = "x"), 63)

  expect_identical(full_q(df, date = "2023-07-04", x = "x"), mtd(df, date = "2023-07-06", x = "x"))

  })
