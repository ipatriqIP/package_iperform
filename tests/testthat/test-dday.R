test_that("dday works", {

  x <- c(10, 13, 8, 12, 6)
  date <- c("2023-08-01", "2023-08-02", "2023-08-03", "2023-08-04", "2023-08-05")
  df <- data.frame(date, x)

  expect_identical(dday(df, "2023-08-03", x = "x"), 8)
  expect_identical(dday(df, "2023-08-05", d = -1, x = "x"), 12)
  expect_identical(dday(df, "2023-08-05", d = 0, x = "x"), 6)
  expect_identical(dday(df, "2023-08-01", d = 1, x = "x"), 13)

  })

test_that("date est de type date", {

  x <- c(10, 13, 8, 12, 6)
  date <- c("2023-08-01", "2023-08-02", "2023-08-03", "2023-08-04", "2023-08-05")
  df <- data.frame(date, x)

  expect_error(ddays(df, date = "2023-08", x = "x"))
  expect_error(ddays(df, date = "20230803", x = "x"))

  })
