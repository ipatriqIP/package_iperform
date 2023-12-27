test_that("taux works", {

  x <- c(10, 15, 8, 12, 6, 18, 22, 21, 28, 23, 22, 26, 19, 7)
  date <- seq.Date(as.Date("2023-03-01"), by = "day", len = 14)
  df <- data.frame(date, x)

  expect_identical(taux_v(df, date = "2023-03-02", x = "x", p = -1), 50.00)
  expect_identical(taux_v(df, date = "2023-03-09", x = "x"), 86.67)

  })


