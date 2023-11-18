test_that("qtd works", {

  x <- rnorm(600, mean = 50, sd = 6.3)
  date <- seq.Date(as.Date("2022-01-01"), by = "day", length.out = 600)
  df <- data.frame(date, x)

  expect_identical(qtd(df, "2023-01-06", x = "x"), mtd(df, "2023-01-06", x = "x"))

  expect_error(qtd(df, date = "2022-08-05", x = "x", decimal = c(1, 0)))

  })
