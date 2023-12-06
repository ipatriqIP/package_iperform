test_that("htd works", {

  x <- rnorm(600, mean = 50, sd = 6.3)
  date <- seq.Date(as.Date("2022-01-01"), by = "day", length.out = 600)
  df <- data.frame(date, x)

  expect_identical(htd(df, "2022-01-01", x = "x"), dday(df, "2022-01-01", x = "x"))
  expect_identical(htd(df, "2023-01-25", x = "x"), mtd(df, "2023-01-25", x = "x"))
  expect_identical(htd(df, "2023-03-04", x = "x"), qtd(df, "2023-03-04", x = "x"))

  })
