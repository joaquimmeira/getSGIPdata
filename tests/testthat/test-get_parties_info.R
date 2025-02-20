library(testthat)
library(dplyr)


test_that("Invalid date format throws an error", {
  expect_error(get_parties_info(ivigencia = "2022-01-01"),
               "Invalid date format: 2022-01-01")
})

test_that("Date before 01/01/1900 throws an error", {
  expect_error(get_parties_info(ivigencia = "31/12/1899"),
               "You insert a date before 01/01/1900")
})

test_that("Future end date throws an error", {
  future_date <- format(Sys.Date() + 1, "%d/%m/%Y")
  expect_error(get_parties_info(fvigencia = future_date),
               "You insert a date for fim_vigencia that is after the today")
})

test_that("Start date after end date throws an error", {
  expect_error(get_parties_info(ivigencia = "01/01/2022", fvigencia = "01/01/2020"),
               "You insert a date for 'inicio_vigencia' that is before 'fim_vigencia'")
})


test_that("get_parties_info handles invalid input types", {
  expect_error(get_parties_info(123), "States must be a character vector")
  expect_error(get_parties_info(TRUE), "States must be a character vector")
  expect_error(get_parties_info(list("SP")), "States must be a character vector")
})

test_that("get_parties_info handles invalid state abbreviations", {
  expect_error(get_parties_info("XX"), "Invalid state")
  expect_error(get_parties_info(c("SP", "XX")), "Invalid state")
})

test_that("get_parties_info returns a tibble with correct columns", {
  result <- get_parties_info("SP")

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("id_orgao_partidario", "sigla", "numero", "municipio",
                    "uf", "tipo_orgao", "situacao_vigencia", "abrangencia", "sgUeSede") %in% colnames(result)))
})

test_that("get_parties_info handles NULL input (all states)", {
  result <- get_parties_info()

  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)  # Ensure it returns some data
})

test_that("get_parties_info handles DF case correctly", {
  result <- get_parties_info("DF")

  expect_s3_class(result, "tbl_df")
  expect_true("DF" %in% result$uf)
})
