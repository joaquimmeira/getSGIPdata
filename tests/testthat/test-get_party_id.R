library(testthat)
library(dplyr)

test_that("get_party_id handles invalid input types", {
  expect_error(get_party_id(123), "States must be a character vector")
  expect_error(get_party_id(TRUE), "States must be a character vector")
  expect_error(get_party_id(list("SP")), "States must be a character vector")
})

test_that("get_party_id handles invalid state abbreviations", {
  expect_error(get_party_id("XX"), "Invalid state")
  expect_error(get_party_id(c("SP", "XX")), "Invalid state")
})

test_that("get_party_id returns a tibble with correct columns", {
  result <- get_party_id("SP")

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("id_orgao_partidario", "sigla", "numero", "municipio",
                    "uf", "tipo_orgao", "situacao_vigencia", "abrangencia", "sgUeSede") %in% colnames(result)))
})

test_that("get_party_id handles NULL input (all states)", {
  result <- get_party_id()

  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)  # Ensure it returns some data
})

test_that("get_party_id handles DF case correctly", {
  result <- get_party_id("DF")

  expect_s3_class(result, "tbl_df")
  expect_true("DF" %in% result$uf)
})
