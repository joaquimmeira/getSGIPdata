library(testthat)
library(tibble)

test_that("Function throws error for non-character input", {
  expect_error(get_party_members(12345), "The parameter 'id_orgao_partidario' must be a character vector.")
})

test_that("Function throws error for invalid party ID", {
  expect_error(get_party_members("99999"), "99999 isn't a valid id_orgao_partidario")
})

test_that("Function handles empty input gracefully", {
  result <- get_party_members(character(0))
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})
