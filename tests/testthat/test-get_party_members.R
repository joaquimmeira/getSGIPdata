library(testthat)
library(tibble)

test_that("Function throws error for non-character input", {
  expect_error(get_party_members(12345), "The parameter 'id_orgao_partidario' must be a character vector.")
})

test_that("Function throws error for invalid party ID", {
  expect_error(get_party_members("99999"), "99999 isn't a valid id_orgao_partidario")
})

