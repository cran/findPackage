test_that("errors", {
  testthat::expect_error(
    findPackage(),
    "The topicString parameter is mandatory"
  )

  testthat::expect_error(
    findPackage("Analytic Hierarchy Process", fromDate="2022-15-01"),
    "From date is not correctly specified"
  )

  testthat::expect_error(
    findPackage("Analytic Hierarchy Process", toDate="2023-15-0A"),
    "To date is not correctly specified"
  )

  testthat::expect_error(
    findPackage("Factor Analysis", fromDate="2022-01-01", toDate="2020-12-31"),
    "From date is later than To date"
  )

  testthat::expect_error(
    findPackage("MCDA", sortOrder="X"),
    "Sort order can be 'A' or 'D' only"
  )

  testthat::expect_equal(
    nrow(findPackage("MCDA", sortOrder="D"))>0,
    TRUE
  )

  testthat::expect_equal(
    nrow(findPackage("VWXYZ")),
    0
  )
  
  testthat::expect_equal(
    nrow(findPackage("SEM"))>0,
    TRUE
  )
  
  testthat::expect_equal(
    nrow(findPackage("(SEM)|(Structural Equation Model)"))
    >  nrow(findPackage("SEM")),
    TRUE
  )
  
})
