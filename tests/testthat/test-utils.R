test_that("log_now() makes a timestamp for logs", {
  expect_no_error({test_result <- log_now()})
  expect_identical(
    {gsub("\\.\\d+", "", gsub("\\d", "0", test_result))},
    "0000-00-00 00:00:00 |"
  )
})
