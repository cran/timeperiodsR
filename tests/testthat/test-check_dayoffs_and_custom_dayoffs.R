test_that("custom_day_offs options and marks work without internet", {
  old_cdo <- getOption("timeperiodsR.custom_day_offs")
  on.exit(options("timeperiodsR.custom_day_offs" = old_cdo), add = TRUE)
  
  options("timeperiodsR.custom_day_offs" = c("2026-10-02", "2026-10-05"))
  
  # check_dayoffs assigns "3" to custom day offs
  marks <- check_dayoffs(date = c("2026-10-01", "2026-10-02", "2026-10-05"), include_custom_day_offs = TRUE)
  expect_equal(unname(marks["2026-10-02"]), "3")
  expect_equal(unname(marks["2026-10-05"]), "3")
  
  # custom_day_offs method on Date vector
  dates <- as.Date(c("2026-10-01", "2026-10-02", "2026-10-05"))
  cdo <- custom_day_offs(dates)
  expect_equal(cdo, as.Date(c("2026-10-02", "2026-10-05")))
})

test_that("custom_day_offs integrates into tpr object when option is set", {
  old_cdo <- getOption("timeperiodsR.custom_day_offs")
  on.exit(options("timeperiodsR.custom_day_offs" = old_cdo), add = TRUE)
  
  options("timeperiodsR.custom_day_offs" = c("2026-10-02", "2026-10-05"))
  p <- custom_period("2026-10-01", "2026-10-10")
  
  expect_s3_class(p, "tpr")
  expect_true(!is.null(p$custom_day_offs))
  expect_equal(p$custom_day_offs, as.Date(c("2026-10-02", "2026-10-05")))
  expect_equal(custom_day_offs(p), as.Date(c("2026-10-02", "2026-10-05")))
})

test_that("check_dayoffs fails gracefully on network errors without warning or error", {
  # Simulate network failure via tracing httr::GET
  trace(httr::GET, tracer = quote(stop("Simulated connection timeout")), print = FALSE)
  on.exit(untrace(httr::GET), add = TRUE)
  
  # Expect message, NO warning, NO error
  expect_no_warning({
    expect_message(
      res <- check_dayoffs(date = c("2026-10-14", "2026-10-15")),
      "Failed to connect to isDayOff API"
    )
  })
  
  # Must return "9" for all dates
  expect_equal(unname(res), c("9", "9"))
  expect_equal(names(res), c("2026-10-14", "2026-10-15"))
})

test_that("check_dayoffs fails gracefully on HTTP error status without warning", {
  skip_on_cran()
  
  # Invalid date triggers HTTP 400 Bad Request on isdayoff.ru
  expect_no_warning({
    expect_message(
      res <- check_dayoffs(year = 99999, month = 99, day = 99),
      "isDayOff API returned HTTP error"
    )
  })
  
  expect_equal(res, "9")
})

test_that("check_dayoffs works with live API when online", {
  skip_on_cran()
  
  # Check if online and service reachable
  test_reach <- tryCatch(
    httr::GET("https://isdayoff.ru/api/getdata", query = list(year = 2026, month = 10, day = 14), httr::timeout(3)),
    error = function(e) NULL
  )
  if (is.null(test_reach) || httr::http_error(test_reach)) {
    skip("isdayoff.ru API is not reachable")
  }
  
  # Wednesday 2026-10-14 is a workday in Russia
  res_day <- check_dayoffs(year = 2026, month = 10, day = 14, cc = "ru")
  expect_equal(res_day, "0")
  
  # Saturday 2026-10-17 is a weekend
  res_weekend <- check_dayoffs(year = 2026, month = 10, day = 17, cc = "ru")
  expect_equal(res_weekend, "1")
  
  # Test with tpr object
  tw <- this_week("2026-10-14")
  res_tw <- check_dayoffs(date = tw, cc = "ru")
  expect_equal(length(res_tw), 7)
  expect_equal(unname(res_tw[1:5]), rep("0", 5)) # Mon-Fri
  expect_equal(unname(res_tw[6:7]), rep("1", 2)) # Sat-Sun
})
