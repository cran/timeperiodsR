test_that("custom_period creates valid tpr object", {
  cp <- custom_period("2026-10-01", "2026-10-10")
  
  expect_s3_class(cp, "tpr")
  expect_equal(cp$start, as.Date("2026-10-01"))
  expect_equal(cp$end, as.Date("2026-10-10"))
  expect_equal(cp$length, 10)
  expect_equal(length(cp$sequence), 10)
  expect_equal(cp$length, cp$workdays_length + cp$weekends_length)
  expect_equal(cp$first_workday, as.Date("2026-10-01"))
  expect_equal(cp$last_workday, as.Date("2026-10-09"))
  expect_equal(cp$first_weekend, as.Date("2026-10-03"))
  expect_equal(cp$last_weekend, as.Date("2026-10-10"))
})

test_that("custom_period handles 1-day period", {
  # Friday 2026-10-02
  cp_single <- custom_period("2026-10-02", "2026-10-02")
  expect_equal(cp_single$length, 1)
  expect_equal(cp_single$workdays_length, 1)
  expect_equal(cp_single$weekends_length, 0)
  expect_equal(cp_single$first_workday, as.Date("2026-10-02"))
  expect_equal(cp_single$last_workday, as.Date("2026-10-02"))
})

test_that("custom_period respects part argument", {
  expect_equal(custom_period("2026-10-01", "2026-10-10", part = "start"), as.Date("2026-10-01"))
  expect_equal(custom_period("2026-10-01", "2026-10-10", part = "end"), as.Date("2026-10-10"))
  expect_equal(custom_period("2026-10-01", "2026-10-10", part = "length"), 10)
})

test_that("as_timeperiod converts character and Date vectors to tpr", {
  dates_char <- c("2026-10-05", "2026-10-01", "2026-10-10", "2026-10-03")
  res_char <- as_timeperiod(dates_char)
  
  expect_s3_class(res_char, "tpr")
  expect_equal(res_char$start, as.Date("2026-10-01"))
  expect_equal(res_char$end, as.Date("2026-10-10"))
  expect_equal(res_char$length, 10)
  
  dates_date <- as.Date(dates_char)
  res_date <- as_timeperiod(dates_date)
  expect_s3_class(res_date, "tpr")
  expect_equal(res_date$start, as.Date("2026-10-01"))
  expect_equal(res_date$end, as.Date("2026-10-10"))
})
