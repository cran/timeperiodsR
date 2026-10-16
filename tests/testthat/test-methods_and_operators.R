test_that("S3 extractor methods work on tpr objects", {
  p <- custom_period("2026-10-01", "2026-10-10")
  
  expect_equal(start(p), as.Date("2026-10-01"))
  expect_equal(end(p), as.Date("2026-10-10"))
  expect_equal(length(p), 10)
  expect_equal(seq(p), seq.Date(as.Date("2026-10-01"), as.Date("2026-10-10"), by = "day"))
  expect_equal(seq(p, what = "workdays"), p$workdays)
  expect_equal(seq(p, what = "weekends"), p$weekends)
  
  expect_equal(workdays(p), p$workdays)
  expect_equal(weekends(p), p$weekends)
  expect_equal(workdays_length(p), 7)
  expect_equal(weekends_length(p), 3)
  expect_equal(first_workday(p), as.Date("2026-10-01"))
  expect_equal(last_workday(p), as.Date("2026-10-09"))
  expect_equal(first_weekend(p), as.Date("2026-10-03"))
  expect_equal(last_weekend(p), as.Date("2026-10-10"))
})

test_that("S3 default methods work on Date vectors", {
  d <- seq.Date(as.Date("2026-10-01"), as.Date("2026-10-10"), by = "day")
  
  expect_equal(workdays(d), d[!format(d, "%w") %in% c("0", "6")])
  expect_equal(weekends(d), d[format(d, "%w") %in% c("0", "6")])
  expect_equal(first_workday(d), as.Date("2026-10-01"))
  expect_equal(last_workday(d), as.Date("2026-10-09"))
  expect_equal(first_weekend(d), as.Date("2026-10-03"))
  expect_equal(last_weekend(d), as.Date("2026-10-10"))
  expect_equal(workdays_length(d), 7)
  expect_equal(weekends_length(d), 3)
})

test_that("print.tpr outputs expected text", {
  p_single <- custom_period("2026-10-01", "2026-10-01")
  expect_output(print(p_single), "Time period: 2026-10-01")
  
  p_range <- custom_period("2026-10-01", "2026-10-10")
  expect_output(print(p_range), "Time period: 2026-10-01 .* - 2026-10-10")
})

test_that("subassignment operators [<- and [[<- modify tpr object correctly", {
  p <- custom_period("2026-10-05", "2026-10-15")
  
  # Change start via [
  p["start"] <- as.Date("2026-10-01")
  expect_equal(start(p), as.Date("2026-10-01"))
  expect_equal(length(p), 15)
  
  # Change end via [
  p["end"] <- as.Date("2026-10-20")
  expect_equal(end(p), as.Date("2026-10-20"))
  expect_equal(length(p), 20)
  
  # Change start via [[
  p[["start"]] <- as.Date("2026-10-03")
  expect_equal(start(p), as.Date("2026-10-03"))
  
  # Change end via [[
  p[["end"]] <- as.Date("2026-10-18")
  expect_equal(end(p), as.Date("2026-10-18"))
  
  # Error when start > end
  expect_error(p["start"] <- as.Date("2026-10-25"), "The beginning of a period cannot be later than its end")
  expect_error(p["end"] <- as.Date("2026-10-01"), "The beginning of a period cannot be later than its end")
  expect_error(p[["start"]] <- as.Date("2026-10-25"), "The beginning of a period cannot be later than its end")
  expect_error(p[["end"]] <- as.Date("2026-10-01"), "The beginning of a period cannot be later than its end")
})

test_that("filtering operators work correctly", {
  p1 <- custom_period("2026-10-01", "2026-10-10")
  p2 <- custom_period("2026-10-06", "2026-10-15")
  
  # %.in%
  expect_true(as.Date("2026-10-05") %.in% p1)
  expect_false(as.Date("2026-10-15") %.in% p1)
  expect_equal(sum(p1 %.in% p2), 5) # 2026-10-06 to 2026-10-10
  
  # %left_in%: dates in p1 that are in p2
  common_dates <- seq.Date(as.Date("2026-10-06"), as.Date("2026-10-10"), by = "day")
  expect_equal(p1 %left_in% p2, common_dates)
  
  # %left_out%: dates in p1 that are NOT in p2
  p1_only <- seq.Date(as.Date("2026-10-01"), as.Date("2026-10-05"), by = "day")
  expect_equal(p1 %left_out% p2, p1_only)
  
  # %right_in%: dates in p2 that are in p1
  expect_equal(p1 %right_in% p2, common_dates)
  
  # %right_out%: dates in p2 that are NOT in p1
  p2_only <- seq.Date(as.Date("2026-10-11"), as.Date("2026-10-15"), by = "day")
  expect_equal(p1 %right_out% p2, p2_only)
})
