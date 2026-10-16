test_that("this_week works correctly with default Monday start", {
  # 2026-10-14 is a Wednesday
  tw <- this_week("2026-10-14")
  
  expect_s3_class(tw, "tpr")
  expect_equal(tw$start, as.Date("2026-10-12")) # Monday
  expect_equal(tw$end, as.Date("2026-10-18"))   # Sunday
  expect_equal(tw$length, 7)
  expect_equal(tw$workdays_length, 5)
  expect_equal(tw$weekends_length, 2)
  expect_equal(tw$first_workday, as.Date("2026-10-12"))
  expect_equal(tw$last_workday, as.Date("2026-10-16"))
  expect_equal(tw$first_weekend, as.Date("2026-10-17"))
  expect_equal(tw$last_weekend, as.Date("2026-10-18"))
})

test_that("this_week works correctly with Sunday start", {
  tw_sun <- this_week("2026-10-14", week_start = 7)
  
  expect_equal(tw_sun$start, as.Date("2026-10-11")) # Sunday
  expect_equal(tw_sun$end, as.Date("2026-10-17"))   # Saturday
  expect_equal(tw_sun$length, 7)
})

test_that("this_month works correctly", {
  tm <- this_month("2026-10-14")
  
  expect_s3_class(tm, "tpr")
  expect_equal(tm$start, as.Date("2026-10-01"))
  expect_equal(tm$end, as.Date("2026-10-31"))
  expect_equal(tm$length, 31)
  
  # February in leap year 2024
  tm_leap <- this_month("2024-02-10")
  expect_equal(tm_leap$start, as.Date("2024-02-01"))
  expect_equal(tm_leap$end, as.Date("2024-02-29"))
  expect_equal(tm_leap$length, 29)
  
  # February in non-leap year 2023
  tm_non_leap <- this_month("2023-02-10")
  expect_equal(tm_non_leap$end, as.Date("2023-02-28"))
  expect_equal(tm_non_leap$length, 28)
})

test_that("this_quarter works correctly across quarters", {
  tq1 <- this_quarter("2026-02-15")
  expect_equal(tq1$start, as.Date("2026-01-01"))
  expect_equal(tq1$end, as.Date("2026-03-31"))
  
  tq2 <- this_quarter("2026-05-15")
  expect_equal(tq2$start, as.Date("2026-04-01"))
  expect_equal(tq2$end, as.Date("2026-06-30"))
  
  tq3 <- this_quarter("2026-08-15")
  expect_equal(tq3$start, as.Date("2026-07-01"))
  expect_equal(tq3$end, as.Date("2026-09-30"))
  
  tq4 <- this_quarter("2026-11-15")
  expect_equal(tq4$start, as.Date("2026-10-01"))
  expect_equal(tq4$end, as.Date("2026-12-31"))
})

test_that("this_year works correctly for leap and non-leap years", {
  ty <- this_year("2026-06-15")
  expect_equal(ty$start, as.Date("2026-01-01"))
  expect_equal(ty$end, as.Date("2026-12-31"))
  expect_equal(ty$length, 365)
  
  ty_leap <- this_year("2024-06-15")
  expect_equal(ty_leap$start, as.Date("2024-01-01"))
  expect_equal(ty_leap$end, as.Date("2024-12-31"))
  expect_equal(ty_leap$length, 366)
})

test_that("part argument works for this_* functions", {
  expect_equal(this_week("2026-10-14", part = "start"), as.Date("2026-10-12"))
  expect_equal(this_month("2026-10-14", part = "end"), as.Date("2026-10-31"))
  expect_equal(this_month("2026-10-14", part = "length"), 31)
  expect_equal(this_quarter("2026-10-14", part = "start"), as.Date("2026-10-01"))
  expect_equal(this_year("2026-10-14", part = "end"), as.Date("2026-12-31"))
})
