test_that("previous_week works correctly", {
  pw1 <- previous_week("2026-10-14", n = 1)
  expect_s3_class(pw1, "tpr")
  expect_equal(pw1$start, as.Date("2026-10-05"))
  expect_equal(pw1$end, as.Date("2026-10-11"))
  expect_equal(pw1$length, 7)
  
  pw2 <- previous_week("2026-10-14", n = 2)
  expect_equal(pw2$start, as.Date("2026-09-28"))
  expect_equal(pw2$end, as.Date("2026-10-04"))
  
  # Sunday start
  pw_sun <- previous_week("2026-10-14", n = 1, week_start = 7)
  expect_equal(pw_sun$start, as.Date("2026-10-04"))
  expect_equal(pw_sun$end, as.Date("2026-10-10"))
})

test_that("previous_month works correctly across months and year transitions", {
  pm1 <- previous_month("2026-10-14", n = 1)
  expect_s3_class(pm1, "tpr")
  expect_equal(pm1$start, as.Date("2026-09-01"))
  expect_equal(pm1$end, as.Date("2026-09-30"))
  expect_equal(pm1$length, 30)
  
  # Year transition (January to December of previous year)
  pm_year_trans <- previous_month("2026-01-15", n = 1)
  expect_equal(pm_year_trans$start, as.Date("2025-12-01"))
  expect_equal(pm_year_trans$end, as.Date("2025-12-31"))
  
  # Multiple months back
  pm3 <- previous_month("2026-10-14", n = 3)
  expect_equal(pm3$start, as.Date("2026-07-01"))
  expect_equal(pm3$end, as.Date("2026-07-31"))
})

test_that("previous_quarter works correctly across quarters and year transitions", {
  pq1 <- previous_quarter("2026-10-14", n = 1)
  expect_s3_class(pq1, "tpr")
  expect_equal(pq1$start, as.Date("2026-07-01"))
  expect_equal(pq1$end, as.Date("2026-09-30"))
  
  # Q1 of 2026 to Q4 of 2025
  pq_year_trans <- previous_quarter("2026-02-15", n = 1)
  expect_equal(pq_year_trans$start, as.Date("2025-10-01"))
  expect_equal(pq_year_trans$end, as.Date("2025-12-31"))
})

test_that("previous_year works correctly", {
  py1 <- previous_year("2026-05-10", n = 1)
  expect_s3_class(py1, "tpr")
  expect_equal(py1$start, as.Date("2025-01-01"))
  expect_equal(py1$end, as.Date("2025-12-31"))
  expect_equal(py1$length, 365)
  
  py2 <- previous_year("2026-05-10", n = 2)
  expect_equal(py2$start, as.Date("2024-01-01"))
  expect_equal(py2$end, as.Date("2024-12-31"))
  expect_equal(py2$length, 366) # 2024 is a leap year
})

test_that("part argument works for previous_* functions", {
  expect_equal(previous_week("2026-10-14", part = "start"), as.Date("2026-10-05"))
  expect_equal(previous_month("2026-10-14", part = "end"), as.Date("2026-09-30"))
  expect_equal(previous_quarter("2026-10-14", part = "length"), 92)
  expect_equal(previous_year("2026-10-14", part = "start"), as.Date("2025-01-01"))
})
