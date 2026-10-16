test_that("next_week works correctly", {
  nw1 <- next_week("2026-10-14", n = 1)
  expect_s3_class(nw1, "tpr")
  expect_equal(nw1$start, as.Date("2026-10-19"))
  expect_equal(nw1$end, as.Date("2026-10-25"))
  expect_equal(nw1$length, 7)
  
  nw2 <- next_week("2026-10-14", n = 2)
  expect_equal(nw2$start, as.Date("2026-10-26"))
  expect_equal(nw2$end, as.Date("2026-11-01"))
  
  # Sunday start
  nw_sun <- next_week("2026-10-14", n = 1, week_start = 7)
  expect_equal(nw_sun$start, as.Date("2026-10-18"))
  expect_equal(nw_sun$end, as.Date("2026-10-24"))
})

test_that("next_month works correctly across months and years", {
  nm1 <- next_month("2026-10-14", n = 1)
  expect_s3_class(nm1, "tpr")
  expect_equal(nm1$start, as.Date("2026-11-01"))
  expect_equal(nm1$end, as.Date("2026-11-30"))
  expect_equal(nm1$length, 30)
  
  # Year transition
  nm_year_trans <- next_month("2026-12-10", n = 1)
  expect_equal(nm_year_trans$start, as.Date("2027-01-01"))
  expect_equal(nm_year_trans$end, as.Date("2027-01-31"))
})

test_that("next_quarter works correctly", {
  # 2026-10-14 is in Q4 2026
  nq1 <- next_quarter("2026-10-14", n = 1)
  expect_s3_class(nq1, "tpr")
  expect_equal(nq1$start, as.Date("2027-01-01"))
  expect_equal(nq1$end, as.Date("2027-03-31"))
  expect_equal(nq1$length, 90)
})

test_that("next_year works correctly", {
  ny1 <- next_year("2026-10-14", n = 1)
  expect_s3_class(ny1, "tpr")
  expect_equal(ny1$start, as.Date("2027-01-01"))
  expect_equal(ny1$end, as.Date("2027-12-31"))
  expect_equal(ny1$length, 365)
})

test_that("next_n_days works correctly", {
  nnd_excl <- next_n_days("2026-10-14", n = 4, include_current = FALSE)
  expect_equal(nnd_excl$start, as.Date("2026-10-15"))
  expect_equal(nnd_excl$end, as.Date("2026-10-18"))
  expect_equal(nnd_excl$length, 4)
  
  nnd_incl <- next_n_days("2026-10-14", n = 4, include_current = TRUE)
  expect_equal(nnd_incl$start, as.Date("2026-10-14"))
  expect_equal(nnd_incl$end, as.Date("2026-10-18"))
  expect_equal(nnd_incl$length, 5)
})

test_that("next_n_weeks works correctly", {
  # 2026-10-14 is Wednesday
  nnw_excl <- next_n_weeks("2026-10-14", n = 2, include_current = FALSE)
  expect_equal(nnw_excl$start, as.Date("2026-10-19"))
  expect_equal(nnw_excl$end, as.Date("2026-11-01"))
  expect_equal(nnw_excl$length, 14)
  
  nnw_incl <- next_n_weeks("2026-10-14", n = 2, include_current = TRUE)
  expect_equal(nnw_incl$start, as.Date("2026-10-12"))
  expect_equal(nnw_incl$end, as.Date("2026-11-01"))
  expect_equal(nnw_incl$length, 21)
})

test_that("next_n_months works correctly", {
  nnm_excl <- next_n_months("2026-10-14", n = 2, include_current = FALSE)
  expect_equal(nnm_excl$start, as.Date("2026-11-01"))
  expect_equal(nnm_excl$end, as.Date("2026-12-31"))
  
  nnm_incl <- next_n_months("2026-10-14", n = 2, include_current = TRUE)
  expect_equal(nnm_incl$start, as.Date("2026-10-01"))
  expect_equal(nnm_incl$end, as.Date("2026-12-31"))
})

test_that("next_n_quarters works correctly", {
  nnq_excl <- next_n_quarters("2026-10-14", n = 2, include_current = FALSE)
  expect_equal(nnq_excl$start, as.Date("2027-01-01"))
  expect_equal(nnq_excl$end, as.Date("2027-06-30"))
  
  nnq_incl <- next_n_quarters("2026-10-14", n = 2, include_current = TRUE)
  expect_equal(nnq_incl$start, as.Date("2026-10-01"))
  expect_equal(nnq_incl$end, as.Date("2027-06-30"))
})

test_that("next_n_years works correctly", {
  nny_excl <- next_n_years("2026-10-14", n = 2, include_current = FALSE)
  expect_equal(nny_excl$start, as.Date("2027-01-01"))
  expect_equal(nny_excl$end, as.Date("2028-12-31"))
  expect_equal(nny_excl$length, 365 + 366) # 2028 is leap year
  
  nny_incl <- next_n_years("2026-10-14", n = 2, include_current = TRUE)
  expect_equal(nny_incl$start, as.Date("2026-01-01"))
  expect_equal(nny_incl$end, as.Date("2028-12-31"))
})

test_that("part argument works for next_* and next_n_* functions", {
  expect_equal(next_week("2026-10-14", part = "start"), as.Date("2026-10-19"))
  expect_equal(next_month("2026-10-14", part = "end"), as.Date("2026-11-30"))
  expect_equal(next_n_days("2026-10-14", n = 3, part = "length"), 3)
})
