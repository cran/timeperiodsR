test_that("last_n_days works correctly with and without include_current", {
  lnd_excl <- last_n_days("2026-10-14", n = 5, include_current = FALSE)
  expect_s3_class(lnd_excl, "tpr")
  expect_equal(lnd_excl$start, as.Date("2026-10-09"))
  expect_equal(lnd_excl$end, as.Date("2026-10-13"))
  expect_equal(lnd_excl$length, 5)
  
  # When include_current = TRUE, period extends to include the current date
  lnd_incl <- last_n_days("2026-10-14", n = 5, include_current = TRUE)
  expect_equal(lnd_incl$start, as.Date("2026-10-09"))
  expect_equal(lnd_incl$end, as.Date("2026-10-14"))
  expect_equal(lnd_incl$length, 6)
})

test_that("last_n_weeks works correctly with and without include_current", {
  # 2026-10-14 is Wednesday
  # Previous completed week ended 2026-10-11
  lnw_excl <- last_n_weeks("2026-10-14", n = 2, include_current = FALSE)
  expect_equal(lnw_excl$start, as.Date("2026-09-28"))
  expect_equal(lnw_excl$end, as.Date("2026-10-11"))
  expect_equal(lnw_excl$length, 14)
  
  # Including current week: extends to end of current week (2026-10-18)
  lnw_incl <- last_n_weeks("2026-10-14", n = 2, include_current = TRUE)
  expect_equal(lnw_incl$start, as.Date("2026-09-28"))
  expect_equal(lnw_incl$end, as.Date("2026-10-18"))
  expect_equal(lnw_incl$length, 21)
})

test_that("last_n_months works correctly with and without include_current", {
  lnm_excl <- last_n_months("2026-10-14", n = 3, include_current = FALSE)
  expect_equal(lnm_excl$start, as.Date("2026-07-01"))
  expect_equal(lnm_excl$end, as.Date("2026-09-30"))
  
  lnm_incl <- last_n_months("2026-10-14", n = 3, include_current = TRUE)
  expect_equal(lnm_incl$start, as.Date("2026-07-01"))
  expect_equal(lnm_incl$end, as.Date("2026-10-31"))
})

test_that("last_n_quarters works correctly with and without include_current", {
  # Current is Q4 2026
  lnq_excl <- last_n_quarters("2026-10-14", n = 2, include_current = FALSE)
  expect_equal(lnq_excl$start, as.Date("2026-04-01"))
  expect_equal(lnq_excl$end, as.Date("2026-09-30"))
  
  lnq_incl <- last_n_quarters("2026-10-14", n = 2, include_current = TRUE)
  expect_equal(lnq_incl$start, as.Date("2026-04-01"))
  expect_equal(lnq_incl$end, as.Date("2026-12-30"))
})

test_that("last_n_years works correctly with and without include_current", {
  lny_excl <- last_n_years("2026-10-14", n = 2, include_current = FALSE)
  expect_equal(lny_excl$start, as.Date("2024-01-01"))
  expect_equal(lny_excl$end, as.Date("2025-12-31"))
  expect_equal(lny_excl$length, 366 + 365)
  
  lny_incl <- last_n_years("2026-10-14", n = 2, include_current = TRUE)
  expect_equal(lny_incl$start, as.Date("2024-01-01"))
  expect_equal(lny_incl$end, as.Date("2026-12-31"))
})

test_that("part argument works for last_n_* functions", {
  expect_equal(last_n_days("2026-10-14", n = 5, part = "length"), 5)
  expect_equal(last_n_weeks("2026-10-14", n = 2, part = "start"), as.Date("2026-09-28"))
  expect_equal(last_n_months("2026-10-14", n = 1, part = "end"), as.Date("2026-09-30"))
})
