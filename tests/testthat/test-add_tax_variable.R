df <- expand.grid(
  bruto = seq(from = -50000, to = 200000, length.out = 26),
  year = c(2018, 2019, 2020),
  starter = c(TRUE, FALSE),
  oudedag = c(TRUE, FALSE),
  ondernemer = c(TRUE, FALSE)
)

test_that("starters aftrek", {
  df1 <- df %>%
    add_tax_variable("starters_aftrek", "starter")

  starters <- df1 %>%
    dplyr::filter(starter)

  expect_true(all(starters$starters_aftrek == 2123L))

  non_starters <- df1 %>%
    dplyr::filter(!starter)

  expect_true(all(non_starters$starters_aftrek == 0L))
})

test_that("oudedag_reserve", {
  df1 <- df %>%
    add_tax_variable("oudedag_reserve", "oudedag")

  oudedags <- df1 %>%
    dplyr::filter(oudedag) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      max = max(oudedag_reserve),
      min = min(oudedag_reserve)
    )

  expect_true(all(oudedags$min == 0L))
  expect_true(oudedags[oudedags$year == 2018, ]$max == 8775L)
  expect_true(oudedags[oudedags$year == 2019, ]$max == 8999L)
  expect_true(oudedags[oudedags$year == 2020, ]$max == 9218L)

  non_oudedags <- df1 %>%
    dplyr::filter(!oudedag)

  expect_true(all(non_oudedags$oudedag_reserve == 0L))
})

test_that("zelf aftrek", {
  df1 <- df %>%
    add_tax_variable("zelf_aftrek", "ondernemer")

  ondernemer_df <- df1 %>%
    dplyr::filter(ondernemer) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(max = max(zelf_aftrek))

  expect_true(ondernemer_df[ondernemer_df$year == 2018, ]$max == 7280L)
  expect_true(ondernemer_df[ondernemer_df$year == 2019, ]$max == 7280L)
  expect_true(ondernemer_df[ondernemer_df$year == 2020, ]$max == 7030L)

  non_ondernemer <- df1 %>%
    dplyr::filter(!ondernemer)

  expect_false(all(non_ondernemer$zelf_aftrek == 0L))
})
