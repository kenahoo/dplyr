context("tbl")

test_that("tbl_nongroup_vars() excludes group variables", {
  cube <- group_by(nasa, month)
  expect_identical(tbl_nongroup_vars(cube), setdiff(tbl_vars(cube), "month"))

  gdf <- group_by(mtcars, cyl)
  expect_identical(tbl_nongroup_vars(gdf), setdiff(tbl_vars(gdf), "cyl"))
})

test_that("tbl_vars() records groups", {
  gdf <- group_by(mtcars, cyl, am)
  expect_is(tbl_vars(gdf), "dplyr_sel_vars")
  expect_true(is_sel_vars(tbl_vars(gdf)))
  expect_identical(tbl_vars(gdf) %@% groups, c("cyl", "am"))
})

test_that("all.equal() works properly on tibbles", {
  tibble <- dplyr::tibble

  data <- tibble(a = c(0.287577520124614, 0.788305135443807),
                 b = c(0.677570635452867, 0.572633401956409))

  result <- tibble(frac = data$a / data$b)
  expected <- tibble(frac = c(0.424424414337859, 1.37663142378798))

  expect_true(all.equal(result, expected))
  expect_equal(result, expected)
})
