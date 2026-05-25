library(apa7)
library(testthat)

d <- data.frame(y = 1:3, x1 = 2:4, x2 = 3:5)

test_that("column_spanner_label renames selected columns with label prefix", {
  result <- column_spanner_label(d, "Group", c(x1, x2))
  expect_true("Group_x1" %in% colnames(result))
  expect_true("Group_x2" %in% colnames(result))
  expect_false("x1" %in% colnames(result))
  expect_false("x2" %in% colnames(result))
})

test_that("column_spanner_label preserves unselected columns", {
  result <- column_spanner_label(d, "Group", c(x1, x2))
  expect_true("y" %in% colnames(result))
  expect_equal(result$y, d$y)
})

test_that("column_spanner_label works with quoted column names", {
  result <- column_spanner_label(d, "Label", c("x1", "x2"))
  expect_true("Label_x1" %in% colnames(result))
  expect_true("Label_x2" %in% colnames(result))
})

test_that("column_spanner_label works with tidyselect helpers", {
  result <- column_spanner_label(d, "X", dplyr::starts_with("x"))
  expect_true("X_x1" %in% colnames(result))
  expect_true("X_x2" %in% colnames(result))
})

test_that("column_spanner_label returns data unchanged when no columns match", {
  result <- column_spanner_label(d, "Label", dplyr::starts_with("z"))
  expect_equal(colnames(result), colnames(d))
})

test_that("column_spanner_label preserves row count", {
  result <- column_spanner_label(d, "G", c(x1, x2))
  expect_equal(nrow(result), nrow(d))
})

test_that("column_spanner_label with relocate = FALSE does not reorder columns", {
  d2 <- data.frame(x2 = 1:3, y = 4:6, x1 = 7:9)
  result <- column_spanner_label(d2, "G", c(x2, y), relocate = FALSE)
  expect_equal(colnames(result)[1], "G_x2")
  expect_equal(colnames(result)[2], "G_y")
  expect_equal(colnames(result)[3], "x1")
})
