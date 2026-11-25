library(apa7)
library(testthat)

# Padding spaces that align values
unpad <- function(x) gsub("[ -  ]", "", x)

desc_data <- function() {
  d <- mtcars
  d$am <- factor(d$am, labels = c("Automatic", "Manual"))
  d$cyl <- factor(d$cyl)
  d$hp[c(2, 5)] <- NA
  d$cyl[3] <- NA
  d
}

test_that("groups get columns with their sizes; continuous and categorical rows", {
  d <- desc_data()
  result <- apa_descriptives(d, variables = c("mpg", "hp", "cyl"), by = "am")
  expect_equal(
    colnames(result),
    c("Variable", "Automatic (*n* = 19)", "Manual (*n* = 13)", "Total (*N* = 32)")
  )
  em <- " "
  expect_equal(
    result$Variable,
    c("Mpg, *M* (*SD*)", "Hp, *M* (*SD*)", "Cyl, *n* (%)", paste0(em, c("4", "6", "8")))
  )
  auto <- d$mpg[d$am == "Automatic"]
  expect_equal(
    result$`Automatic (*n* = 19)`[1],
    sprintf("%.2f (%.2f)", mean(auto), sd(auto))
  )
  # Percentages of nonmissing responses
  cyl_auto <- d$cyl[d$am == "Automatic" & !is.na(d$cyl)]
  expect_equal(
    result$`Automatic (*n* = 19)`[4],
    sprintf("%d (%.1f)", sum(cyl_auto == "4"), 100 * mean(cyl_auto == "4"))
  )
  expect_equal(result$`Automatic (*n* = 19)`[3], "")

  note <- apa_note(result)
  expect_match(note, "Values are *M* (*SD*) for continuous variables and *n* (%) for categorical variables.", fixed = TRUE)
  expect_match(note, "Missing values: 2 for Hp and 1 for Cyl.", fixed = TRUE)
  expect_equal(apa_flextable(result)$body$dataset[[1]], result$Variable)
})

test_that("medians, labels, numeric categories, and tests of group differences", {
  d <- desc_data()
  result <- apa_descriptives(
    d,
    variables = c("mpg", "hp", "gear"),
    by = "am",
    continuous = c(hp = "median_iqr"),
    categorical = "gear",
    labels = c(mpg = "Miles per gallon"),
    test = TRUE,
    total = FALSE
  )
  expect_equal(colnames(result)[4], "*p*")
  expect_equal(result$Variable[1:3], c("Miles per gallon, *M* (*SD*)", "Hp, *Mdn* (*IQR*)", "Gear, *n* (%)"))
  hp <- d$hp[d$am == "Manual" & !is.na(d$hp)]
  expect_equal(result$`Manual (*n* = 13)`[2], sprintf("%.2f (%.2f)", median(hp), IQR(hp)))
  # Zero counts keep their decimals
  expect_true("0 (0.0)" %in% result$`Manual (*n* = 13)`)

  p_t <- t.test(mpg ~ am, data = d)$p.value
  expect_equal(unpad(result$`*p*`[1]), unpad(the$column_formats$p@formatter(p_t)))
  p_chi <- suppressWarnings(chisq.test(table(d$gear, d$am), correct = FALSE)$p.value)
  expect_equal(unpad(result$`*p*`[3]), unpad(the$column_formats$p@formatter(p_chi)))
  expect_equal(unpad(result$`*p*`[4]), "")

  note <- apa_note(result)
  expect_match(note, "*M* (*SD*) or *Mdn* (*IQR*) for continuous variables", fixed = TRUE)
  expect_match(note, "Welch's *t* tests", fixed = TRUE)
  expect_match(note, "Kruskal", fixed = TRUE)
  expect_match(note, "*Mdn* = median; *IQR* = interquartile range.", fixed = TRUE)
})

test_that("more than two groups use Welch's ANOVA; no groups give one column", {
  d <- desc_data()
  d$gear <- factor(d$gear)
  result <- apa_descriptives(d, variables = "mpg", by = "gear", test = TRUE)
  p_anova <- oneway.test(mpg ~ gear, data = d)$p.value
  expect_equal(unpad(result$`*p*`[1]), unpad(the$column_formats$p@formatter(p_anova)))
  expect_match(apa_note(result), "Welch's one-way ANOVAs", fixed = TRUE)

  one <- apa_descriptives(d[c("mpg", "wt")])
  expect_equal(colnames(one), c("Variable", "Total (*N* = 32)"))
  expect_no_match(apa_note(one), "categorical", fixed = TRUE)

  # Rows with a missing group are left out, and the note says so
  d$am[1:2] <- NA
  dropped <- apa_descriptives(d, variables = "mpg", by = "am")
  expect_equal(colnames(dropped)[4], "Total (*N* = 30)")
  expect_match(apa_note(dropped), "Respondents with a missing value of Am (*n* = 2) are not included.", fixed = TRUE)
})

test_that("apa_descriptives() checks its arguments", {
  d <- desc_data()
  expect_error(apa_descriptives(as.matrix(d)), "data frame")
  expect_error(apa_descriptives(d, by = "zzz"), "name of a column")
  expect_error(apa_descriptives(d, variables = "zzz"), "zzz")
  expect_error(apa_descriptives(d, variables = "mpg", test = TRUE), "needs groups")
  expect_error(apa_descriptives(d, variables = "mpg", continuous = "mode"), "mean_sd")
  expect_error(apa_descriptives(d, variables = "mpg", continuous = c(zz = "median_iqr")), "unknown")
  d$one <- "a"
  expect_error(apa_descriptives(d, variables = "mpg", by = "one"), "at least 2 groups")
  d$when <- Sys.Date() + 1:32
  expect_error(apa_descriptives(d, variables = "when"), "cannot be described")
})

test_that("Fisher's exact test is used when expected counts are small", {
  d <- desc_data()
  d$gear <- factor(d$gear)
  auto <- apa_descriptives(d, variables = c("cyl", "gear"), by = "am", test = TRUE)
  p_cyl <- stats::fisher.test(table(d$cyl, d$am))$p.value
  expect_equal(unpad(auto$`*p*`[1]), sub("^0", "", sprintf("%.2f", p_cyl)))
  note <- apa_note(auto)
  expect_match(note, "Fisher's exact test was used instead for Cyl and Gear, which had expected counts below 5.", fixed = TRUE)

  chisq <- apa_descriptives(d, variables = "cyl", by = "am", test = TRUE, categorical_test = "chisq")
  p_chisq <- suppressWarnings(stats::chisq.test(table(d$cyl, d$am), correct = FALSE)$p.value)
  expect_equal(unpad(chisq$`*p*`[1]), sub("^0", "", sprintf("%.2f", p_chisq)))
  expect_no_match(apa_note(chisq), "Fisher", fixed = TRUE)

  fisher <- apa_descriptives(d, variables = "cyl", by = "am", test = TRUE, categorical_test = "fisher")
  expect_match(apa_note(fisher), "Fisher's exact tests (categorical variables)", fixed = TRUE)

  # Large expected counts keep the chi-square test
  big <- data.frame(g = rep(c("a", "b"), each = 100), x = rep(c("u", "v"), 100))
  expect_no_match(apa_note(apa_descriptives(big, variables = "x", by = "g", test = TRUE)), "Fisher", fixed = TRUE)
  expect_error(apa_descriptives(d, variables = "cyl", by = "am", test = TRUE, categorical_test = "exact"), "categorical_test")
})

test_that("missing values can be shown in rows, and ranges added", {
  d <- desc_data()
  result <- apa_descriptives(d, variables = c("mpg", "hp", "cyl"), by = "am", missing = "row", range = TRUE)
  expect_equal(
    result$Variable,
    c(
      "Mpg, *M* (*SD*)", "\u2003Range",
      "Hp, *M* (*SD*)", "\u2003Range", "\u2003Missing",
      "Cyl, *n* (%)", "\u20034", "\u20036", "\u20038", "\u2003Missing"
    )
  )
  totals <- result[[4]]
  expect_equal(unpad(totals[2]), paste0(sprintf("%.2f", min(d$mpg)), "\u2013", sprintf("%.2f", max(d$mpg))))
  expect_equal(totals[5], "2")
  expect_equal(totals[10], "1")
  expect_equal(result[[2]][5], as.character(sum(is.na(d$hp[d$am == "Automatic"]))))
  note <- apa_note(result)
  expect_match(note, "Ranges are the minimum and maximum.", fixed = TRUE)
  expect_match(note, "Missing rows give the number of missing values.", fixed = TRUE)
  expect_no_match(note, "Missing values:", fixed = TRUE)

  # Variables without missing values get no missing row
  complete <- apa_descriptives(d, variables = "mpg", missing = "row")
  expect_equal(complete$Variable, "Mpg, *M* (*SD*)")
  expect_no_match(apa_note(complete), "Missing", fixed = TRUE)

  # Missing and range rows have no p-value
  tested <- apa_descriptives(d, variables = "hp", by = "am", test = TRUE, missing = "row", range = TRUE)
  expect_equal(tested$`*p*`[2:3], c("", ""))
})

test_that("variables, by, and categorical take unquoted names and tidyselect helpers", {
  d <- desc_data()
  quoted <- apa_descriptives(d, variables = c("mpg", "hp", "cyl"), by = "am")
  expect_identical(apa_descriptives(d, variables = c(mpg, hp, cyl), by = am), quoted)
  expect_identical(
    apa_descriptives(d, variables = c(mpg, hp, starts_with("cy")), by = am),
    quoted
  )
  # A character vector stored in a variable, without a deprecation message
  vars <- c("mpg", "hp", "cyl")
  expect_no_warning(expect_identical(apa_descriptives(d, variables = vars, by = am), quoted))
  expect_identical(apa_descriptives(d, variables = tidyselect::all_of(vars), by = "am"), quoted)
  # Passing a column through a wrapper function with {{ }}
  wrapper <- function(data, v, g) apa_descriptives(data, variables = {{ v }}, by = {{ g }})
  expect_identical(wrapper(d, c(mpg, hp, cyl), am), quoted)
  # The grouping variable is not described, even when selected
  expect_identical(apa_descriptives(d, variables = c(mpg, hp, cyl, am), by = am), quoted)
  all_but_by <- apa_descriptives(d[c("mpg", "cyl", "am")], by = am)
  expect_false(any(grepl("^Am", all_but_by$Variable)))
  # Numeric columns selected as categorical
  expect_identical(
    apa_descriptives(d, variables = c(mpg, gear), categorical = gear),
    apa_descriptives(d, variables = c("mpg", "gear"), categorical = "gear")
  )
  expect_true(any(grepl("Gear, *n*", fixed = TRUE, apa_descriptives(d, variables = gear, categorical = where(is.numeric))$Variable)))
  # Errors
  expect_error(apa_descriptives(d, by = c(am, vs)), "one column")
  expect_error(apa_descriptives(d, by = zzz), "name of a column")
  expect_error(apa_descriptives(d, variables = zzz), "zzz")
  expect_error(apa_descriptives(d, variables = am, by = am), "at least one column")
})
