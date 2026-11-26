library(apa7)
library(testthat)

bfi_scales <- list(Agreeableness = paste0("A", 1:5), Conscientiousness = paste0("C", 1:5))
bfi_reverse <- c("A1", "C4", "C5")

reversed_items <- function(vars, reverse) {
  x <- stats::na.omit(psych::bfi[vars])
  for (v in intersect(reverse, vars)) x[[v]] <- 7 - x[[v]]
  x
}

test_that("item rows match psych::alpha(), with scale statistics below", {
  skip_on_cran()
  result <- apa_reliability(
    psych::bfi,
    scales = bfi_scales,
    reverse = bfi_reverse,
    item_range = c(1, 6)
  )
  expect_equal(colnames(result), c("Item", "*M*", "*SD*", "*r*~it~", "&alpha; if deleted"))
  em <- " "
  expect_equal(
    result$Item[1:10],
    c(
      "Agreeableness",
      paste0(em, c("A1 (R)", "A2", "A3", "A4", "A5")),
      paste0(em, c(
        "Scale mean (*k* = 5)", "&alpha; [95% CI]", "*&omega;*~total~", "Mean inter-item *r*"
      ))
    )
  )
  x <- reversed_items(bfi_scales$Agreeableness, bfi_reverse)
  a <- suppressWarnings(psych::alpha(x, check.keys = FALSE, warnings = FALSE))
  expect_equal(as_num(result$`*r*~it~`[2:6]), round(a$item.stats$r.drop, 2))
  expect_equal(as_num(result$`&alpha; if deleted`[2:6]), round(a$alpha.drop$raw_alpha, 2))
  expect_equal(as_num(result$`*M*`[2]), round(mean(x$A1), 2))
  # Scale mean and alpha with Feldt's interval
  expect_equal(as_num(result$`*M*`[7]), round(mean(rowMeans(x)), 2))
  expect_equal(trimws(result$`*M*`[8]), ".70 [.69, .72]")
  expect_equal(as_num(result$`*M*`[10]), round(a$total$average_r, 2))

  note <- apa_note(result)
  expect_match(note, "Agreeableness: *n* = 2,709; Conscientiousness: *n* = 2,707", fixed = TRUE)
  expect_match(note, "Items marked (R) were reverse-scored.", fixed = TRUE)
  expect_match(note, "CI = 95% confidence interval (Feldt et al., 1987)", fixed = TRUE)
  # Labels are not formatted again in flextables
  expect_equal(apa_flextable(result)$body$dataset[[1]], result$Item)
})

test_that("omega matches psych::omega(); one scale has no heading", {
  x <- reversed_items(paste0("A", 1:5), "A1")
  result <- apa_reliability(x)
  expect_equal(result$Item[1], "A1")
  o <- suppressWarnings(suppressMessages(psych::omega(x, nfactors = 1, plot = FALSE)))
  omega_row <- result$Item == "*&omega;*~total~"
  expect_equal(as_num(result$`*M*`[omega_row]), round(o$omega.tot, 2))
  expect_match(apa_note(result), "*N* = 2,709 respondents with complete responses.", fixed = TRUE)

  no_omega <- apa_reliability(x, omega = FALSE)
  expect_false("*&omega;*~total~" %in% no_omega$Item)
  expect_no_match(apa_note(no_omega), "omega", fixed = TRUE)

  sums <- apa_reliability(x, score = "sum", ci = .90)
  expect_true("Scale sum (*k* = 5)" %in% sums$Item)
  expect_true("&alpha; [90% CI]" %in% sums$Item)
  expect_match(apa_note(sums), "sums of the items", fixed = TRUE)
})

test_that("one row per scale with level = 'scales'", {
  result <- apa_reliability(
    psych::bfi,
    scales = bfi_scales,
    reverse = bfi_reverse,
    item_range = c(1, 6),
    level = "scales"
  )
  expect_equal(
    colnames(result),
    c("Scale", "*k*", "*M*", "*SD*", "&alpha;", "95% CI", "*&omega;*~total~", "Mean *r*")
  )
  expect_equal(result$Scale, c("Agreeableness", "Conscientiousness"))
  expect_true(is_ci_md(result$`95% CI`))
  expect_match(apa_note(result), "*k* = number of items", fixed = TRUE)
  expect_match(apa_note(result), "Reverse-keyed items were reverse-scored.", fixed = TRUE)
})

test_that("dichotomous items are described as difficulty and KR-20; flags bold weak items", {
  set.seed(1)
  theta <- rnorm(300)
  b <- as.data.frame(sapply(seq(-1, 1, length.out = 6), \(d) rbinom(300, 1, plogis(theta - d))))
  result <- apa_reliability(b, flag = TRUE)
  expect_true("&alpha; (KR-20) [95% CI]" %in% result$Item)
  note <- apa_note(result)
  expect_match(note, "*M* is item difficulty (the proportion of 1s)", fixed = TRUE)
  expect_match(note, "Bold values are item", fixed = TRUE)
  # Item-total correlations below .30 are bolded
  rit <- result$`*r*~it~`[1:6]
  expect_true(any(grepl("**", rit, fixed = TRUE)))
  expect_true(all(grepl("**", rit, fixed = TRUE) == (as_num(rit) < .30)))
})

test_that("apa_reliability() checks its arguments", {
  d <- psych::bfi
  expect_error(apa_reliability(as.matrix(d)), "data frame")
  expect_error(apa_reliability(d, scales = list(c("A1", "A2"))), "named list")
  expect_error(apa_reliability(d, scales = list(X = c("A1", "Z9"))), "Z9")
  expect_error(apa_reliability(d, scales = list(X = "A1")), "at least 2 items")
  expect_error(apa_reliability(d[1:2, 1:3]), "at least 3 respondents")
  expect_error(apa_reliability(d[1:3], ci = 2), "between 0 and 1")
  expect_error(
    apa_reliability(data.frame(a = 1:5, b = letters[1:5])),
    "must be numeric"
  )
})

test_that("scales and reverse take unquoted names and tidyselect helpers", {
  d <- psych::bfi
  quoted <- apa_reliability(d, scales = bfi_scales, reverse = bfi_reverse, item_range = c(1, 6))
  expect_identical(
    apa_reliability(
      d,
      scales = list(Agreeableness = A1:A5, Conscientiousness = c(C1, C2, C3, C4, C5)),
      reverse = c(A1, C4, C5),
      item_range = c(1, 6)
    ),
    quoted
  )
  expect_identical(
    apa_reliability(
      d,
      scales = base::list(Agreeableness = starts_with("A", ignore.case = FALSE), Conscientiousness = num_range("C", 1:5)),
      reverse = tidyselect::all_of(bfi_reverse),
      item_range = c(1, 6)
    ),
    quoted
  )
  # Quoted names inside list(), and character vectors computed there
  expect_identical(
    apa_reliability(
      d,
      scales = list(Agreeableness = paste0("A", 1:5), Conscientiousness = c("C1", "C2", "C3", "C4", "C5")),
      reverse = c("A1", "C4", "C5"),
      item_range = c(1, 6)
    ),
    quoted
  )
  # Through a wrapper function with {{ }}
  wrapper <- function(data, s, r) apa_reliability(data, scales = {{ s }}, reverse = {{ r }}, item_range = c(1, 6))
  expect_identical(wrapper(d, bfi_scales, c(A1, C4, C5)), quoted)
  expect_error(apa_reliability(d, scales = list(A1:A5)), "named list")
  expect_error(apa_reliability(d, scales = c("A1", "A2")), "named list")
  expect_error(apa_reliability(d, scales = list(X = c(A1, Z9))), "Z9")
  expect_error(apa_reliability(d, scales = list(X = A1:A5), reverse = Z9), "Z9")
})

test_that("named character vectors select items by their values", {
  d <- psych::bfi[, paste0("A", 1:5)]
  labels <- c(A1 = "Indifferent", A2 = "Inquire", A3 = "Comfort", A4 = "Children", A5 = "Ease")
  names(d) <- labels
  named <- apa_reliability(d, scales = list(Agreeableness = labels), reverse = labels["A1"], item_range = c(1, 6))
  plain <- apa_reliability(d, scales = list(Agreeableness = unname(labels)), reverse = "Indifferent", item_range = c(1, 6))
  expect_identical(named, plain)
  expect_true(any(grepl("Indifferent (R)", named$Item, fixed = TRUE)))
})

test_that("select_names() ignores the names of a selection", {
  d <- data.frame(a = 1, b = 2, c = 3)
  vars <- c(first = "a", second = "c")
  expect_equal(select_names(rlang::quo(vars), d), c("a", "c"))
  expect_equal(select_names(rlang::quo(c(x = a, y = b)), d), c("a", "b"))
  expect_equal(select_names(rlang::quo(starts_with("b")), d), "b")
})

test_that("flag takes a cutoff for item-total correlations", {
  d <- psych::bfi[, paste0("A", 1:5)]
  flagged <- apa_reliability(d, reverse = "A1", item_range = c(1, 6), flag = .45)
  rit <- flagged$`*r*~it~`[1:5]
  expect_true(all(grepl("**", rit, fixed = TRUE) == (as_num(rit) < .45)))
  expect_match(apa_note(flagged), "below .45", fixed = TRUE)
  expect_identical(
    apa_reliability(d, flag = TRUE),
    apa_reliability(d, flag = .30)
  )
  expect_false(any(grepl("**", apa_reliability(d)$`*r*~it~`, fixed = TRUE)))
  expect_error(apa_reliability(d, flag = "yes"), "must be")
})
