# Convert aligned, padded strings back to numbers (positive values only)
to_num <- function(v) as.numeric(gsub("[^0-9.]", "", v))

# Deterministic rotations (varimax, promax) keep factor labels stable
fit_v <- psych::fa(Harman74.cor$cov, 4, fm = "pa", rotate = "varimax")
fit_p <- psych::fa(Harman74.cor$cov, 4, fm = "pa", rotate = "promax")

test_that("apa_loadings returns an apa7_tbl of loadings", {
  x <- apa_loadings(fit_v)
  expect_s3_class(x, "apa7_tbl")
  expect_equal(nrow(x), 24L)
  expect_equal(colnames(x), c("Variable", colnames(unclass(fit_v$loadings))))
  expect_true(all(vapply(x, is.character, logical(1))))
  expect_equal(attr(x, "no_format_columns"), "Variable")
  # Variable names are formatted
  expect_true("Sentence Completion" %in% x$Variable)
})

test_that("apa_loadings blanks small loadings", {
  L <- unclass(fit_v$loadings)
  x <- apa_loadings(fit_v, sort_loading = FALSE, min_loading = .3)
  expect_equal(x[[2]] == "", abs(L[, 1]) < .3, ignore_attr = TRUE)

  x_all <- apa_loadings(fit_v, min_loading = NA)
  expect_false(any(as.matrix(x_all[, -1]) == ""))
  expect_false(any(as.matrix(apa_loadings(fit_v, min_loading = 0)[, -1]) == ""))
})

test_that("apa_loadings sorts rows", {
  x_unsorted <- apa_loadings(fit_v, sort_loading = FALSE, format_variable_names = FALSE)
  expect_equal(x_unsorted$Variable, rownames(unclass(fit_v$loadings)))
  x_sorted <- apa_loadings(fit_v, format_variable_names = FALSE)
  expect_equal(x_sorted$Variable, rownames(unclass(psych::fa.sort(fit_v)$loadings)))
})

test_that("apa_loadings renames factors", {
  fc <- colnames(unclass(fit_v$loadings))
  new <- c("A", "B", "C", "D")
  expect_equal(colnames(apa_loadings(fit_v, factor_names = new))[-1], new)

  x <- apa_loadings(fit_v, factor_names = stats::setNames("Verbal", fc[2]))
  expect_equal(colnames(x)[-1], replace(fc, 2, "Verbal"))
  # Loadings stay with their factor
  expect_equal(x$Verbal, apa_loadings(fit_v)[[fc[2]]])

  expect_error(apa_loadings(fit_v, factor_names = c("A", "B")), "4 factors")
  expect_error(apa_loadings(fit_v, factor_names = c(PA9 = "A")), "Unknown factor")
})

test_that("apa_loadings optional columns", {
  x <- apa_loadings(
    fit_v,
    sort_loading = FALSE,
    communality = TRUE,
    uniqueness = TRUE,
    complexity = TRUE
  )
  expect_equal(utils::tail(colnames(x), 3), c("*h*^2^", "*u*^2^", "Complexity"))
  expect_equal(
    to_num(x[["*h*^2^"]]),
    round(unname(fit_v$communality), 2),
    tolerance = .011
  )
  expect_equal(
    to_num(x[["*u*^2^"]]) + to_num(x[["*h*^2^"]]),
    rep(1, 24),
    tolerance = .011
  )
  expect_false(any(startsWith(trimws(x[["*h*^2^"]]), "0.")))
})

test_that("apa_loadings variance rows", {
  x <- apa_loadings(fit_v, variance = TRUE, communality = TRUE)
  expect_equal(nrow(x), 27L)
  expect_equal(
    utils::tail(x$Variable, 3),
    c("Sum of squared loadings", "Proportion of variance", "Cumulative proportion")
  )
  fc <- colnames(unclass(fit_v$loadings))
  ss <- as.numeric(x[25, fc])
  expect_equal(ss, unname(round(fit_v$Vaccounted["SS loadings", fc], 2)))
  cumulative <- as.numeric(x[27, fc])
  expect_equal(
    cumulative,
    unname(round(cumsum(fit_v$Vaccounted["Proportion Var", fc]), 2))
  )
  # No NA cells
  expect_false(anyNA(x))
  expect_equal(x[["*h*^2^"]][25:27], rep("", 3))

  # One factor: no cumulative row
  f1 <- psych::fa(Harman74.cor$cov, 1)
  x1 <- apa_loadings(f1, variance = TRUE)
  expect_equal(nrow(x1), 26L)
  expect_false("Cumulative proportion" %in% x1$Variable)
})

test_that("apa_loadings keeps item text when format_variable_names = FALSE", {
  d <- psych::bfi[1:300, 1:5]
  items <- c(
    "am indifferent to others",
    "Inquire about others' well-being",
    "Know how to comfort others",
    "Love children",
    "Make people feel at ease"
  )
  colnames(d) <- items
  fit <- psych::fa(d, 1)
  x <- apa_loadings(fit, format_variable_names = FALSE)
  expect_setequal(x$Variable, items)

  # apa_flextable does not reformat the Variable column, even after dplyr verbs
  ft <- apa_flextable(dplyr::rename(x, Loading = MR1))
  expect_setequal(ft$body$dataset$Variable, items)
})

test_that("apa_loadings works with psych::principal", {
  pc <- psych::principal(Harman74.cor$cov, 3, rotate = "varimax")
  x <- apa_loadings(pc, variance = TRUE)
  expect_s3_class(x, "apa7_tbl")
  expect_equal(colnames(x), c("Variable", "RC1", "RC2", "RC3"))
  expect_equal(nrow(x), 27L)
  expect_no_error(apa_flextable(x))
})

test_that("apa_loadings rejects other objects", {
  expect_error(apa_loadings(lm(mpg ~ wt, mtcars)), "psych::fa")
  expect_error(apa_loadings(mtcars), "data frame")
  expect_error(apa_factor_cor(lm(mpg ~ wt, mtcars)), "psych::fa")
})

test_that("apa_factor_cor", {
  x <- apa_factor_cor(fit_p)
  fc <- colnames(unclass(fit_p$loadings))
  expect_s3_class(x, "apa7_tbl")
  expect_equal(colnames(x), c("Factor", "1", "2", "3", "4"))
  expect_equal(x$Factor, paste0(1:4, ". ", fc))
  expect_equal(unname(diag(as.matrix(x[, -1]))), rep(intToUtf8(8212), 4))
  expect_true(all(as.matrix(x[, -1])[upper.tri(diag(4))] == ""))
  expect_equal(
    to_num(x[[2]][2:4]),
    unname(round(fit_p$Phi[fc[2:4], fc[1]], 2))
  )
  expect_false(any(grepl("0.", as.matrix(x[, -1]), fixed = TRUE)))

  x2 <- apa_factor_cor(fit_p, factor_names = c("A", "B", "C", "D"))
  expect_equal(x2$Factor, paste0(1:4, ". ", c("A", "B", "C", "D")))
  expect_no_error(apa_flextable(x2))

  pc <- psych::principal(Harman74.cor$cov, 3, rotate = "promax")
  expect_equal(nrow(apa_factor_cor(pc)), 3L)
})

test_that("apa_factor_cor errors without factor correlations", {
  expect_error(apa_factor_cor(fit_v), "orthogonal")
  expect_error(apa_factor_cor(psych::fa(Harman74.cor$cov, 1)), "one-factor")
})

test_that("apa_loadings bold_max bolds the largest displayed loading", {
  skip_on_cran()
  x <- apa_loadings(fit_v, sort_loading = FALSE, bold_max = TRUE)
  m <- as.matrix(x[, -1])
  L <- abs(unclass(fit_v$loadings))
  bolded <- grepl("**", m, fixed = TRUE)
  expect_equal(rowSums(matrix(bolded, nrow = 24)), rep(1, 24))
  expect_equal(
    apply(matrix(bolded, nrow = 24), 1, which),
    unname(apply(L, 1, which.max))
  )
  # Padding stays outside the markdown so that the bold text is parsed
  expect_false(any(grepl("** ", m, fixed = TRUE)))
  expect_false(any(grepl(paste0("**", intToUtf8(8199)), m, fixed = TRUE)))

  # Hidden loadings are never bolded
  x_hi <- apa_loadings(fit_v, min_loading = .99, bold_max = TRUE)
  expect_false(any(grepl("**", as.matrix(x_hi[, -1]), fixed = TRUE)))
  expect_no_error(apa_flextable(x))
})

test_that("apa_flextable draws a rule above the variance rows", {
  skip_on_cran()
  x <- apa_loadings(fit_v, variance = TRUE)
  expect_equal(attr(x, "rule_above"), "Sum of squared loadings")
  ft <- apa_flextable(x)
  bottom <- ft$body$styles$cells$border.width.bottom$data[, 1]
  expect_true(bottom[24] > 0)
  expect_equal(bottom[23], 0)

  # The rule follows the row after filtering
  ft2 <- apa_flextable(dplyr::filter(x, !grepl("Word", Variable)))
  bottom2 <- ft2$body$styles$cells$border.width.bottom$data[, 1]
  n_items <- sum(!grepl("Word", x$Variable)) - 3
  expect_true(bottom2[n_items] > 0)

  # No rule without variance rows
  ft3 <- apa_flextable(apa_loadings(fit_v))
  expect_equal(sum(ft3$body$styles$cells$border.width.bottom$data[1:23, 1]), 0)
})

test_that("apa_loadings and apa_factor_cor accept parameters output", {
  d <- psych::bfi[1:500, 1:10]
  e <- parameters::factor_analysis(
    d,
    n = 2,
    rotation = "oblimin",
    threshold = "max"
  )
  model <- attr(e, "model")
  # The threshold in parameters does not hide loadings in apa_loadings
  expect_equal(
    apa_loadings(e, min_loading = 0),
    apa_loadings(model, min_loading = 0)
  )
  expect_equal(apa_factor_cor(e), apa_factor_cor(model))

  # Unrotated components wrap a prcomp model
  pc <- parameters::principal_components(d, n = 3)
  x <- apa_loadings(pc, min_loading = 0, variance = TRUE, communality = TRUE)
  expect_equal(colnames(x)[2:4], c("PC1", "PC2", "PC3"))
  tab <- as.data.frame(pc)
  x_unsorted <- apa_loadings(pc, sort_loading = FALSE, min_loading = 0)
  expect_equal(to_num(sub(intToUtf8(8722), "", x_unsorted$PC1)), round(abs(tab$PC1), 2))
  expect_equal(
    to_num(x[nrow(x) - 2, 2:4]),
    round(attr(pc, "summary")$Eigenvalues, 2)
  )
  expect_error(apa_factor_cor(pc), "orthogonal")

  # Rotated components wrap a psych::principal model
  pc_rot <- parameters::principal_components(d, n = 3, rotation = "varimax")
  expect_equal(apa_loadings(pc_rot), apa_loadings(attr(pc_rot, "model")))
})

# apa_note ----
test_that("apa_note describes loadings tables", {
  expect_equal(
    apa_note(apa_loadings(fit_v), collapse = NULL),
    c(
      "Principal axis factoring with varimax rotation.",
      "Loadings smaller than .20 in absolute value are omitted."
    )
  )

  notes <- apa_note(
    apa_loadings(
      fit_v,
      min_loading = .3,
      bold_max = TRUE,
      communality = TRUE,
      uniqueness = TRUE,
      complexity = TRUE
    ),
    collapse = NULL
  )
  expect_equal(notes[2], "Loadings smaller than .30 in absolute value are omitted.")
  expect_equal(notes[3], "The largest loading for each variable is in bold.")
  expect_equal(
    notes[4],
    "*h*^2^ = communality; *u*^2^ = uniqueness; Complexity = Hofmann's complexity index."
  )

  # No cutoff sentence when every loading is shown
  expect_false(grepl("omitted", apa_note(apa_loadings(fit_v, min_loading = 0))))
  expect_false(grepl("omitted", apa_note(apa_loadings(fit_v, min_loading = NA))))
  expect_match(apa_note(apa_loadings(fit_v, min_loading = .25)), "than .25 in", fixed = TRUE)
})

test_that("apa_note describes oblique solutions", {
  notes <- apa_note(apa_loadings(fit_p, variance = TRUE), collapse = NULL)
  expect_equal(notes[1], "Principal axis factoring with promax rotation.")
  expect_equal(notes[2], "Loadings are pattern coefficients.")
  expect_match(utils::tail(notes, 1), "^Because the factors are correlated")

  # Orthogonal rotation: no overlap sentence
  expect_false(grepl("overlap", apa_note(apa_loadings(fit_v, variance = TRUE))))

  expect_equal(
    apa_note(apa_factor_cor(fit_p)),
    "Principal axis factoring with promax rotation."
  )
})

test_that("apa_note names extraction methods and handles special cases", {
  f_ml <- psych::fa(Harman74.cor$cov, 2, fm = "ml", rotate = "varimax")
  expect_match(apa_note(apa_loadings(f_ml)), "^Maximum likelihood factor analysis with varimax")

  # One factor: no rotation is mentioned
  f1 <- psych::fa(Harman74.cor$cov, 1, fm = "minres")
  expect_match(apa_note(apa_loadings(f1)), "^Minimum residual factor analysis[.]")

  pc <- psych::principal(Harman74.cor$cov, 3, rotate = "promax")
  pc_notes <- apa_note(apa_loadings(pc, variance = TRUE), collapse = NULL)
  expect_equal(pc_notes[1], "Principal components analysis with promax rotation.")
  expect_match(utils::tail(pc_notes, 1), "components are correlated", fixed = TRUE)

  pc0 <- parameters::principal_components(mtcars[, 1:7], n = 2)
  expect_match(apa_note(apa_loadings(pc0)), "^Principal components analysis without rotation[.]")

  f_geo <- psych::fa(Harman74.cor$cov, 3, rotate = "geominQ")
  expect_match(apa_note(apa_loadings(f_geo)), "geomin (oblique) rotation", fixed = TRUE)
})
