library(apa7)
library(testthat)

test_that("apa7's BibTeX file is parsed into keys, authors, and years", {
  entries <- apa7:::bib_entries(apa7_bib())
  expect_true(all(c(
    "beggOperatingCharacteristicsRank1994",
    "cheungModelingDependentEffect2014",
    "duvalTrimFillSimple2000",
    "eggerBiasMetaanalysisDetected1997",
    "johnsonTestsCertainLinear1936"
  ) %in% names(entries)))
  expect_equal(entries$eggerBiasMetaanalysisDetected1997$authors[2], "Davey Smith")
  # Every entry has authors and a four-digit year (a malformed entry would not)
  expect_true(all(lengths(lapply(entries, `[[`, "authors")) > 0))
  expect_true(all(grepl("^[0-9]{4}$", vapply(entries, `[[`, character(1), "year"))))
  chars <- strsplit(paste(readLines(apa7_bib()), collapse = ""), "")[[1]]
  expect_equal(sum(chars == "{"), sum(chars == "}"))
  expect_equal(apa7:::citation_text("cheungModelingDependentEffect2014"), "Cheung, 2014")
  expect_equal(apa7:::citation_text("duvalTrimFillSimple2000"), "Duval & Tweedie, 2000")
  expect_equal(apa7:::citation_text("duvalTrimFillSimple2000", narrative = TRUE), "Duval and Tweedie (2000)")
  expect_equal(apa7:::citation_text("eggerBiasMetaanalysisDetected1997"), "Egger et al., 1997")
  expect_equal(
    apa7:::citation_text("satorraScaledAdjustedRestricted2000", narrative = TRUE),
    "Satorra (2000)"
  )
  expect_equal(
    apa7:::citation_text("satorraScaledDifferenceChisquare2001", narrative = TRUE),
    "Satorra and Bentler (2001)"
  )
})

test_that("citation markers become text or keys", {
  s <- c(
    paste0("Method ", apa7:::cite("duvalTrimFillSimple2000"), " here."),
    paste0("As ", apa7:::cite("cheungModelingDependentEffect2014", narrative = TRUE), " showed."),
    "No citation."
  )
  expect_equal(
    apa7:::resolve_citations(s, "text"),
    c("Method (Duval & Tweedie, 2000) here.", "As Cheung (2014) showed.", "No citation.")
  )
  expect_equal(
    apa7:::resolve_citations(s, "keys"),
    c("Method [@duvalTrimFillSimple2000] here.", "As @cheungModelingDependentEffect2014 showed.", "No citation.")
  )
  # Outside a rendered document, auto gives text
  expect_equal(apa7:::resolve_citations(s, "auto"), apa7:::resolve_citations(s, "text"))
})

test_that("auto uses keys only for entries in the document's bibliography", {
  skip_if_not_installed("rmarkdown")
  dir <- withr::local_tempdir()
  bib <- file.path(dir, "refs.bib")
  writeLines(
    c("@article{duvalTrimFillSimple2000,", "  author = {Duval, Sue and Tweedie, Richard},", "  year = 2000", "}"),
    bib
  )
  withr::local_options(knitr.in.progress = TRUE)
  local_mocked_bindings(
    metadata = list(bibliography = bib),
    .package = "rmarkdown"
  )
  s <- c(
    paste0("A ", apa7:::cite("duvalTrimFillSimple2000"), "."),
    paste0("B ", apa7:::cite("cheungModelingDependentEffect2014"), ".")
  )
  expect_equal(
    apa7:::resolve_citations(s, "auto"),
    c("A [@duvalTrimFillSimple2000].", "B (Cheung, 2014).")
  )
})

test_that("apa_note() takes citations as an argument or an option", {
  skip_if_not_installed("metafor")
  skip_if_not_installed("metadat")
  d <- metafor::escalc(
    measure = "RR",
    ai = tpos, bi = tneg, ci = cpos, di = cneg,
    data = metadat::dat.bcg
  )
  tbl <- apa_publication_bias(metafor::rma(yi, vi, data = d))
  expect_match(apa_note(tbl), "Trim and fill (Duval & Tweedie, 2000)", fixed = TRUE)
  expect_match(apa_note(tbl, citations = "keys"), "Trim and fill [@duvalTrimFillSimple2000]", fixed = TRUE)
  withr::local_options(apa7.citations = "keys")
  expect_match(apa_note(tbl), "[@beggOperatingCharacteristicsRank1994]", fixed = TRUE)
  expect_match(apa_note(tbl, citations = "text"), "(Begg & Mazumdar, 1994)", fixed = TRUE)
  expect_error(apa_note(tbl, citations = "bibtex"), "auto")
})

test_that("apa7_write_bib() writes the BibTeX file", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "apa7.bib")
  expect_equal(apa7_write_bib(path), path)
  expect_equal(readLines(path), readLines(apa7_bib()))
  expect_error(apa7_write_bib(path, overwrite = FALSE), "already exists")
  expect_error(apa7_write_bib(NA_character_), "file path")
})

test_that("every key cited in note code is in apa7's BibTeX file", {
  ns <- asNamespace("apa7")
  code <- unlist(lapply(ls(ns, all.names = TRUE), \(name) {
    f <- get(name, envir = ns)
    if (is.function(f)) deparse(f) else character(0)
  }))
  # Keys look like "authorTitleWords1999" (Better BibTeX), whether passed to
  # cite() directly or through switch()
  keys <- unique(unlist(regmatches(code, gregexpr('"[a-z]+[A-Z][A-Za-z]+[0-9]{4}"', code))))
  keys <- gsub('"', "", keys, fixed = TRUE)
  expect_gt(length(keys), 20)
  expect_true(all(keys %in% names(apa7:::bib_entries(apa7_bib()))), info = paste(setdiff(keys, names(apa7:::bib_entries(apa7_bib()))), collapse = ", "))
})

test_that("named procedures are cited as keys in keys mode", {
  skip_on_cran()
  fit <- lm(mpg ~ wt + factor(cyl), data = mtcars)
  expect_match(
    apa_note(apa_collinearity(fit), citations = "keys"),
    "generalized *VIF* [@foxGeneralizedCollinearityDiagnostics1992]",
    fixed = TRUE
  )
  g0 <- glm(am ~ wt, data = mtcars, family = binomial)
  g1 <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  expect_match(
    apa_note(apa_parameters(list(g0, g1)), citations = "keys"),
    "*R*^2^~Tjur~ = Tjur's coefficient of discrimination [@tjurCoefficientsDeterminationLogistic2009]",
    fixed = TRUE
  )
  expect_match(
    apa_note(apa_parameters(list(g0, g1))),
    "&Delta;*R*^2^~Tjur~ = change in *R*^2^~Tjur~",
    fixed = TRUE
  )
})
