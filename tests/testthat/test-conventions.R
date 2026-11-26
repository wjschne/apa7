# Checks of the rules in dev/CONVENTIONS.md marked "Checked". Rule IDs refer
# to that file. Known departures are listed with the number of their row in
# its "Open inconsistencies" table, and should be removed here when fixed.

ns <- asNamespace("apa7")
exported <- sort(getNamespaceExports("apa7"))
exported <- exported[vapply(exported, \(f) is.function(get(f, ns)), logical(1))]

table_generics <- c(
  "apa_anova", "apa_chisq", "apa_collinearity", "apa_contrasts", "apa_cor",
  "apa_descriptives", "apa_factor_cor", "apa_htest", "apa_loadings",
  "apa_means", "apa_n_factors", "apa_parameters", "apa_performance",
  "apa_performance_comparison", "apa_publication_bias", "apa_reliability",
  "apa_slopes", "apa_subgroups"
)
# Table functions and their S3 methods
table_functions <- ls(ns, all.names = TRUE)
table_functions <- table_functions[
  table_functions %in% table_generics |
    sub("[.].*$", "", table_functions) %in% table_generics &
      grepl(".", table_functions, fixed = TRUE)
]

arg_names <- function(f) names(formals(get(f, ns)))
after_dots <- function(f) {
  a <- arg_names(f)
  i <- match("...", a)
  if (is.na(i)) character(0) else a[-seq_len(i)]
}

# Help pages from the source package when it is available (devtools::test()),
# otherwise from the installed package (R CMD check)
pkg_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = FALSE)
rd_db <- if (file.exists(file.path(pkg_root, "DESCRIPTION")) && dir.exists(file.path(pkg_root, "man"))) {
  tools::Rd_db(dir = pkg_root)
} else {
  tools::Rd_db("apa7")
}
rd_aliases <- lapply(rd_db, \(rd) {
  unlist(lapply(rd[vapply(rd, attr, "", "Rd_tag") == "\\alias"], as.character))
})
rd_for <- function(f) {
  rd_db[[which(vapply(rd_aliases, \(a) f %in% a, logical(1)))[1]]]
}
rd_section <- function(rd, tag) {
  parts <- rd[vapply(rd, attr, "", "Rd_tag") == tag]
  paste(unlist(lapply(parts, \(p) paste(unlist(p), collapse = ""))), collapse = " ")
}

test_that("ARG-1: the first argument of a table function is fit, data, or x", {
  for (f in table_functions) {
    expect_true(arg_names(f)[1] %in% c("fit", "data", "x", "..."), label = f)
  }
})

test_that("ARG-2: renamed argument names are not used for new arguments", {
  # Old names that 0.3.0 replaced. They may remain only as deprecated
  # arguments after `...` (ARG-7).
  retired <- c("p_value", "d")
  for (f in exported) {
    expect_false(any(retired %in% setdiff(arg_names(f), after_dots(f))), label = f)
  }
})

test_that("ARG-2: shared arguments have the shared defaults", {
  default_of <- function(f, a) formals(get(f, ns))[[a]]
  for (f in c(exported, table_functions)) {
    a <- arg_names(f)
    if ("alpha" %in% a && f != "apa_p_star_note") {
      expect_equal(eval(default_of(f, "alpha")), c(.05, .01, .001), label = paste(f, "alpha"))
    }
    if ("ci" %in% a) {
      expect_equal(default_of(f, "ci"), .95, label = paste(f, "ci"))
    }
    if ("digits" %in% a && f != "sci_md") {
      expect_equal(default_of(f, "digits"), 2, label = paste(f, "digits"))
    }
    for (sel in intersect(c("starred", "bolded"), a)) {
      d <- eval(default_of(f, sel))
      expect_true(identical(d, NA) || is.character(d), label = paste(f, sel))
    }
    for (setting in intersect(c("column_formats", "table_width", "accuracy"), a)) {
      expect_null(default_of(f, setting), label = paste(f, setting))
    }
  }
})

test_that("ARG-3: no function takes both digits and accuracy", {
  for (f in exported) {
    expect_false(all(c("digits", "accuracy") %in% arg_names(f)), label = f)
  }
})

test_that("ARG-7: deprecated arguments come after the current ones", {
  deprecated <- list(
    apa_cor = "p_value",
    apa_p_star_note = "x",
    add_break_columns = "d"
  )
  for (f in names(deprecated)) {
    a <- arg_names(f)
    # After `...`, or last when the function has no `...`
    ok <- if ("..." %in% a) deprecated[[f]] %in% after_dots(f) else utils::tail(a, 1) == deprecated[[f]]
    expect_true(ok, label = f)
  }
})

test_that("ARG-8: argument names are snake_case", {
  exceptions <- c("...", ".before", ".data", "launch.browser", "NA_value")
  for (f in c(exported, table_functions)) {
    a <- setdiff(arg_names(f), exceptions)
    expect_true(all(grepl("^[a-z][a-z0-9_]*$", a)), label = paste(f, paste(a, collapse = ", ")))
  }
})

test_that("RET-1: table functions document an apa7_tbl return value", {
  known <- c(
    apa_cor = 2,
    apa_parameters = 6, apa_performance = 6, apa_performance_comparison = 6
  )
  for (f in setdiff(table_generics, names(known))) {
    expect_match(rd_section(rd_for(f), "\\value"), "apa7_tbl", label = f)
  }
})

test_that("DOC-3: every exported function has examples", {
  for (f in exported) {
    expect_true(nzchar(rd_section(rd_for(f), "\\examples")), label = f)
  }
})

test_that("DOC-5: every method cited in a note is in a help page's references", {
  bib <- readLines(system.file("apa7.bib", package = "apa7"), encoding = "UTF-8")
  # Citation keys in the package code (strings passed to cite())
  code <- unlist(lapply(ls(ns, all.names = TRUE), \(f) {
    obj <- get(f, ns)
    if (is.function(obj)) deparse(obj) else character(0)
  }))
  keys <- unique(unlist(regmatches(code, gregexpr("\"[a-z]+[A-Z][A-Za-z]+[0-9]{4}\"", code))))
  keys <- gsub("\"", "", keys)
  keys <- keys[keys %in% sub("^@[a-z]+\\{([^,]+),.*$", "\\1", bib)]
  expect_gt(length(keys), 0)
  references <- paste(vapply(rd_db, rd_section, "", "\\references"), collapse = " ")
  for (k in keys) {
    # The key is the first author's surname, a title word, and the year
    surname <- sub("^([a-z]+).*$", "\\1", k)
    year <- sub("^.*([0-9]{4})$", "\\1", k)
    found <- grepl(
      paste0("(?i)\\b", surname, "\\b[^()]*\\(", year, "\\)"),
      references,
      perl = TRUE
    )
    expect_true(found, label = k)
  }
})
