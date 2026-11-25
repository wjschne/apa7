d_survey <- MASS::survey
pct <- function(x) sprintf("%.1f", x)
# Percentages are padded with figure spaces to line up on the decimal point
unpad <- function(x) gsub("[\u2007 ]", "", x)

test_that("apa_chisq() makes a table of counts and column percentages", {
  tbl <- apa_chisq(d_survey, Exer, by = Sex)
  expect_s3_class(tbl, "apa7_tbl")
  expect_named(tbl, c("Exer", "Female_n", "Female_%", "Male_n", "Male_%", "Total_n", "Total_%"))
  tab <- table(d_survey$Exer, d_survey$Sex)
  expect_equal(tbl$Female_n[1:3], as.integer(tab[, "Female"]))
  expect_equal(unpad(tbl$`Female_%`[1:3]), (pct(100 * as.numeric(prop.table(tab, 2)[, "Female"]))))
  expect_equal(tbl$Exer, c(levels(d_survey$Exer), "Total"))
  expect_equal(tbl$Total_n[4], sum(tab))
  expect_equal(unpad(tbl$`Male_%`[4]), "100.0")
  expect_equal(attr(tbl, "rule_above"), "Total")
})

test_that("variables can be quoted, and two-column data frames work as before", {
  quoted <- apa_chisq(d_survey, "Exer", by = "Sex")
  unquoted <- apa_chisq(d_survey, Exer, by = Sex)
  expect_identical(quoted, unquoted)
  # The first column gives the columns of the table, and the second the rows
  two <- apa_chisq(d_survey[, c("Sex", "Exer")])
  expect_identical(two, unquoted)
})

test_that("percent sets the percentages", {
  tab <- table(d_survey$Exer, d_survey$Sex)
  row <- apa_chisq(d_survey, Exer, by = Sex, percent = "row")
  expect_equal(unpad(row$`Female_%`[1:3]), (pct(100 * as.numeric(prop.table(tab, 1)[, "Female"]))))
  expect_equal(unpad(row$`Total_%`[1:3]), rep("100.0", 3))
  total <- apa_chisq(d_survey, Exer, by = Sex, percent = "total")
  expect_equal(unpad(total$`Female_%`[1:3]), (pct(100 * as.numeric(tab[, "Female"]) / sum(tab))))
  expect_equal(unpad(total$`Total_%`[4]), "100.0")
  none <- apa_chisq(d_survey, Exer, by = Sex, percent = "none", total = FALSE)
  expect_named(none, c("Exer", "Female", "Male"))
  expect_match(apa_note(none), "Values are counts.", fixed = TRUE)
})

test_that("percent_digits sets the decimals of the percentages", {
  tbl <- apa_chisq(d_survey, Exer, by = Sex, percent_digits = 0)
  expect_equal(unpad(tbl$`Female_%`), c("42", "9", "49", "100"))
})

test_that("total = FALSE leaves out the Total row and column", {
  tbl <- apa_chisq(d_survey, Exer, by = Sex, total = FALSE)
  expect_false(any(grepl("Total", colnames(tbl))))
  expect_false("Total" %in% tbl$Exer)
  expect_null(attr(tbl, "rule_above"))
})

test_that("the note reports the chi-square test and Cramer's V", {
  tbl <- apa_chisq(d_survey, Exer, by = Sex)
  ch <- suppressWarnings(chisq.test(table(d_survey$Exer, d_survey$Sex)))
  v <- effectsize::cramers_v(table(d_survey$Exer, d_survey$Sex), adjust = FALSE, ci = NULL)[[1]]
  note <- apa_note(tbl)
  expect_match(note, "percentages of each column (each group of Sex)", fixed = TRUE)
  expect_match(
    note,
    paste0(
      "*&chi;*^2^(2, *N* = 236) = ", sprintf("%.2f", ch$statistic), ", ",
      apa_p(ch$p.value, inline = TRUE), ", Cramér's *V* = ",
      sub("^0", "", sprintf("%.2f", v))
    ),
    fixed = TRUE
  )
  expect_match(note, "(*n* = 1) are not included", fixed = TRUE)
  expect_no_match(note, "Yates", fixed = TRUE)
})

test_that("2 x 2 tables use phi and Yates's correction unless correct = FALSE", {
  d <- carData::Cowles
  tab <- table(d$volunteer, d$sex)
  note <- apa_note(apa_chisq(d, volunteer, by = sex))
  expect_match(note, "*&phi;* = ", fixed = TRUE)
  expect_match(note, "Yates's continuity correction", fixed = TRUE)
  expect_match(note, sprintf("= %.2f,", chisq.test(tab)$statistic), fixed = TRUE)
  uncorrected <- apa_note(apa_chisq(d, volunteer, by = sex, correct = FALSE))
  expect_no_match(uncorrected, "Yates", fixed = TRUE)
  expect_match(uncorrected, sprintf("= %.2f,", chisq.test(tab, correct = FALSE)$statistic), fixed = TRUE)
  expect_match(apa_note(apa_chisq(d, volunteer, by = sex, effect_size = "cramers_v")), "Cramér's *V*", fixed = TRUE)
})

test_that("small expected counts are noted, and Fisher's exact test can be used", {
  note <- apa_note(apa_chisq(d_survey, Smoke, by = Exer))
  expect_match(note, "smallest expected count is 1.07", fixed = TRUE)
  tab <- table(d_survey$Smoke, d_survey$Sex)
  fisher <- apa_note(apa_chisq(d_survey, Smoke, by = Sex, test = "fisher"))
  expect_match(fisher, paste0("Fisher's exact test: ", apa_p(fisher.test(tab)$p.value, inline = TRUE)), fixed = TRUE)
  expect_no_match(fisher, "chi^2", fixed = TRUE)
  none <- apa_note(apa_chisq(d_survey, Smoke, by = Sex, test = "none"))
  expect_no_match(none, "test", fixed = TRUE)
  expect_match(none, "Cramér's *V* = ", fixed = TRUE)
  expect_no_match(apa_note(apa_chisq(d_survey, Smoke, by = Sex, test = "none", effect_size = "none")), "*V*", fixed = TRUE)
})

test_that("labels and format_variable_names name the variables", {
  tbl <- apa_chisq(d_survey, Exer, by = Sex, labels = c(Exer = "Exercise"))
  expect_equal(colnames(tbl)[1], "Exercise")
  expect_match(apa_note(tbl), "Sex", fixed = TRUE)
  raw <- apa_chisq(data.frame(group = c("a", "b", "a", "b"), y = c("x", "x", "z", "z")), y, by = group, format_variable_names = FALSE)
  expect_equal(colnames(raw)[1], "y")
})

test_that("apa_chisq() is displayed with apa_flextable() and its note", {
  ft <- apa_flextable(apa_chisq(d_survey, Exer, by = Sex), note = TRUE)
  expect_s3_class(ft, "flextable")
  expect_equal(nrow(ft$footer$dataset), 1)
})

test_that("deprecated arguments return a flextable with a warning", {
  withr::local_options(rlib_warning_verbosity = "verbose")
  expect_warning(ft <- apa_chisq(d_survey[, c("Sex", "Exer")], note = NA), "deprecated")
  expect_s3_class(ft, "flextable")
  expect_equal(nrow(ft$footer$dataset), 0)
  expect_warning(ft <- apa_chisq(d_survey[, c("Sex", "Exer")], font_size = 10), "font_size")
  expect_equal(nrow(ft$footer$dataset), 1)
  expect_warning(ft <- apa_chisq(d_survey[, c("Sex", "Exer")], note = "A custom note."), "deprecated")
  expect_equal(nrow(ft$footer$dataset), 1)
  expect_error(apa_chisq(d_survey, Exer, by = Sex, colour = "red"), "Unknown argument")
})

test_that("apa_chisq() checks its arguments", {
  expect_error(apa_chisq(matrix(1:4, 2, 2)), "must be a data frame")
  expect_error(apa_chisq(mtcars[, c("am", "gear", "cyl")]), "exactly 2 columns")
  expect_error(apa_chisq(d_survey, Exer), "Give both")
  expect_error(apa_chisq(d_survey, Exer, by = Exer), "different columns")
  expect_error(apa_chisq(d_survey, Exer, by = nonexistent), "must be the name of a column")
  expect_error(apa_chisq(data.frame(a = c(1, 1), b = c(2, 3))), "at least 2 observed values")
  expect_error(apa_chisq(d_survey, Exer, by = Sex, effect_size = "phi"), "2 .* 2 table")
  expect_error(apa_chisq(d_survey, Exer, by = Sex, percent = "cell"))
})
