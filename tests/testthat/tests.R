library(apa7)
library(testthat)
test_that("apa_p", {
  expect_equal(apa_p(0, inline = TRUE, markdown = TRUE),
               "*p*\u00A0<\u00A0.001")
  expect_equal(apa_p(0, inline = FALSE), "<.001")
  expect_equal(apa_p(0.0007, inline = T, markdown = TRUE),
               "*p*\u00A0<\u00A0.001")
  expect_equal(apa_p(0.001, inline = T, markdown = TRUE),
               "*p*\u00A0=\u00A0.001")
  expect_equal(apa_p(0.000999, inline = T, markdown = TRUE),
               "*p*\u00A0<\u00A0.001")
  expect_equal(apa_p(0.000999, FALSE, markdown = TRUE), "<.001")
  expect_equal(apa_p(0.001, inline = T, markdown = TRUE),
               "*p*\u00A0=\u00A0.001")
  expect_equal(apa_p(0.005, FALSE), ".005")
  expect_equal(apa_p(0.0081, inline = T, markdown = TRUE),
               "*p*\u00A0=\u00A0.008")
  expect_equal(apa_p(0.00999, FALSE), ".01")
  expect_equal(apa_p(0.00999, inline = T, markdown = TRUE),
               "*p*\u00A0=\u00A0.01")
  expect_equal(apa_p(0.00999, FALSE), ".01")
  expect_equal(apa_p(0.0999, inline = TRUE, markdown = TRUE),
               "*p*\u00A0=\u00A0.10")
  expect_equal(apa_p(0.0999, inline = FALSE), ".10")
  expect_equal(apa_p(0.0999, inline = T, markdown = TRUE),
               "*p*\u00A0=\u00A0.10")
  expect_equal(apa_p(0.0999, FALSE), ".10")
  expect_equal(apa_p(0.0999, inline = TRUE, markdown = FALSE), "p = .10")
  expect_equal(apa_p(0, inline = TRUE, markdown = FALSE), "p < .001")
  expect_equal(
    apa_p(
      c(.0001, .001, .01, .5, .99, .995),
      inline = FALSE,
      markdown = FALSE
    ),
    c("<.001", ".001", ".01", ".50", ".99", ">.99")
  )
  expect_equal(
    apa_p(
      c(.0001, .001, .01, .5, .99, .995),
      inline = FALSE,
      markdown = TRUE
    ),
    c("<.001", ".001", ".01", ".50", ".99", "\\>.99")
  )

})


test_that("align_chr", {
  posnum <- c(.010, .511)
  expect_equal(align_chr(posnum , accuracy = .01), c("0.01", "0.51"))
  expect_equal(align_chr(posnum , accuracy = .001), c("0.010", "0.511"))

  expect_equal(align_chr(posnum , accuracy = .01, trim_leading_zeros = T),
               c(".01", ".51"))
  expect_equal(align_chr(posnum , accuracy = .001, trim_leading_zeros = T),
               c(".010", ".511"))

  expect_equal(align_chr(posnum), align_chr(align_chr(posnum)))

  negnum <- posnum * -1
  expect_equal(align_chr(negnum , accuracy = .01),
               c("\u22120.01", "\u22120.51"))
  expect_equal(align_chr(negnum , accuracy = .001),
               c("\u22120.010", "\u22120.511"))

  expect_equal(
    align_chr(negnum , accuracy = .01, trim_leading_zeros = T),
    c("\u2212.01", "\u2212.51")
  )
  expect_equal(
    align_chr(negnum , accuracy = .001, trim_leading_zeros = T),
    c("\u2212.010", "\u2212.511")
  )

  expect_equal(align_chr(negnum), align_chr(align_chr(negnum)))

  posnegnum <- c(-.010, .511)
  expect_equal(align_chr(posnegnum , accuracy = .01),
               c("\u22120.01", "\u20070.51"))
  expect_equal(align_chr(posnegnum , accuracy = .001),
               c("\u22120.010", "\u20070.511"))

  expect_equal(
    align_chr(
      posnegnum ,
      accuracy = .01,
      trim_leading_zeros = T
    ),
    c("\u2212.01", "\u2007.51")
  )
  expect_equal(
    align_chr(
      posnegnum ,
      accuracy = .001,
      trim_leading_zeros = T
    ),
    c("\u2212.010", "\u2007.511")
  )

  expect_equal(align_chr(posnegnum), align_chr(align_chr(posnegnum)))

  # equal text without .
  expect_equal(align_chr(c("a", "b", "c")), c("a", "b", "c"))
  # unequal text without .
  expect_equal(align_chr(c("aa", "ab", "c", "ad")), c("aa", "ab", "\u2007c", "ad"))
  # equal text with trailing .
  expect_equal(align_chr(c("a.", "b.", "c.")), c("a.", "b.", "c."))
  # unequal text with trailing .
  expect_equal(align_chr(c("aa", "ab", "c", "ad")), c("aa", "ab", "\u2007c", "ad"))

  # equal text with prefix .
  expect_equal(align_chr(c(".a", ".b", ".c")), c(".a", ".b", ".c"))
  # unequal text with prefix .
  expect_equal(align_chr(c(".aa", ".ab", ".c", ".ad")), c(".aa", ".ab", ".c\u2007", ".ad"))

  # equal text with middle .
  expect_equal(align_chr(c("a.a", "a.b", "a.c")), c("a.a", "a.b", "a.c"))
  # unequal text with middle .
  expect_equal(
    align_chr(c(
      "a.aa", "aa.a", "c.c", "aa.ad", ".", "a.", ".a"
    )),
    c(
      "\u2007a.aa",
      "aa.a\u2007",
      "\u2007c.c\u2007",
      "aa.ad",
      "\u2007\u2007.\u2007\u2007",
      "\u2007a.\u2007\u2007",
      "\u2007\u2007.a\u2007"
    )
  )

  # equal text with two periods
  expect_equal(align_chr(c(".a.", ".b.", ".c.")), c(".a.", ".b.", ".c."))

  # unequal text with two periods
  expect_equal(align_chr(c("0.a.", ".b.0", ".c.", "0.a.c")),
               c("0.a.\u2007", "\u2007.b.0", "\u2007.c.\u2007", "0.a.c"))

  align_chr(c(".1", ".12^\\*\\*^")) |> align_chr()


})

test_that("p2stars", {
  expect_equal(p2stars(0.0001), "\\*\\*\\*")
  expect_equal(p2stars(0.0001, superscript = TRUE), "^\\*\\*\\*^")
  expect_equal(p2stars(
    0.0001,
    superscript = TRUE,
    add_trailing_space = TRUE
  ),
  "^\\*\\*\\*\u2009^")
  expect_equal(p2stars(
    0.0001,
    superscript = FALSE,
    add_trailing_space = TRUE
  ),
  "\\*\\*\\*\u2009")
  expect_equal(p2stars(0.01), "\\*\\*")
  expect_equal(p2stars(0.05), "\\*")
  expect_equal(p2stars(0.10), "")
  expect_equal(p2stars(c(0.10, .02), superscript = TRUE), c("", "^\\*^"))
  expect_equal(p2stars(
    c(0.10, .02),
    superscript = TRUE,
    add_trailing_space = TRUE
  ),
  c("", "^\\*\u2009^"))
  expect_equal(p2stars(
    c(0.09, .02),
    first_alpha_marginal = TRUE,
    alpha = c(.10, .05, .01, .001)
  ),
  c("\u2020", "\\*"))
})
vnf <- apa7:::variable_name_formatter

test_that("variable name formatter", {
  expect_equal(vnf("a^2^"), "A^2^")
  expect_equal(vnf("cut [linear]"), "Cut")
  expect_equal(vnf("cut [quadratic]"), "Cut^2^")
  expect_equal(vnf("cut [cubic]"), "Cut^3^")
  expect_equal(vnf("cut [4th degree]"), "Cut^4^")
})
