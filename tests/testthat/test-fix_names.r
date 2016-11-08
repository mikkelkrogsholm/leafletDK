context("Fix names")

# Test 1
test_that("fix_names_encoding is working", {
  expect_equal(leafletDK::fix_names_encoding(c("Ã¦", "Ã¸", "Ã¥", "Ã†", "Ã˜", "Ã…")),
               c("æ", "ø", "å", "Æ", "Ø", "Å"))
  })

# Test 2
test_that("fix_names_join is working", {
  expect_equal(leafletDK::fix_names_join(c("æ", "ø", "å", "Æ", "Ø", "Å", "-", " "),  to_lower = TRUE),
              tolower(c("ae", "oe", "aa", "Ae", "Oe", "Aa", "", "")))
})
