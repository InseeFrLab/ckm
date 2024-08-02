testthat::test_that("matrice de transition", {
  testthat::expect_equal(creer_matrice_transition(D = 5, V = 2)@tMatrix, expected = ptable::create_cnt_ptable(D = 5, V = 2)@tMatrix)
})
