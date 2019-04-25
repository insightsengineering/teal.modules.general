context("g_expert_cl")

test_that("g_expert_cl can work with all combinations",{


      expect_equal(
          g_expert_cl(NULL, NULL, NULL, FALSE),
          NULL
          )

      expect_equal(
          g_expert_cl("ASL", NULL, NULL, FALSE) %>% deparse,
          "aes(colour = ASL)"
          )

      expect_equal(
          g_expert_cl("ASL", "ASL", NULL, FALSE) %>% deparse,
          "aes(colour = ASL, fill = ASL)"
          )
      expect_equal(
          g_expert_cl("ASL", "ASL", "ASL", FALSE) %>% deparse,
          "aes(colour = ASL, fill = ASL)"
          )
      expect_equal(
          g_expert_cl("ASL", "ASL", NULL, TRUE) %>% deparse,
          "aes(colour = ASL, fill = ASL)"
          )

      expect_equal(
          g_expert_cl("ASL", "ASL", "ASL", TRUE) %>% deparse,
          "aes(colour = ASL, fill = ASL, size = ASL)"
          )
      expect_equal(
          g_expert_cl("ASL", NULL, "ASL", TRUE) %>% deparse,
          "aes(colour = ASL, size = ASL)"
          )



})
test_that("g_expert_cl can work with all combinations without colour_var",{


      expect_equal(
          g_expert_cl(NULL, NULL, NULL, FALSE),
          NULL
          )

      expect_equal(
          g_expert_cl(NULL, "ASL", NULL, FALSE) %>% deparse,
          "aes(fill = ASL)"
          )
      expect_equal(
          g_expert_cl(NULL, "ASL", "ASL", FALSE) %>% deparse,
          "aes(fill = ASL)"
          )
      expect_equal(
          g_expert_cl(NULL, "ASL", NULL, TRUE) %>% deparse,
          "aes(fill = ASL)"
          )
      expect_equal(
          g_expert_cl(NULL, "ASL", "ASL", TRUE) %>% deparse,
          "aes(fill = ASL, size = ASL)"
          )

})

test_that("g_expert_cl can work size_var",{


      expect_equal(
          g_expert_cl(NULL, NULL, "ASL", TRUE) %>% deparse,
          "aes(size = ASL)"
          )
      expect_equal(
          g_expert_cl(NULL, NULL, NULL, TRUE),
          NULL
          )
      expect_equal(
          g_expert_cl(NULL, NULL, "ASL", FALSE),
          NULL
          )

})
