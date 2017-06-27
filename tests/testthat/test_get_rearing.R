library(animalr)
library(RODBC)
library(stringi)
conn <- odbcConnect("frogstar-vortex-animal-sa")
id_df <- data.frame(id = c("  6200", "  6209", "xyz"))

rearing_df <- get_rearing(id_df, conn)

context("get_rearing")

test_that("get_rearing returns correct values", {
  expect_equal(rearing_df$rearing_code[rearing_df$id == "  6200"], "3")
  expect_equal(rearing_df$rearing_code[rearing_df$id == "  6209"], "2")
  expect_equal(length(rearing_df$rearing_code[rearing_df$id == "   xyz"]), 0)
})
odbcClose(conn)
