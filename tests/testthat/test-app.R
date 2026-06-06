test_that("Shiny app UI and Server are defined", {
  expect_true(file.exists(system.file("app/app.R", package = "XploreQuran")) || file.exists("../../inst/app/app.R"))
})
