test_that("countryLevelData rule works", {
  expect_error(countryLevelData(monthFrom_month= "1",
                               monthFrom_year=2000,
                               monthTo_month= "5",
                               monthTo_year=2024,
                               to_dataframe =TRUE),
               message("Please provide either the countryName or the admin0Pcode"))
  expect_error(countryLevelData(admin0Pcode= "AFG",
                               countryName= "Afghanistan",
                               monthFrom_month= "1",
                               monthFrom_year=2000,
                               monthTo_month= "5",
                               monthTo_year=2024,
                               to_dataframe =TRUE),
               message("Please provide either the countryName or the admin0Pcode, not both"))
  expect_error(countryLevelData(countryName= "Afghanistan",
                               monthFrom_month= 1,
                               monthFrom_year=2000,
                               monthTo_month= "5",
                               monthTo_year=2024,
                               to_dataframe =TRUE),
               message("Please provide the start month of the reporting period (str)"))
})



test_that("admin1LevelData rule works", {
  expect_error(admin1LevelData(monthFrom_month= "1",
                                monthFrom_year=2000,
                                monthTo_month= "5",
                                monthTo_year=2024,
                                to_dataframe =TRUE),
               message("Please provide either the countryName or the admin0Pcode"))
  expect_error(admin1LevelData(admin0Pcode= "AFG",
                                countryName= "Afghanistan",
                                monthFrom_month= "1",
                                monthFrom_year=2000,
                                monthTo_month= "5",
                                monthTo_year=2024,
                                to_dataframe =TRUE),
               message("Please provide either the countryName or the admin0Pcode, not both"))
  expect_error(admin1LevelData(countryName= "Afghanistan",
                                monthFrom_month= 1,
                                monthFrom_year=2000,
                                monthTo_month= "5",
                                monthTo_year=2024,
                                to_dataframe =TRUE),
               message("Please provide the start month of the reporting period (str)"))
})



test_that("admin2LevelData rule works", {
  expect_error(admin2LevelData(monthFrom_month= "1",
                                monthFrom_year=2000,
                                monthTo_month= "5",
                                monthTo_year=2024,
                                to_dataframe =TRUE),
               message("Please provide either the countryName or the admin0Pcode"))
  expect_error(admin2LevelData(admin0Pcode= "AFG",
                                countryName= "Afghanistan",
                                monthFrom_month= "1",
                                monthFrom_year=2000,
                                monthTo_month= "5",
                                monthTo_year=2024,
                                to_dataframe =TRUE),
               message("Please provide either the countryName or the admin0Pcode, not both"))
  expect_error(admin2LevelData(countryName= "Afghanistan",
                                monthFrom_month= 1,
                                monthFrom_year=2000,
                                monthTo_month= "5",
                                monthTo_year=2024,
                                to_dataframe =TRUE),
               message("Please provide the start month of the reporting period (str)"))
})
