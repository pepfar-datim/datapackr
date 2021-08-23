context("Package setup")

test_that("We can pick a schema", {

  test_schema<-pick_schema(2020,"OPU Data Pack") 
  testthat::expect_identical(test_schema,datapackr::cop20OPU_data_pack_schema) 
  
  test_schema<-pick_schema(2021,"OPU Data Pack") 
  testthat::expect_identical(test_schema,datapackr::cop21OPU_data_pack_schema) 
  
  
  expect_error(pick_schema(1999,"OPU Data Pack"))
 
  test_schema<-pick_schema(2020,"Data Pack") 
  testthat::expect_identical(test_schema,  datapackr::cop20_data_pack_schema)
  test_schema<-pick_schema(2021,"Data Pack") 
  testthat::expect_identical(test_schema,  datapackr::cop21_data_pack_schema)
  
  #Are we sure we want to return this object for anything other than 2020 
  # and 2021?
  test_schema<-pick_schema(1999,"Data Pack") 
  testthat::expect_identical(test_schema,  datapackr::data_pack_schema)
  
  
  #Thrown an error for garbage inputs
  expect_error(pick_schema(1999,"Foo Pack"))
  expect_error(pick_schema(NA,NA))
  
  }
)

test_that("We can pick template file", {
  
  test_template<-pick_template_path(2020,"OPU Data
                                    Pack")
  expect_true(grepl("COP20_OPU_Data_Pack_Template.xlsx",
                    test_template))
  test_template<-pick_template_path(2021,"OPU Data
                                    Pack")
  expect_true(grepl("COP21_OPU_Data_Pack_Template.xlsx",
                    test_template))
  
  
  #Thrown an error for garbage inputs
  expect_error(pick_template_path(1999,"Foo Pack"))
  expect_error(pick_template_path(NA,NA))
  
}
)