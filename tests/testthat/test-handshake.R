context("test-handshake")


test_that("Can handshake template", {
  d <- datapackr::handshakeFile(path = test_sheet("COP20_Data_Pack_Template_vFINAL.xlsx"),
                     tool = "Data Pack Template")
  expect_true(file.exists(d))
})

test_that("Can error on bad type", {

  expect_error(datapackr::handshakeFile(test_sheet("COP20_Data_Pack_Template_vFINAL.xlsx"),
                   "Foo Template"),
                   "Please specify correct file type: Data Pack, Data Pack Template, OPU Data Pack Template.")

})

test_that("Can error on bad file location", {

  expect_error(datapackr::handshakeFile("/home/littlebobbytables/DataPack.xlsx",
                                         tool = "Data Pack"),
                                         "File could not be read!")

})

test_that("Can error on bad file exstention", {

  foo_file <- tempfile(fileext = ".xlsb")
  file.create(foo_file)
  expect_error(datapackr::handshakeFile(foo_file,
                                         tool = "Data Pack"),
               "File is not the correct format! File must have extension .xlsx")
  unlink(foo_file)
})
