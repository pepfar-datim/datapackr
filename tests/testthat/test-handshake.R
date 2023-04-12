context("test-handshake")


test_that("Can handshake  COP23 DataPack Template", {
  d <- handshakeFile(path = getTemplate("COP23_Data_Pack_Template.xlsx"),
                     tool = "Data Pack Template")
  expect_true(file.exists(d))
})


test_that("Can handshake  COP23 PSNUxIM Template", {
  d <- handshakeFile(path = getTemplate("COP23_PSNUxIM_Template.xlsx"),
                     tool = "PSNUxIM Template")
  expect_true(file.exists(d))
})

test_that("Can error on bad type", {

  expect_error(handshakeFile(getTemplate("COP23_Data_Pack_Template.xlsx"), "Foo Template"))
})

test_that("Can error on bad file location", {

  #This will bring up a file dialog if run directly with devtools::test()
  skip_if(interactive())
  expect_error(handshakeFile("/home/littlebobbytables/DataPack.xlsx",
                                         tool = "Data Pack"),
                                         "File could not be read!")
})

test_that("canReadFalse if path is NULL", {
  expect_false(canReadFile())
})


test_that("Can error on bad file exstension", {

  foo_file <- tempfile(fileext = ".xlsb")
  file.create(foo_file)
  expect_error(handshakeFile(foo_file,
                                         tool = "Data Pack"),
               "File is not the correct format! File must have extension .xlsx")
  unlink(foo_file)
})
