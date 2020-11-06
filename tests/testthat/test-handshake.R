context("test-handshake")


test_that("Can handshake template", {
  d <- handshakeFile(path = test_sheet('COP20_Data_Pack_Template_vFINAL.xlsx'),
                     tool = 'Data Pack Template')
  expect_true(file.exists(d))
} )

test_that("Can error on bad type", {

  expect_error(handshakeFile(test_sheet('COP20_Data_Pack_Template_vFINAL.xlsx'),
                   'Foo Template'),'Please specify correct file type: Data Pack, Data Pack Template, OPU Data Pack Template.')

} )
