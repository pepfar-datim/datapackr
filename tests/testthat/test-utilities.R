context("test-utilities")

# swapColumns ----
test_that("Testing swapping of columns...", {
  
  # create input data frames
  df1 <- 
    data.frame(
      w = c(0,9,8),
      x = c(1,2,3),
      y = c(4,5,6)
    )
  
  df2 <- 
    data.frame(
      x = c(9,9,9),
      y = c(1,1,1),
      z = c(6,6,6)
    )
  
  # create output data frame
  df_output <- 
    data.frame(
      w = c(0,9,8),
      x = c(9,9,9),
      y = c(1,1,1)
    )
  
  # perform test when not tibble
  res <- swapColumns(df1, df2 )
  expect_equal(res, df_output)
  rm(res)
  
  # perform test when  tibble
  res <- swapColumns(df1 %>% tibble(), df2 %>% tibble())
  expect_equal(res, df_output %>% tibble())
  
  
})
