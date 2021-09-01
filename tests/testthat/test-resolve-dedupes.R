context("dedupe")



test_that("No modifications on if there are no duplicates", {
  foo<-list(data=list(), info=list())
  foo$info$warning_msg<-Messages$new()
  
  foo$data$distributedMER<-tibble::tribble(
    ~PSNU,~psnuid,~sheet_name,~indicator_code,~Age,~Sex,~KeyPop,~mechanism_code,~support_type,~value,
    "Foo","ab123","test_sheet","TX_CURR","<1","Male",NA,"12345","DSD",100,
    "Bar","ab123","test_sheet","TX_CURR","<1","Male",NA,"54321","DSD",200
  ) 
  
  foo$data$SNUxIM<-tibble::tribble(
    ~PSNU,~psnuid,~indicator_code,~Age,~Sex,~KeyPop,~support_type, ~mechanism_code,~distribution,
    "Foo","abc123","TX_CURR","<1","Male",NA,"DSD","12345",1,
    "Bar","abc123","TX_CURR","<1","Male",NA,"DSD","54321",1
  )
  
  foo<-datapackr:::autoResolveDuplicates(foo,keep_dedup = FALSE)
  
  expect_true(NROW(foo$datim$MER)==2)
  
  expect_true(sum(grepl("00000",foo$datim$MER$mechanism_code)) == 0)
  
  expect_equal(NROW(foo$info$warning_msg$msg_frame),0L)
} )



test_that("Can resolve non-overallocated  pure dupes", {
  foo<-list(data=list(), info=list())
  foo$info$warning_msg<-Messages$new()
  
  
  foo$data$distributedMER<-tibble::tribble(
    ~PSNU,~psnuid,~sheet_name,~indicator_code,~Age,~Sex,~KeyPop,~mechanism_code,~support_type,~value,
    "Foo","ab123","test_sheet","TX_CURR","<1","Male",NA,"12345","DSD",100,
    "Foo","ab123","test_sheet","TX_CURR","<1","Male",NA,"54321","DSD",200
  ) 
  
  foo$data$SNUxIM<-tibble::tribble(
    ~PSNU,~psnuid,~indicator_code,~Age,~Sex,~KeyPop,~support_type, ~mechanism_code,~distribution,
    "Foo","abc123","TX_CURR","<1","Male",NA,"DSD","12345",0.5,
    "Foo","abc123","TX_CURR","<1","Male",NA,"DSD","54321",0.5
  )

  foo<-datapackr:::autoResolveDuplicates(foo,keep_dedup = FALSE)
  
  expect_true(NROW(foo$datim$MER)==3)
  
  expect_true(sum(grepl("00000",foo$datim$MER$mechanism_code)) == 1)
  
  expect_true(grepl("1 zero-valued pure deduplication adjustments",foo$info$warning_msg$msg_frame$message))
} )



test_that("Provide info only for  over-allocated pure dupes", {
  foo<-list(data=list(), info=list())
  foo$info$warning_msg<-Messages$new()
  
  
  foo$data$distributedMER<-tibble::tribble(
    ~PSNU,~psnuid,~sheet_name,~indicator_code,~Age,~Sex,~KeyPop,~mechanism_code,~support_type,~value,
    "Foo","ab123","test_sheet","TX_CURR","<1","Male",NA,"12345","DSD",100,
    "Foo","ab123","test_sheet","TX_CURR","<1","Male",NA,"54321","DSD",200
  ) 
  
  foo$data$SNUxIM<-tibble::tribble(
    ~PSNU,~psnuid,~indicator_code,~Age,~Sex,~KeyPop,~support_type, ~mechanism_code,~distribution,
    "Foo","abc123","TX_CURR","<1","Male",NA,"DSD","12345",1,
    "Foo","abc123","TX_CURR","<1","Male",NA,"DSD","54321",1
  )
  
  foo<-datapackr:::autoResolveDuplicates(foo,keep_dedup = FALSE)
  
  expect_true(NROW(foo$datim$MER)==2)
  
  expect_true(sum(grepl("00000",foo$datim$MER$mechanism_code)) == 0)
  
  expect_true(grepl("duplicates with allocation greater than 100% were identified",foo$info$warning_msg$msg_frame$message))
} )


test_that("Can resolve non-overallocated crosswalk dupes", {
  foo<-list(data=list(), info=list())
  foo$info$warning_msg<-Messages$new()
  
  
  foo$data$distributedMER<-tibble::tribble(
    ~PSNU,~psnuid,~sheet_name,~indicator_code,~Age,~Sex,~KeyPop,~mechanism_code,~support_type,~value,
    "Foo","ab123","test_sheet","TX_CURR","<1","Male",NA,"12345","DSD",100,
    "Foo","ab123","test_sheet","TX_CURR","<1","Male",NA,"54321","TA",200
  ) 
  
  foo$data$SNUxIM<-tibble::tribble(
    ~PSNU,~psnuid,~indicator_code,~Age,~Sex,~KeyPop,~support_type, ~mechanism_code,~distribution,
    "Foo","abc123","TX_CURR","<1","Male",NA,"DSD","12345",0.5,
    "Foo","abc123","TX_CURR","<1","Male",NA,"TA","54321",0.5
  )
  
  foo<-datapackr:::autoResolveDuplicates(foo,keep_dedup = FALSE)
  
  expect_true(NROW(foo$datim$MER)==3)
  
  expect_true(sum(grepl("00001",foo$datim$MER$mechanism_code)) == 1)
  
  expect_true(grepl("1 zero-valued crosswalk deduplication adjustments will be added to your DATIM import",foo$info$warning_msg$msg_frame$message))
} )


test_that("Provide info only for over-allocated crosswalk dupes", {
  foo<-list(data=list(), info=list())
  foo$info$warning_msg<-Messages$new()
  
  
  foo$data$distributedMER<-tibble::tribble(
    ~PSNU,~psnuid,~sheet_name,~indicator_code,~Age,~Sex,~KeyPop,~mechanism_code,~support_type,~value,
    "Foo","ab123","test_sheet","TX_CURR","<1","Male",NA,"A","DSD",100,
    "Foo","ab123","test_sheet","TX_CURR","<1","Male",NA,"B","TA",200,
    "Foo","ab123","test_sheet","TX_CURR","<1","Male",NA,"C","DSD",300,
    "Foo","ab123","test_sheet","TX_CURR","<1","Male",NA,"D","TA",50,
    "Foo","ab123","test_sheet","TX_CURR","<1","Male",NA,"E","DSD",10
  ) 
  
  foo$data$SNUxIM<-tibble::tribble(
    ~PSNU,~psnuid,~indicator_code,~Age,~Sex,~KeyPop,~support_type, ~mechanism_code,~distribution,
    "Foo","abc123","TX_CURR","<1","Male",NA,"DSD","A",0.11,
    "Foo","abc123","TX_CURR","<1","Male",NA,"TA","B",0.62,
    "Foo","abc123","TX_CURR","<1","Male",NA,"DSD","C",0.25,
    "Foo","abc123","TX_CURR","<1","Male",NA,"TA","D",0.02,
    "Foo","abc123","TX_CURR","<1","Male",NA,"DSD","E",0.02
    )
  
  foo<-datapackr:::autoResolveDuplicates(foo,keep_dedup = FALSE)
  
  expect_true(NROW(foo$datim$MER)==5)
  
  expect_true(sum(grepl("00001",foo$datim$MER$mechanism_code)) == 0)
  
  expect_true(grepl("crosswalk duplicates with allocation greater than 100% were identified",foo$info$warning_msg$msg_frame$message))
} )



test_that("Preserve non-deduplicated data when having over-allocated crosswalk dupes", {
  foo<-list(data=list(), info=list())
  foo$info$warning_msg<-Messages$new()
  
  
  foo$data$distributedMER<-tibble::tribble(
    ~PSNU,~psnuid,~sheet_name,~indicator_code,~Age,~Sex,~KeyPop,~mechanism_code,~support_type,~value,
    "Foo","ab123","test_sheet","TX_CURR","<1","Male",NA,"A","DSD",100,
    "Bar","ab123","test_sheet","TX_CURR","<1","Male",NA,"C","DSD",300,
    "Bar","ab123","test_sheet","TX_CURR","<1","Male",NA,"D","TA",50
  ) 
  
  foo$data$SNUxIM<-tibble::tribble(
    ~PSNU,~psnuid,~indicator_code,~Age,~Sex,~KeyPop,~support_type, ~mechanism_code,~distribution,
    "Foo","abc123","TX_CURR","<1","Male",NA,"DSD","A",0.5,
    "Bar","abc123","TX_CURR","<1","Male",NA,"DSD","C",0.5,
    "Bar","abc123","TX_CURR","<1","Male",NA,"TA","D",0.6
  )
  
  foo<-datapackr:::autoResolveDuplicates(foo,keep_dedup = FALSE)
  
  expect_true(NROW(foo$datim$MER)==3)
  
  expect_true(sum(grepl("00001",foo$datim$MER$mechanism_code)) == 0)
  
  expect_true(grepl("crosswalk duplicates with allocation greater than 100% were identified",foo$info$warning_msg$msg_frame$message))
} )
