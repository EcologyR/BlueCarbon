
test_that("Generate models", {

  df<-data.frame(Core.ID="A", Min.D=c(0,20,40,60,80), Max.D=c(20,40,60,80,100), DBD=1, POC=2)


  expect_equal("POC" %in% colnames(test_extrapolation(df))==TRUE, TRUE)
})
