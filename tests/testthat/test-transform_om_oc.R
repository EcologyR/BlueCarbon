

test_that("POC column has been generated", {

  df<-data.frame(Core.ID="A", Ecosystem="Seagrass", Specie="Posidonia", Site.ID="Cabrera", OM=c(4,6,2,7,3,6), OC=c(NA,5,3,5,9,3))


  expect_equal("POC" %in% colnames(transform_om_oc(df))==TRUE, TRUE)
})
