#' Organic carbon estimation
#'
#' @description Model linear relation between organic matter and organic carbon and estimate organic carbon values from organic matter data
#'
#' @param df A [data.frame] with, at least, columns CoreID, Ecosystem, Specie, SiteID, OM, and OC.
#' @param SiteID the name of the column (between "") from the df with the sampling site identification for each sample
#' @param CoreID the name of the column (between "") from the df with the Core identification for each sample
#' @param Ecosystem the name of the column (between "") from the df with the Ecosystem type for each sample
#' @param Specie the name of the column (between "") from the df with the predominant specie for each sample
#' @param OM the name of the column (between "") from the df with the percentage of organic matter of that sample
#' @param OC the name of the column (between "") from the df with the percentage of organic carbon in the sample
#'
#'
#' @return the initial [data.frame] + one column with organic carbon values (fOC = final organic carbon)
#' @export
#'
#' @examples

transform_om_oc <- function(df = NULL, SiteID="SiteID", CoreID="CoreID", Ecosystem="Ecosystem", Specie="Specie", OM="OM", OC="OC") {

  #### Estimate df linear model to predict OC from OM for each ecosystem, specie and station ###
  #skip those models with R2<0.5 or P value>0.05

  # check if the class of the parameters objects, the names and class of the columns of df are correct

  # class of the dataframe
  if (is.data.frame(df)==FALSE) {stop("The data provided is not class data.frame, please check data and transforme")}

  df2<-as.data.frame(cbind(df[[SiteID]], df[[CoreID]], df[[Ecosystem]],df[[Specie]],df[[OM]], df[[OC]]))
  colnames(df2)<-c("SiteID", "CoreID", "Ecosystem","Specie","OM","OC")
  df2[, 5:6] <- sapply(df2[, 5:6], as.numeric)

  #create df list of dataframes with data from each ecosystem, specie, and station (site)
  X<-split(df2, df2$Ecosystem)
  X2<-split(df2, df2$Specie)
  X3<-split(df2, df2$SiteID)
  X<-c(X,X2,X3)
  length(X)

  #create empty table to log model data
  OCEst <- data.frame(ID=character(),
                      R2=numeric(),
                      P=numeric(),
                      f=numeric(),
                      int=numeric(),
                      slope=numeric()
  )

  for(i in 1:length(X)) {
    OCEst[i,1]<-names(X[i])
    Data<-as.data.frame(X[i])
    colnames(Data)<-colnames(df2)


    #we only model those ecosystem, specie, and station with more than 5 samples were OC and LOI were mwasured
    if((nrow(Data |> dplyr::filter_at(dplyr::vars(OM,OC),dplyr::all_vars(!is.na(.)))))<5) next


    else{

      model<-lm(OC ~ OM, data=Data)

      if(summary(model)$r.squared<0.5 | broom::glance(model)$p.value>0.05 ) next

      else{

        OCEst[i,2]<-summary(model)$r.squared
        OCEst[i,3]<-broom::glance(model)$p.value
        OCEst[i,4]<-summary(model)$fstatistic[1]
        OCEst[i,5]<-model$coefficients[1]
        OCEst[i,6]<-model$coefficients[2]

      }}
  }

  rownames(OCEst)<-OCEst$ID

  # write.csv(OCEst,file.path(Folder,"OM-OC_lm.csv"),sep=";", dec=",")

  ## Use the models estimated to predict OC from OM content for those samples with no OC data
  #If there is OC data for that sample, keep original OC data,
  #else use function estimated for that SiteID
  #else function for specie
  #else function for ecosystem

  df2$fOC <- NA

  for(i in 1:nrow(df2)) {

    if (is.na(df2[i,which( colnames(df2)=="OC" )])==FALSE)
    {df2[i,which( colnames(df2)=="fOC" )]<-df2[i,which( colnames(df2)=="OC" )]}

    else { if (is.na(OCEst[which(rownames(OCEst)==(df2[i,which( colnames(df2)=="SiteID" )])),which(colnames(OCEst)=="int")])==FALSE)

    {df2[i,which( colnames(df2)=="fOC" )]<-
      OCEst[which(rownames(OCEst)==(df2[i,which( colnames(df2)=="SiteID" )])),which(colnames(OCEst)=="int" )]+
      (OCEst[which(rownames(OCEst)==(df2[i,which( colnames(df2)=="SiteID" )])),which(colnames(OCEst)=="slope" )])*
      df2[i,which( colnames(df2)=="OM" )] }

      else{ if (is.na(OCEst[which(rownames(OCEst)==(df2[i,which( colnames(df2)=="Specie" )])),which(colnames(OCEst)=="int")])==FALSE)

      {df2[i,which( colnames(df2)=="fOC" )]<-
        OCEst[which(rownames(OCEst)==(df2[i,which( colnames(df2)=="Specie" )])),which(colnames(OCEst)=="int" )]+
        (OCEst[which(rownames(OCEst)==(df2[i,which( colnames(df2)=="Specie" )])),which(colnames(OCEst)=="slope" )])*
        df2[i,which( colnames(df2)=="OM" )]}

        else {df2[i,which( colnames(df2)=="fOC" )]<-
          OCEst[which(rownames(OCEst)==(df2[i,which( colnames(df2)=="Ecosystem" )])),which(colnames(OCEst)=="int" )]+
          (OCEst[which(rownames(OCEst)==(df2[i,which( colnames(df2)=="Ecosystem" )])),which(colnames(OCEst)=="slope" )])*
          df2[i,which( colnames(df2)=="OM" )]}}}

  }

  ## when OM very low, the estimation can give negative values of OC. We change negative values for 0.

  df2$fOC[df2$fOC < 0] <- 0


  if (sum(is.na(df2$fOC) & !is.na(df2$OM))>=1) {
  message(
    paste("Howard et al (2014) applied to",
          sum(is.na(df2$fOC) & !is.na(df2$OM) ), "observations")
  )}

  df2 <- df2 |>
    dplyr::mutate(
      fOC = dplyr::case_when(
        is.na(fOC) & OM <= 0.2 & Ecosystem == "Seagrass" ~
          0.4 * OM - 0.21,
        is.na(fOC) & OM > 0.2 & Ecosystem == "Seagrass" ~
          0.43 * OM - 0.33,
        is.na(fOC) & Ecosystem == "Salt Marsh" ~
          0.47 * OM + 0.0008 * OM^2,
        is.na(fOC) & Ecosystem == "Mangrove" ~
          0.415 * OM - 2.89,
        T ~ fOC
      )
    )

  df3<-cbind(df,df2$fOC)
  colnames(df3)<-c(colnames(df),"fOC")

  return(df3)
  #return(OCEst)

}
