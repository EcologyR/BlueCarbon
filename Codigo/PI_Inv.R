# Iberian Peninsula Blue Carbon inventory #



#Libraries#

library(ggplot2)
library(gridExtra)
library(dplyr)
library(reshape2)
library(ggpubr)
library(broom)
library(corrplot)
library(mapproj)
library(grid)
library(rbacon)
library(tidyr)

library(tidyverse)
library(here)
library(broom)
here()

A <- read.csv(here("Codigo", "example.csv"), sep = ";")
# Summary initial data

# Organic matter to organic carbon -------------------------------------------------------------------

#' Organic carbon estimation
#'
#' @description Model linear relation between organic matter and organic carbon and estimate organic carbon values from organic matter data
#'
#' @param df A data frame with columns Core.ID, Ecosystem, Genus, Site.ID, OM, and OC.
#'
#' @return the initial data frame + one column with organic carbon values
#' @export
#'
#' @examples

transform_om_oc <- function(df = NULL) {

  #### Estimate df linear model to predict OC from OM for each ecosystem, specie and station ###
  #skip those models with R2<0.5 or P value>0.05

  #create df list of dataframes with data from each ecosystem, specie, and station (site)
  table(df$Ecosystem)
  X<-split(df, df$Ecosystem)
  X2<-split(df, df$Genus)
  X3<-split(df, df$Site.ID)
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
    colnames(Data)<-colnames(df)


    #we only model those ecosystem, genus, and station with more than 5 samples were OC and LOI were mwasured
    if((nrow(Data %>% filter_at(vars(OM,OC),all_vars(!is.na(.)))))<5) next


    else{

      model<-lm(OC ~ OM, data=Data)

      if(summary(model)$r.squared<0.5 | glance(model)$p.value>0.05 ) next

      else{

        OCEst[i,2]<-summary(model)$r.squared
        OCEst[i,3]<-glance(model)$p.value
        OCEst[i,4]<-summary(model)$fstatistic[1]
        OCEst[i,5]<-model$coefficients[1]
        OCEst[i,6]<-model$coefficients[2]

      }}
  }

  rownames(OCEst)<-OCEst$ID

  # write.csv(OCEst,file.path(Folder,"OM-OC_lm.csv"),sep=";", dec=",")

  ## Use the models estimated to predict OC from OM content for those samples with no OC data
  #If there is OC data for that sample, keep original OC data,
  #else use function estimated for that Site.ID
  #else function for specie
  #else function for ecosystem

  df$POC <- NA

  for(i in 1:nrow(df)) {

    if (is.na(df[i,which( colnames(df)=="OC" )])==FALSE)
    {df[i,which( colnames(df)=="POC" )]<-df[i,which( colnames(df)=="OC" )]}

    else { if (is.na(OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="Site.ID" )])),which(colnames(OCEst)=="int")])==FALSE)

    {df[i,which( colnames(df)=="POC" )]<-
      OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="Site.ID" )])),which(colnames(OCEst)=="int" )]+
      (OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="Site.ID" )])),which(colnames(OCEst)=="slope" )])*
      df[i,which( colnames(df)=="OM" )] }

      else{ if (is.na(OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="Genus" )])),which(colnames(OCEst)=="int")])==FALSE)

      {df[i,which( colnames(df)=="POC" )]<-
        OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="Genus" )])),which(colnames(OCEst)=="int" )]+
        (OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="Genus" )])),which(colnames(OCEst)=="slope" )])*
        df[i,which( colnames(df)=="OM" )]}

        else {df[i,which( colnames(df)=="POC" )]<-
          OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="Ecosystem" )])),which(colnames(OCEst)=="int" )]+
          (OCEst[which(rownames(OCEst)==(df[i,which( colnames(df)=="Ecosystem" )])),which(colnames(OCEst)=="slope" )])*
          df[i,which( colnames(df)=="OM" )]}}}

  }

  ## when OM very low, the estimation can give negative values of OC. We change negative values for 0.

  df$POC[df$POC < 0] <- 0

  message(
    paste("Howard et al (2014) applied to",
          sum(is.na(df$POC)), "observations")
    )

  df <- df |>
    mutate(
      POC = case_when(
        is.na(POC) & OM <= 0.2 & Ecosystem == "Seagrass" ~
          0.4 * OM - 0.21,
        is.na(POC) & OM > 0.2 & Ecosystem == "Seagrass" ~
          0.43 * OM - 0.33,
        is.na(POC) & Ecosystem == "Salt Marsh" ~
          0.47 * OM + 0.0008 * OM^2,
        T ~ POC
      )
    )

  return(df)

}

df_OC <- transform_om_oc(df = A)



# Soil C stocks estimation ------------------------------------------------


#####################
###estimate stocks for the whole core + 1m with extrapolation of OC accumulated mass if needed
#####


#' Organic Carbon Stock estimation
#'
#' @description Estimates the carbon stock from soil core data until a specific depth, 100 cm by default. If the core do not reach the
#' standardization depth it extrapolate the stock from a linear model between the organic carbon accumulated mass and depth.
#'
#' @param df A data frame with columns: Core.ID, Min.D (minimum depth of the sample), Max.D (maximum depth of the sample), DBD (dry bulk density), POC (organic carbon %)
#' @param Depth standardization soil depth, by default 100 cm.
#'
#' @return data frame with columns Core.id, S.WC (organic carbon stock at the whole core), D.Max (maximum depth of the core), and Stock (organic carbon stock at the standardized depth)
#' @export
#'
#' @examples

estimate_stock <- function(df = NULL, Depth = 100) {

df <-df %>% mutate (Center = Min.D+((Max.D-Min.D)/2))

df = filter(df, !is.na(POC))

X<-split(df, df$Core.ID)

BCS <- data.frame(Core.ID=character(),
                  S.WC=numeric(),
                  D.Max=numeric(),
                  Stock=numeric())

for(i in 1:length(X)) {
  BCS[i,1]<-names(X[i])
  Data<-as.data.frame(X[i])
  colnames(Data)<-colnames(df)



  if(nrow(Data)<3) next

  else{

    Data$h<-NA

    #estimation of the thickness of the sample (h)
    for (j in 2:(nrow(Data)-1)) {

      Data[j,which(colnames(Data)=="h")]<-
        ((Data[j,which(colnames(Data)=="Center")]-Data[(j-1),which(colnames(Data)=="Center")])/2)+
        ((Data[(j+1),which(colnames(Data)=="Center")]-Data[j,which(colnames(Data)=="Center")])/2)

    }

    Data[1,which(colnames(Data)=="h")]<-Data[1,which(colnames(Data)=="Center")]
    Data[nrow(Data),which(colnames(Data)=="h")]<-
      Data[nrow(Data),which(colnames(Data)=="Max.D")]-Data[nrow(Data),which(colnames(Data)=="Center")]

    #estimation of carbon g cm2 per sample, OCgcm2= carbon density (g cm3) by thickness (h)
    Data <-Data %>% mutate (OCgcm2 = DBD*(POC/100)*h)

    #estimation of the OC stoack in the whole core
    BCS[i,2]<-sum(Data[,which(colnames(Data)=="OCgcm2")])
    BCS[i,3]<-max(Data[,which(colnames(Data)=="Max.D")])

    #if core exactly the standarization depth, we keep the stock of the whole core
    if(max(Data$Max.D)==Depth) {BCS[i,4]<-sum(Data[,which(colnames(Data)=="OCgcm2")])}


    else{

      # if the core longer than the standarization depth we estimate the stock until 1m depth
      if (max(Data$Max.D)>=Depth)

      {
        Data<-Data[c(1:(length(which(Data$Max.D <=Depth))+1)),]

        BCS[i,4]<-(((sum(Data[c(1:(nrow(Data)-1)),which(colnames(Data)=="OCgcm2")]))+
                      (Data[nrow(Data),which(colnames(Data)=="OCgcm2")]/((max(Data$Max.D)-Data[(nrow(Data)-1),which(colnames(Data)=="Max.D")]))
                       *(Depth-Data[(nrow(Data)-1),which(colnames(Data)=="Max.D")]))))}

      #if core shorter than than the standarization depth we model the OC acumulated mass with depth and predict the stock at 1m
      else {

        Data <-Data %>% mutate (OCM = cumsum(OCgcm2))
        model<-lm(OCM ~ Max.D, data=Data)
        BCS[i,4]<-coef(model)[1] + Depth*coef(model)[2]}}
    }}
return(BCS)
}

stocks<-estimate_stock(df_OC,100)


# Test extrapolation errors -----------------------------------------------

## Testing if extrapolations from accumulated OC mass report similar values than the estimation
#of 1m stocks using those cores longer than 1 meter

#' Test differences between observed and extrapolated stocks
#'
#' @description subset those cores that reach the standardization depth and estimates the stock (observed stock), estimate the stock from the linear relation of organic carbon accumulated mass and depth using the 90, 75, 50 and 25% top length of the indicated standardization depth. Compares the observed stock with the estimated stocks by extrapolation.
#'
#' @param df A data frame with columns: Core.ID, Min.D (minimum depth of the sample), Max.D (maximum depth of the sample), DBD (dry bulk density), POC (organic carbon %)
#' @param Depth standardization soil depth, by default 100 cm.
#'
#' @return A data frame with the observed and extrapolated stocks. A plot with comparisons.
#' @export
#'
#' @examples

test_extrapolation <- function(df = NULL, Depth = 100) {

  df <-df %>% mutate (Center = Min.D+((Max.D-Min.D)/2))
  X<-split(df, df$Core.ID)

ExtS<- data.frame(Core.ID=character(),
                  S.1m=numeric(),
                  EXT.90=numeric(),
                  EXT.75=numeric(),
                  EXT.50=numeric(),
                  EXT.25=numeric(),
                  Ecosystem=character(),
                  Genus=character())



for(i in 1:length(X)) {
  ExtS[i,1]<-names(X[i])
  Data<-as.data.frame(X[i])
  colnames(Data)<-colnames(df)
  ExtS[i,7]<-Data[1,which( colnames(df)=="Ecosystem" )]
  ExtS[i,8]<-Data[1,which( colnames(df)=="Genus" )]

  Data = filter(Data, !is.na(POC))

  if(nrow(Data) <3) next


  else { Data$h<-NA

        #estimation of the thickness of the sample (h)
        for (j in 2:(nrow(Data)-1)) {

          Data[j,which(colnames(Data)=="h")]<-
            ((Data[j,which(colnames(Data)=="Center")]-Data[(j-1),which(colnames(Data)=="Center")])/2)+
            ((Data[(j+1),which(colnames(Data)=="Center")]-Data[j,which(colnames(Data)=="Center")])/2)

        }

        #estimation of carbon g cm2 per sample, OCgcm2= carbon density (g cm3) by thickness (h)
        Data[1,which(colnames(Data)=="h")]<-Data[1,which(colnames(Data)=="Center")]
        Data[nrow(Data),which(colnames(Data)=="h")]<-
          Data[nrow(Data),which(colnames(Data)=="Max.D")]-Data[nrow(Data),which(colnames(Data)=="Center")]
        Data <-Data %>% mutate (OCgcm2 = DBD*(POC/100)*h)


        #For those cores longer than the extrapolation depth we estimate stock the observed stock at that depth and from linear models
        # using data from the 90, 75, 50 and 25 % of the extrapolation depth
        if (max(Data$Max.D)>=Depth)

        {
          Data<-Data[c(1:(length(which(Data$Max.D <=Depth))+1)),]

          ExtS[i,2]<-(((sum(Data[c(1:(nrow(Data)-1)),which(colnames(Data)=="OCgcm2")]))+
                        (Data[nrow(Data),which(colnames(Data)=="OCgcm2")]/((max(Data$Max.D)-Data[(nrow(Data)-1),which(colnames(Data)=="Max.D")]))
                         *(Depth-Data[(nrow(Data)-1),which(colnames(Data)=="Max.D")]))))

          Data <-Data %>% mutate (OCM = cumsum(OCgcm2))

          ninety<-Depth*0.9 #90% of the extrapolation length
          seventy<-Depth*0.75 #90% of the extrapolation length
          fifhty<-Depth*0.50 #90% of the extrapolation length
          twentififty<-Depth*0.25 #90% of the extrapolation length

          Data<- subset(Data,Data$Max.D<=ninety)

          if (nrow(Data)>3){
            model90<-lm(OCM ~ Max.D, data=Data)
            ExtS[i,3]<-coef(model90)[1] + 100*coef(model90)[2]}

          Data<- subset(Data,Data$Max.D<=seventy)
          if (nrow(Data)>3){
            model75<-lm(OCM ~ Max.D, data=Data)
            ExtS[i,4]<-coef(model75)[1] + 100*coef(model75)[2]}

          Data<- subset(Data,Data$Max.D<=fifhty)
          if (nrow(Data)>3){
            model50<-lm(OCM ~ Max.D, data=Data)
            ExtS[i,5]<-coef(model50)[1] + 100*coef(model50)[2]}

          Data<- subset(Data,Data$Max.D<=twentififty)
          if (nrow(Data)>3){
            model25<-lm(OCM ~ Max.D, data=Data)
            ExtS[i,6]<-coef(model25)[1] + 100*coef(model25)[2]}

        }}}

#############
# we test the correlation between the stock at 1m estimated from real data and the models
# and the error of the models
##############


CorMat<-cor(na.omit(ExtS[,c(2:6)]), method = "pearson")


corrplot::corrplot(CorMat, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)

#write.csv(CorMat,file.path(Folder,"Corr.Extrapolation.csv"),sep=";", dec=",")


ExtS<-ExtS %>% mutate (Error.90 = (abs(S.1m-EXT.90)*100)/S.1m)
ExtS<-ExtS %>% mutate (Error.75 = (abs(S.1m-EXT.75)*100)/S.1m)
ExtS<-ExtS %>% mutate (Error.50 = (abs(S.1m-EXT.50)*100)/S.1m)
ExtS<-ExtS %>% mutate (Error.25 = (abs(S.1m-EXT.25)*100)/S.1m)

summary(ExtS)


#Global Error
m.ExtS<-ExtS[,c(1,9:12)]
m.ExtS<-reshape::melt(m.ExtS,id=c("Core.ID"))

ggplot2::ggplot(m.ExtS,aes(variable, value))+
  geom_boxplot()+
  geom_jitter()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot2::ggplot(ExtS, aes(ExtS[,2], ExtS[,3]))+xlab("1m stock")+ ylab("Model 1m stock")+
  geom_point(aes(ExtS[,2], ExtS[,3],color="90%"), size=2)+
  geom_point(aes(ExtS[,2], ExtS[,4],color="75%"), size=2)+
  geom_point(aes(ExtS[,2], ExtS[,5],color="50%"), size=2)+
  geom_point(aes(ExtS[,2], ExtS[,6],color="25%"), size=2)+
  theme(text = element_text(size=15))+
  #geom_text_repel(aes(label=A[,1]), size=4)+
  xlim(0,5)+ylim(0,5)+
  geom_abline()

}


test_extrapolation (df = df_OC, Depth = 100)



# C fluxes estimation -----------------------------------------------------


#' Estimate the average organic carbon flux
#'
#' @description estimate the average organic carbon flux to the soil in a indicated time frame (by default last 100 years) from the organic carbon concentration and ages obtained from a age-depth or age-accumulated mass model
#'
#' @param df A data frame with columns: Core.ID, Min.D (minimum depth of the sample), Max.D (maximum depth of the sample), DBD (dry bulk density), POC (organic carbon %), Age (age of the sample obtained from a age-depth or age-accumulated mass model)
#' @param TimeFrame standardization time frame, by default 100 years
#'
#' @return data frame with columns Core.id, F.WC (organic carbon fluxes at the whole core), A.Max (maximum age of the core), and Flux (average organic carbon flux at the indicated time frame)
#' @export
#'
#' @examples


estimate_flux<- function(df=NULL,TimeFrame=100) {


df <-df %>% mutate (Center = Min.D+((Max.D-Min.D)/2))

#select those cores with chronological models
AD = filter(df, !is.na(Age))

length(unique(AD$Core.ID))# number of cores with age estimations

X<-split(AD, AD$Core.ID)

BCF <- data.frame(Core.ID=character(),
                  F.WC=numeric(),
                  A.Max=numeric(),
                  Flux=numeric())


for(i in 1:length(X)) {
  BCF[i,1]<-names(X[i])
  Data<-as.data.frame(X[i])
  colnames(Data)<-colnames(AD)


  Data = filter(Data, !is.na(POC))

  if(nrow(Data)<3) next

  else{

    Data$h<-NA

    #estimation of the thickness of the sample (h)
    for (j in 2:(nrow(Data)-1)) {

      Data[j,which(colnames(Data)=="h")]<-
        ((Data[j,which(colnames(Data)=="Center")]-Data[(j-1),which(colnames(Data)=="Center")])/2)+
        ((Data[(j+1),which(colnames(Data)=="Center")]-Data[j,which(colnames(Data)=="Center")])/2)
    }

    #estimation of carbon g cm2 per sample, OCgcm2= carbon density (g cm3) by thickness (h)
    Data[1,which(colnames(Data)=="h")]<-Data[1,which(colnames(Data)=="Center")]
    Data[nrow(Data),which(colnames(Data)=="h")]<-
    Data[nrow(Data),which(colnames(Data)=="Max.D")]-Data[nrow(Data),which(colnames(Data)=="Center")]

    Data <-Data %>% mutate (OCgcm2 = DBD*(POC/100)*h)

    #estimation of the average carbon flux for the whole core (OC stock/Max Age)
    BCF[i,2]<-(sum(Data[,which(colnames(Data)=="OCgcm2")]))/max(Data$Age)
    BCF[i,3]<-max(Data$Age)

    #estimation of the average carbon flux for the selected TimeFrame (OC stock last 100 yrs/TimeFrame)
    Data<-Data[c(1:(length(which(Data$Age <=TimeFrame))+1)),]

    BCF[i,4]<-((((sum(Data[c(1:(nrow(Data)-1)),which(colnames(Data)=="OCgcm2")]))+
      (Data[nrow(Data),which(colnames(Data)=="OCgcm2")]/((max(Data$Age)-Data[(nrow(Data)-1),which(colnames(Data)=="Age")]))
      *(TimeFrame-Data[(nrow(Data)-1),which(colnames(Data)=="Age")]))))/TimeFrame)

  }}
return(BCF)
}

fluxes<-estimate_flux(df=df_OC,TimeFrame=100)

