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

  df

}

kk <- transform_om_oc(df = A)


###################################
### Soil C stocks estimation 1m ###
###################################

#Estimation of the sample center

A <-A %>% mutate (Center = DMin.D+((DMax.D-DMin.D)/2))


## Testing if extrapolations from accumulated OC mass report similar values than the estimation
#of 1m stocks using those cores longer than 1 meter


X<-split(A, A$Core.ID)

ExtS<- data.frame(Core.ID=character(),
                  S.1m=numeric(),
                  EXT.90cm=numeric(),
                  EXT.75cm=numeric(),
                  EXT.50cm=numeric(),
                  EXT.25cm=numeric(),
                  Ecosystem=character(),
                  Genus=character())



for(i in 1:length(X)) {
  ExtS[i,1]<-names(X[i])
  Data<-as.data.frame(X[i])
  colnames(Data)<-colnames(A)
  ExtS[i,7]<-Data[1,which( colnames(A)=="Ecosystem" )]
  ExtS[i,8]<-Data[1,which( colnames(A)=="Genus" )]

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
          Data[nrow(Data),which(colnames(Data)=="DMax.D")]-Data[nrow(Data),which(colnames(Data)=="Center")]
        Data <-Data %>% mutate (OCgcm2 = DDBD*(POC/100)*h)


        #For those cores longer than 1m we estimate stock at 1m from real data and from linear models
        # using data from the first 90, 75, 50 and 25 cm of the core
        if (max(Data$DMax.D)>=100)

        {
          Data<-Data[c(1:(length(which(Data$DMax.D <=100))+1)),]

          ExtS[i,2]<-(((sum(Data[c(1:(nrow(Data)-1)),which(colnames(Data)=="OCgcm2")]))+
                        (Data[nrow(Data),which(colnames(Data)=="OCgcm2")]/((max(Data$DMax.D)-Data[(nrow(Data)-1),which(colnames(Data)=="DMax.D")]))
                         *(100-Data[(nrow(Data)-1),which(colnames(Data)=="DMax.D")]))))

          Data <-Data %>% mutate (OCM = cumsum(OCgcm2))

          Data<- subset(Data,Data$DMax.D<=90)

          if (nrow(Data)>3){
            model90<-lm(OCM ~ DMax.D, data=Data)
            ExtS[i,3]<-coef(model90)[1] + 100*coef(model90)[2]}

          Data<- subset(Data,Data$DMax.D<=75)
          if (nrow(Data)>3){
            model75<-lm(OCM ~ DMax.D, data=Data)
            ExtS[i,4]<-coef(model75)[1] + 100*coef(model75)[2]}

          Data<- subset(Data,Data$DMax.D<=50)
          if (nrow(Data)>3){
            model50<-lm(OCM ~ DMax.D, data=Data)
            ExtS[i,5]<-coef(model50)[1] + 100*coef(model50)[2]}

          Data<- subset(Data,Data$DMax.D<=25)
          if (nrow(Data)>3){
            model25<-lm(OCM ~ DMax.D, data=Data)
            ExtS[i,6]<-coef(model25)[1] + 100*coef(model25)[2]}

        }}}

#############
# we test the correlation between the stock at 1m estimated from real data and the models
# and the error of the models
##############


CorMat<-cor(na.omit(ExtS[,c(2:6)]), method = "pearson")

corrplot(CorMat, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)

write.csv(CorMat,file.path(Folder,"Corr.Extrapolation.csv"),sep=";", dec=",")


ggplot(ExtS, aes(ExtS[,2], ExtS[,3]))+xlab("1m stock")+ ylab("Model 1m stock")+
  geom_point(aes(ExtS[,2], ExtS[,3],color="90cm"), size=2)+
  geom_point(aes(ExtS[,2], ExtS[,4],color="75cm"), size=2)+
  geom_point(aes(ExtS[,2], ExtS[,5],color="50cm"), size=2)+
  geom_point(aes(ExtS[,2], ExtS[,6],color="25cm"), size=2)+
  theme(text = element_text(size=15))+
  #geom_text_repel(aes(label=A[,1]), size=4)+
  xlim(0,5)+ylim(0,5)+
  geom_abline()

A<- A %>% mutate (DMin.D = Min.D/(1-(Compresion/100)))

ExtS<-ExtS %>% mutate (Error.90 = (abs(S.1m-EXT.90cm)*100)/S.1m)
ExtS<-ExtS %>% mutate (Error.75 = (abs(S.1m-EXT.75cm)*100)/S.1m)
ExtS<-ExtS %>% mutate (Error.50 = (abs(S.1m-EXT.50cm)*100)/S.1m)
ExtS<-ExtS %>% mutate (Error.25 = (abs(S.1m-EXT.25cm)*100)/S.1m)

summary(ExtS)


#Global Error
m.ExtS<-ExtS[,c(1,9:12)]
m.ExtS<-melt(m.ExtS,id=c("Core.ID"))

ggplot(m.ExtS,aes(variable, value))+
  geom_boxplot()+
  geom_jitter()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


#By ecosystem

ExtSm <- subset(ExtS, ExtS[,"Ecosystem"]=='Salt Marsh')

m.ExtS<-ExtSm[,c(1,9:12)]
m.ExtS<-melt(m.ExtS,id=c("Core.ID"))

ggplot(m.ExtS,aes(variable, value))+
  geom_boxplot()+
  geom_jitter()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Small seagrasses

ExtSSgr <- subset(ExtS, ExtS[,"Genus"]=='Cymodocea nodosa'|ExtS[,"Genus"]=='Zostera marina'|ExtS[,"Genus"]=='Zostera noltii')

m.ExtS<-ExtSSgr[,c(1,9:12)]
m.ExtS<-melt(m.ExtS,id=c("Core.ID"))

ggplot(m.ExtS,aes(variable, value))+
  geom_boxplot()+
  geom_jitter()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Posidonia

ExtP <- subset(ExtS, ExtS[,"Genus"]=='Posidonia oceanica')

m.ExtS<-ExtP[,c(1,9:12)]
m.ExtS<-melt(m.ExtS,id=c("Core.ID"))

ggplot(m.ExtS,aes(variable, value))+
  geom_boxplot()+
  geom_jitter()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


#####################
###estimate stocks for the whole core + 1m with extrapolation of OC accumulated mass if needed
#####

X<-split(A, A$Core.ID)

BCS <- data.frame(Core.ID=character(),
                    S.WC=numeric(),
                    D.Max=numeric(),
                    S.1m=numeric())



for(i in 1:length(X)) {
  BCS[i,1]<-names(X[i])
  Data<-as.data.frame(X[i])
  colnames(Data)<-colnames(A)

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

    Data[1,which(colnames(Data)=="h")]<-Data[1,which(colnames(Data)=="Center")]
    Data[nrow(Data),which(colnames(Data)=="h")]<-
      Data[nrow(Data),which(colnames(Data)=="DMax.D")]-Data[nrow(Data),which(colnames(Data)=="Center")]

    #estimation of carbon g cm2 per sample, OCgcm2= carbon density (g cm3) by thickness (h)
    Data <-Data %>% mutate (OCgcm2 = DDBD*(POC/100)*h)

    #estimation of the OC stoack in the whole core
    BCS[i,2]<-sum(Data[,which(colnames(Data)=="OCgcm2")])
    BCS[i,3]<-max(Data[,which(colnames(Data)=="DMax.D")])

    #if core exactly 1m, we keep the stock of the whole core
    if(max(Data$DMax.D)==100) {BCS[i,4]<-sum(Data[,which(colnames(Data)=="OCgcm2")])}


      else{

        # if the core longer than 1m we estimate the stock until 1m depth
        if (max(Data$DMax.D)>=100)

        {
        Data<-Data[c(1:(length(which(Data$DMax.D <=100))+1)),]

        BCS[i,4]<-(((sum(Data[c(1:(nrow(Data)-1)),which(colnames(Data)=="OCgcm2")]))+
                      (Data[nrow(Data),which(colnames(Data)=="OCgcm2")]/((max(Data$DMax.D)-Data[(nrow(Data)-1),which(colnames(Data)=="DMax.D")]))
                       *(100-Data[(nrow(Data)-1),which(colnames(Data)=="DMax.D")]))))}

        #if core shorter than 1m we model the OC acumulated mass with depth and predict the stock at 1m
        else {

          Data <-Data %>% mutate (OCM = cumsum(OCgcm2))
          model<-lm(OCM ~ DMax.D, data=Data)
          BCS[i,4]<-coef(model)[1] + 100*coef(model)[2]}}

      }}

write.csv(BCS,file.path(Folder,"Stock_core.csv"),sep=";", dec=",")


########################################
### C fluxes estimation AV 100 years ###
########################################

#select those cores with chronological models
AD = filter(A, !is.na(Age))

length(unique(AD$Core.ID))# number of cores with age estimations

X<-split(AD, AD$Core.ID)

BCF <- data.frame(Core.ID=character(),
                  F.WC=numeric(),
                  A.Max=numeric(),
                  F.100=numeric(),
                  F.75=numeric(),
                  F.50=numeric(),
                  Method=character(),
                  Ecosystem=character(),
                  Genus=character())


for(i in 1:length(X)) {
  BCF[i,1]<-names(X[i])
  Data<-as.data.frame(X[i])
  colnames(Data)<-colnames(AD)
  BCF[i,8]<-Data$Ecosystem[1]
  BCF[i,9]<-Data$Genus[1]

  #Annotate the methods used to obtain the raw dates (14C, 210Pb or 210Pb & 14C)
  Data2 <- subset(Data, Data$D.Method=='14C'|Data$D.Method=='210Pb'|Data$D.Method=='HR')
  if (length(unique(Data2$D.Method))>1) { BCF[i,7]<-"210Pb & 14C"}
    else {BCF[i,7]<-Data2$D.Method[1]}


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
    Data[nrow(Data),which(colnames(Data)=="DMax.D")]-Data[nrow(Data),which(colnames(Data)=="Center")]

    Data <-Data %>% mutate (OCgcm2 = DDBD*(POC/100)*h)

    #estimation of the average carbon flux for the whole core (OC stock/Max Age)
    BCF[i,2]<-(sum(Data[,which(colnames(Data)=="OCgcm2")]))/max(Data$Age)
    BCF[i,3]<-max(Data$Age)

    #estimation of the average carbon flux for the last 100 years (OC stock last 100 yrs/100)
    Data<-Data[c(1:(length(which(Data$Age <=100))+1)),]

    BCF[i,4]<-((((sum(Data[c(1:(nrow(Data)-1)),which(colnames(Data)=="OCgcm2")]))+
      (Data[nrow(Data),which(colnames(Data)=="OCgcm2")]/((max(Data$Age)-Data[(nrow(Data)-1),which(colnames(Data)=="Age")]))
      *(100-Data[(nrow(Data)-1),which(colnames(Data)=="Age")]))))/100)

    #estimation of the average carbon flux for the last 75 years (OC stock last 75 yrs/75)

    if(nrow(Data)<3) next

    else{

        Data<-Data[c(1:(length(which(Data$Age <=75))+1)),]

        BCF[i,5]<-((((sum(Data[c(1:(nrow(Data)-1)),which(colnames(Data)=="OCgcm2")]))+
                       (Data[nrow(Data),which(colnames(Data)=="OCgcm2")]/((max(Data$Age)-Data[(nrow(Data)-1),which(colnames(Data)=="Age")]))
                        *(100-Data[(nrow(Data)-1),which(colnames(Data)=="Age")]))))/75)

        #estimation of the average carbon flux for the last 50 years (OC stock last 50 yrs/50)
        if(nrow(Data)<3) next

        else{

        Data<-Data[c(1:(length(which(Data$Age <=50))+1)),]

        BCF[i,6]<-((((sum(Data[c(1:(nrow(Data)-1)),which(colnames(Data)=="OCgcm2")]))+
                       (Data[nrow(Data),which(colnames(Data)=="OCgcm2")]/((max(Data$Age)-Data[(nrow(Data)-1),which(colnames(Data)=="Age")]))
                        *(100-Data[(nrow(Data)-1),which(colnames(Data)=="Age")]))))/50)



    }}}}

write.csv(BCF,file.path(Folder,"Flux_core.csv"),sep=";", dec=",")

# Differences among dating methods ####

ggplot(BCF,aes(Method,F.100))+
  geom_boxplot()+
  geom_jitter(aes(color=factor(Ecosystem)))

Posi_BCF<-subset(BCF,Genus=="Posidonia oceanica")

ggplot(BCF,aes(Method,F.100))+
  geom_boxplot()+
  geom_jitter(aes(color=factor(Ecosystem)))


#### Made models for selected cores and load data as csv


Date.Method.comp<- data.frame(Core.ID=character(),
                              F.100=numeric(),
                              F.100.Pb=numeric(),
                              F.100.C=numeric(),
                              Ecosystem=character(),
                              Genus=character())

# check if the use of one method or other changes estimated carbon flux significantly
# for this we estimate chronological models with different dating method for the same cores
# select those core were there are dates from more than one dating method
#(in our data cores with both 210Pb and 14C)

XM = vector()


for(i in 1:length(X)) {

  Data<-as.data.frame(X[i])
  colnames(Data)<-colnames(AD)

  Data<- Data[!(Data$D.Method==""), ]

  if (length(unique(Data$D.Method))>1) {XM<-c(XM,i)}
  else {next}}

Data_XM<-sapply(XM, function(z) X[z])

#Those core with more than one dating method are stored at Data_XM as data.frames

#Extract data from the individual core data.frame to build "bacon" data.frames:
#1st data.frame with all the dates, 2nd with only 210Pb dates and 3rd with only 14C dates
#and estimate the average 100 year C fluxes with each

# we need to give confirmation to bacon function from {rbacon} for each age-depth model estimation



for(i in 1:length(Data_XM)) {


  Data<-as.data.frame(Data_XM[i])
  colnames(Data)<-colnames(AD)

  Date.Method.comp[i,1]<-Data$Core.ID[1]
  Date.Method.comp[i,5]<-Data$Ecosystem[1]
  Date.Method.comp[i,6]<-Data$Genus[1]


  Bacon_D <- as.data.frame(matrix(NA, nrow = nrow(Data), ncol = 7))
  colnames(Bacon_D)<-c("labID", "age", "error","depth","cc","dR","dSTD")

  Bacon_D$labID<-paste(Data$Core.ID,Data$Max.D)
  Bacon_D$age<-Data$Raw.Age.BP
  Bacon_D$error<-Data$Date.Error
  Bacon_D$depth<-Data$Center
  Bacon_D$cc<-Data$Bacon.cc
  Bacon_D$dR<-Data$Bacon.dr
  Bacon_D$dSTD<-Data$Bacon.dSTD

  Bacon_D<-na.omit(Bacon_D)
  Bacon_Pb<-Bacon_D[c(1:length(which(Data$D.Method == "210Pb"))),] #to select columns with Pb values
  Bacon_14C<-Bacon_D[-c(1:length(which(Data$D.Method == "210Pb"))),] #to select columns with NO Pb values (in our data 14C values)

  ###create folders and save bacon files ####

  dir.create("Bacon_runs")
  path <- "Bacon_runs"

  newfolder<-Data$Core.ID[1]
  newpath<- file.path(path, newfolder)
  dir.create(newpath)
  write.csv(Bacon_D,file.path(newpath,paste0(newfolder,".csv")),row.names=FALSE, quote=F) # use paste0() no paste() to create file names o bacon will have trouble with the espace between the file name and de .csv

  newfolder<-paste(Data$Core.ID[1],"Pb")
  newpath<- file.path(path, newfolder)
  dir.create(newpath)
  write.csv(Bacon_Pb,file.path(newpath,paste0(newfolder,".csv")),row.names=FALSE, quote=F)

  newfolder<-paste(Data$Core.ID[1],"C")
  newpath<- file.path(path, newfolder)
  dir.create(newpath)
  write.csv(Bacon_14C,file.path(newpath,paste0(newfolder,".csv")),row.names=FALSE, quote=F)

  ###run bacon ####

  #bacon with both
  bfile<-Data$Core.ID[1]
  Bacon(bfile,
        d.min=0,d.max=max(Data$DMax.D),
        MinYr=-66)

  #load chronological model file
  results<-list.files(path = file.path(path, Data$Core.ID[1]), full.names = TRUE)
  Ages.file<-results[grep("*_ages*", results)]
  crono1<-read.table(Ages.file, header = T, sep = "", dec = ".")

  #bacon with Pb
  bfile<-paste(Data$Core.ID[1],"Pb")
  Bacon(bfile,
        d.min=0,d.max=max(Data$DMax.D),
        MinYr=0-66)

  #load chronological model file
  results<-list.files(path = file.path(path, paste(Data$Core.ID[1],"Pb")), full.names = TRUE)
  Ages.file<-results[grep("*_ages*", results)]
  crono2<-read.table(Ages.file, header = T, sep = "", dec = ".")

  #bacon with 14C
  bfile<-paste(Data$Core.ID[1],"C")
  Bacon(bfile,
        d.min=0,d.max=max(Data$DMax.D),
        MinYr=-66)


  #load chronological model file
  results<-list.files(path = file.path(path, paste(Data$Core.ID[1],"C")), full.names = TRUE)
  Ages.file<-results[grep("*_ages*", results)]
  crono3<-read.table(Ages.file, header = T, sep = "", dec = ".")

  ###create data frame with depth, DBD, POC and bacon models ####

  temp <-data.frame(matrix(NA, nrow = nrow(Data), ncol = 8))
  colnames(temp)<-c("Center","Age", "Age.Pb", "Age.C","DDBD","DMin.D","DMax.D","POC")


  temp$Center<-Data$Center#as depth we chose the center of the sample
  temp$DDBD<-Data$DDBD
  temp$DMin.D<-Data$DMin.D
  temp$DMax.D<-Data$DMax.D
  temp$POC<-Data$POC

  #we fill the conological model columns with the mean age at the depth of the center of the sample
  #for each of the estimated cronological models
  for (j in 1:(length(temp$Center))) {

    temp[j,"Age"]<-crono1[which.min(abs( crono1[,"depth"]- temp[j,"Center"])),"mean"]
  }

  for (j in 1:(length(temp$Center))) {

    temp[j,"Age.Pb"]<-crono2[which.min(abs(crono2[,"depth"]- temp[j,"Center"])),"mean"]
  }

  for (j in 1:(length(temp$Center))) {

    temp[j,"Age.C"]<-crono3[which.min(abs(crono3[,"depth"]- temp[j,"Center"])),"mean"]
  }


  # fluxes estimation with each chronological model ####


  temp<-na.omit(temp)

  temp$h<-NA

  #estimation of the thickness of the sample (h)
  for (j in 2:(nrow(temp)-1)) {

    temp[j,which(colnames(temp)=="h")]<-
      ((temp[j,which(colnames(temp)=="Center")]-temp[(j-1),which(colnames(temp)=="Center")])/2)+
      ((temp[(j+1),which(colnames(temp)=="Center")]-temp[j,which(colnames(temp)=="Center")])/2)
  }

  temp[1,which(colnames(temp)=="h")]<-temp[1,which(colnames(temp)=="Center")]
  temp[nrow(temp),which(colnames(temp)=="h")]<-
    temp[nrow(temp),which(colnames(temp)=="DMax.D")]-temp[nrow(temp),which(colnames(temp)=="Center")]

  #estimation of carbon g cm2 per sample, OCgcm2= carbon density (g cm3) by thickness (h)

  temp <-temp %>% mutate (OCgcm2 = DDBD*(POC/100)*h)


  #estimation of the average carbon flux for the last 100 years (OC stock last 100 yrs/100)

  BP100<-(1950-(Data[1,"Samp.year"]))+100#last 100 years per core in BP



  temp2<-temp[c(1:(length(which(temp$Age <=BP100))+1)),]

  if (nrow(temp2)>1) {


    Date.Method.comp[i,2]<-((((sum(temp2[c(1:(nrow(temp2)-1)),which(colnames(temp2)=="OCgcm2")]))+
                                (temp2[nrow(temp2),which(colnames(temp2)=="OCgcm2")]/((max(temp2$Age)-temp2[(nrow(temp2)-1),which(colnames(temp2)=="Age")]))
                                 *(BP100-temp2[(nrow(temp2)-1),which(colnames(temp2)=="Age")]))))/100)
  }
            else {next}


  temp2<-temp[c(1:(length(which(temp$Age.Pb <=BP100))+1)),]

  if (nrow(temp2)>1) {

    Date.Method.comp[i,3]<-((((sum(temp2[c(1:(nrow(temp2)-1)),which(colnames(temp2)=="OCgcm2")]))+
                                (temp2[nrow(temp2),which(colnames(temp2)=="OCgcm2")]/((max(temp2$Age.Pb)-temp2[(nrow(temp2)-1),which(colnames(temp2)=="Age.Pb")]))
                                 *(BP100-temp2[(nrow(temp2)-1),which(colnames(temp2)=="Age.Pb")]))))/100)
  }
  else {next}

  temp2<-temp[c(1:(length(which(temp$Age.C <=BP100))+1)),]

  if (nrow(temp2)>1) {
    Date.Method.comp[i,4]<-((((sum(temp2[c(1:(nrow(temp2)-1)),which(colnames(temp2)=="OCgcm2")]))+
                                (temp2[nrow(temp2),which(colnames(temp2)=="OCgcm2")]/((max(temp2$Age.C)-temp2[(nrow(temp2)-1),which(colnames(temp2)=="Age.C")]))
                                 *(BP100-temp2[(nrow(temp2)-1),which(colnames(temp2)=="Age.C")]))))/100)
  }
  else {next} }

# compare fluxes estimated with each model

ggplot(Date.Method.comp, aes(F.100, F.100.Pb))+xlab("210Pb+14C")+ ylab("210Pb cyrcle, 14C triangle")+ggtitle("100 yr average C flux (g m-2 yr)")+
  geom_point(aes(F.100, F.100.Pb, color=Genus), shape=16, size=4)+
  geom_point(aes(F.100, F.100.C, color=Genus), shape=17, size=4)+
  theme(text = element_text(size=15))+
  #geom_text_repel(aes(label=A[,1]), size=4)+
  xlim(0,0.01)+ylim(0,0.01)+
  geom_abline()

## check if differences can have something to do with the age of the first 14C date


mDate.Method.comp<-melt(Date.Method.comp[,c(1:4)], id=c("Core.ID"))

ggplot(mDate.Method.comp, aes(x=as.factor(variable), y=as.numeric(value))) +
  geom_boxplot()+
  geom_jitter()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

pairwise.wilcox.test(as.numeric(mDate.Method.comp$value), mDate.Method.comp$variable,#are significantly different (p < 0.05)
                     p.adjust.method = "BH")



## differences between 100, 75 and 50 time frames ####

Fluxdiff<-melt(BCF[,c(1,4:6)], id=c("Core.ID"))

ggplot(Fluxdiff, aes(x=as.factor(variable), y=as.numeric(value))) +
  geom_boxplot()+
  geom_jitter()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

pairwise.wilcox.test(as.numeric(Fluxdiff$value), Fluxdiff$variable,#are significantly different (p < 0.05)
                     p.adjust.method = "BH")



###############
### Biomass ###
###############

File<-"Biomass.csv"

C<-read.csv(File, header=T, sep=";", dec=".")
C<-as.data.frame(C)

length(unique(C$Sample.ID))# number of biomass samples

File<-"C.Plant.csv"

D<-read.csv(File, header=T, sep=";", dec=".")
D<-as.data.frame(D)


#average the OC content in different tissues by specie

TOC.Planta <- as.data.frame(tapply(D$TOC, list(D$Specie, D$Tissue), mean))
TOC.Planta.2 <- as.data.frame(tapply(D$TOC, list(D$Ecosystem, D$Tissue), mean))



#estimate OC in biomass

C$Ab.TOC<- NA

species<-rownames(TOC.Planta)

for(i in 1:nrow(C)) {
  #si hay datos pa biomassa abouve de esa especie usa esa especie
  if(C[i,which( colnames(C)=="Specie" )] %in% species==TRUE)

    {if ((is.na(TOC.Planta[which(rownames(TOC.Planta)==C[i,which( colnames(C)=="Specie" )]),which( colnames(TOC.Planta)=="Aboveground" )]))==FALSE)

      {C[i,which( colnames(C)=="Ab.TOC" )]<-C[i,which( colnames(C)=="Above" )]*
                                              ((TOC.Planta[which(rownames(TOC.Planta)==C[i,which( colnames(C)=="Specie" )]),which( colnames(TOC.Planta)=="Aboveground" )]/100))}

      else { if (is.na(TOC.Planta[which(rownames(TOC.Planta)==C[i,which( colnames(C)=="Specie" )]),which( colnames(TOC.Planta)=="Leaves" )])==FALSE)

            {C[i,which( colnames(C)=="Ab.TOC" )]<-C[i,which( colnames(C)=="Above" )]*
            ((TOC.Planta[which(rownames(TOC.Planta)==C[i,which( colnames(C)=="Specie" )]),which( colnames(TOC.Planta)=="Leaves" )]/100))}

            else {next}}}
  #If there is no TOC for that specie use the average of the ecosystem
  else { C[i,which( colnames(C)=="Ab.TOC" )]<-C[i,which( colnames(C)=="Above" )]*
    ((TOC.Planta.2[which(rownames(TOC.Planta.2)==C[i,which( colnames(C)=="Ecosystem" )]),which( colnames(TOC.Planta.2)=="Aboveground" )]/100))}
}


#Average by site
B_by_Site <-merge(aggregate( Ab.TOC ~ Site.ID, C, mean), aggregate( Ab.TOC ~ Site.ID, C, sd), by = "Site.ID")
colnames(B_by_Site)<-c("Site.ID", "Mean_Biomass", "SD_Biomass")

##############################
### Summary BC per station ###
##############################


#Add information about site.ID to stocks and fluxes data.frames


# Stock 1m. Mean and sd by site
BCS$Site.ID<-NA

for (i in 1:nrow(BCS)) {

  Site<- unique(A[c(which(A$Core.ID==BCS[i,which(colnames(BCS)=="Core.ID")])),which(colnames(A)=="Site.ID")])
  BCS[i,which(colnames(BCS)=="Site.ID")]<- Site

}

S_by_Site <-merge(aggregate( S.1m ~ Site.ID, BCS, mean), aggregate( S.1m ~ Site.ID, BCS, sd), by = "Site.ID")

colnames(S_by_Site)<-c("Site.ID", "Mean_S.1m", "SD_S.1m")

# Flux 100 yr. Mean and sd by site

BCF$Site.ID<-NA

for (i in 1:nrow(BCF)) {

  Site<- unique(A[c(which(A$Core.ID==BCF[i,which(colnames(BCF)=="Core.ID")])),which(colnames(A)=="Site.ID")])
  BCF[i,which(colnames(BCF)=="Site.ID")]<- Site

}

F_by_Site <-merge(aggregate( F.100 ~ Site.ID, BCF, mean), aggregate( F.100 ~ Site.ID, BCF, sd), by = "Site.ID")

colnames(F_by_Site)<-c("Site.ID", "Mean_F.100", "SD_F.100")

### Summary table per station (Site)

File<-"GInf.csv"

B<-read.csv(File, header=T, sep=";", dec=".")
B<-as.data.frame(B)

### Final data.frame with stocks at 1m and fluxes at 100 yr by station
#with information about region, coast, genus and Ecosystem

library(dplyr)
library(purrr)


BC_PI <-merge(B, S_by_Site, by = "Site.ID", all = T)

BC_PI<-left_join(BC_PI, F_by_Site, by="Site.ID",all = T)

BC_PI<-left_join(BC_PI, B_by_Site, by="Site.ID",all = T)

### !!!!!!!!!!!! from g cm2 to kg m2

BC_PI [,c(13:16)]<- BC_PI [,c(13:16)]*10

write.csv(BC_PI,file.path(Folder,"BC_Station.csv"),sep=";", dec=",")

#########################
### Data distribution ###
#########################
#escala en los mapas!!

WM <- map_data("world")


IP <- subset(WM, region %in% c("Portugal","Spain", "Canary Islands", "Azores", "Madeira Islands"))


DBC_PI = filter(BC_PI, !is.na(Mean_Biomass))
BSg<-subset(DBC_PI, Ecosystem=='Seagrass')
BSm<-subset(DBC_PI, Ecosystem=='Salt Marsh')

DBC_PI = filter(BC_PI, !is.na(Mean_S.1m))
SSg<-subset(DBC_PI, Ecosystem=='Seagrass')
SSm<-subset(DBC_PI, Ecosystem=='Salt Marsh')

DBC_PI = filter(BC_PI, !is.na(Mean_F.100))
FSg<-subset(DBC_PI, Ecosystem=='Seagrass')
FSm<-subset(DBC_PI, Ecosystem=='Salt Marsh')



BSgp<-ggplot()+ ylab("Latitude")+
  geom_polygon(data = WM, aes(x=long, y = lat, group = group), fill = "white", color = "black")+
  geom_polygon(data = IP, aes(x=long, y = lat, group = group), fill = "grey", color = "black")+
  coord_map(xlim = c(-30, 5),ylim = c(27, 45))+
  geom_point(aes(BSg$long,BSg$lat), fill="green",pch=21,size=2.5)+
  theme(axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank()
  )

BSmp<-ggplot()+
  geom_polygon(data = WM, aes(x=long, y = lat, group = group), fill = "white", color = "black")+
  geom_polygon(data = IP, aes(x=long, y = lat, group = group), fill = "grey", color = "black")+
  coord_map(xlim = c(-30, 5),ylim = c(27, 45))+
  geom_point(aes(BSm$long,BSm$lat), fill="blue",pch=21,size=2.5)+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank()
  )


SSgp<-ggplot()+ ylab("Latitude")+
  geom_polygon(data = WM, aes(x=long, y = lat, group = group), fill = "white", color = "black")+
  geom_polygon(data = IP, aes(x=long, y = lat, group = group), fill = "grey", color = "black")+
  coord_map(xlim = c(-30, 5),ylim = c(27, 45))+
  geom_point(aes(SSg$long,SSg$lat), fill="green",pch=21,size=2.5)+
  theme(axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank()
        )

SSmp<-ggplot()+
  geom_polygon(data = WM, aes(x=long, y = lat, group = group), fill = "white", color = "black")+
  geom_polygon(data = IP, aes(x=long, y = lat, group = group), fill = "grey", color = "black")+
  coord_map(xlim = c(-30, 5),ylim = c(27, 45))+
  geom_point(aes(SSm$long,SSm$lat), fill="blue",pch=21,size=2.5)+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank()
  )


FSgp<-ggplot()+ xlab("Longitude")+ ylab("Latitude")+
  geom_polygon(data = WM, aes(x=long, y = lat, group = group), fill = "white", color = "black")+
  geom_polygon(data = IP, aes(x=long, y = lat, group = group), fill = "grey", color = "black")+
  coord_map(xlim = c(-30, 5),ylim = c(27, 45))+
  geom_point(aes(FSg$long,FSg$lat), fill="green",pch=21,size=2.5)


FSmp<-ggplot()+ xlab("Longitude")+
  geom_polygon(data = WM, aes(x=long, y = lat, group = group), fill = "white", color = "black")+
  geom_polygon(data = IP, aes(x=long, y = lat, group = group), fill = "grey", color = "black")+
  coord_map(xlim = c(-30, 5),ylim = c(27, 45))+
  geom_point(aes(FSm$long,FSm$lat), fill="blue",pch=21,size=2.5)+
  theme(axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.y=element_blank()
  )


grid.arrange(arrangeGrob(BSgp, top="Seagrass", left=textGrob("Ab Biomass stock",rot = 90,gp=gpar(fontsize=15))),arrangeGrob(BSmp,top="Salt Marsh"),
             arrangeGrob(SSgp, left=textGrob("OC Soil stock",rot = 90,gp=gpar(fontsize=15))),SSmp,
             arrangeGrob(FSgp, left=textGrob("OC flux",rot = 90, gp=gpar(fontsize=15))),FSmp,
             ncol=2, nrow=3,top=textGrob("Sampling sites", gp=gpar(fontsize=15)),
             widths = c(1,0.9))

final<-arrangeGrob(arrangeGrob(BSgp, top="Seagrass", left=textGrob("Ab Biomass stock",rot = 90,gp=gpar(fontsize=15))),arrangeGrob(BSmp,top="Salt Marsh"),
                    arrangeGrob(SSgp, left=textGrob("OC Soil stock",rot = 90,gp=gpar(fontsize=15))),SSmp,
                    arrangeGrob(FSgp, left=textGrob("OC flux",rot = 90, gp=gpar(fontsize=15))),FSmp,
                    ncol=2, nrow=3,top=textGrob("Sampling sites", gp=gpar(fontsize=15)),
                    widths = c(1,0.9))


ggsave(path = Folder,"Sampling sites.jpg",final, units="cm", width = 20, height = 20)

#The patchwork package is another option for laying out multiple plots and it also lines up the plot panels
#Unfortunately, patchwork doesn't provide an easy way to add spanning axis titles (like the bottom, left, and right arguments of grid.arrange)
#so we have to manually set the widths for those grobs, relative to the plot grobs.
#https://community.rstudio.com/t/common-axis-title-in-grid-arrange/96353/2


####################################
### Comparisons among categories ###
####################################

############ Tabla de areas !!!!



#delete row containing information about unvegetated stations
BC_PI<-BC_PI[!(BC_PI$Genus=="Unvegetated Salt Marsh" | BC_PI$Genus=="Unvegetated Seagrass"| BC_PI$Genus=="Unvegetated"),]

### Sumary table ###

Summary<- data.frame(Category=character(),
                     nS.Biomass=numeric(),
                     nP.Biomass=numeric(),
                     PA.Biomass=numeric(),
                     Av.Biomass=numeric(),
                     M.Biomass=numeric(),
                     SE.Biomass=numeric(),
                     nS.Stock=numeric(),
                     nC.Stock=numeric(),
                     PA.Stock=numeric(),
                     Av.Stock=numeric(),
                     M.Stock=numeric(),
                     SE.Stock=numeric(),
                     nS.Flux=numeric(),
                     nC.Flux=numeric(),
                     PA.Stock=numeric(),
                     Av.Flux=numeric(),
                     M.Flux=numeric(),
                     SE.Flux=numeric())





std.error <- function(x) sd(x)/sqrt(length(x))

X<-split(BC_PI, BC_PI$Ecosystem)
Sg<-subset(BC_PI, Ecosystem=='Seagrass')
X2<-split(Sg, Sg$Genus)
Sm<-Sg<-subset(BC_PI, Ecosystem=='Salt Marsh')
X3<-split(Sm, Sm$Tidal.R)

X<-c(X,X2,X3)


for(i in 1:length(X)) {


  Data<-as.data.frame(X[i])
  colnames(Data)<-colnames(BC_PI)

  Summary[i,1]<-names(X[i])

  DataB = filter(Data, !is.na(Mean_Biomass))

  Summary[i,3]<-nrow(DataB)
  #Summary[i,4]
  Summary[i,5]<-mean(DataB$Mean_Biomass)
  Summary[i,6]<-median(DataB$Mean_Biomass)
  Summary[i,7]<-std.error(DataB$Mean_Biomass)

  DataS = filter(Data, !is.na(Mean_S.1m))
  Summary[i,9]<-nrow(DataS)
  #Summary[i,10]
  Summary[i,11]<-mean(DataS$Mean_S.1m)
  Summary[i,12]<-median(DataS$Mean_S.1m)
  Summary[i,13]<-std.error(DataS$Mean_S.1m)

  DataF = filter(Data, !is.na(Mean_F.100))
  Summary[i,15]<-nrow(DataF)
  #Summary[i,16]
  Summary[i,17]<-mean(DataF$Mean_F.100)
  Summary[i,18]<-median(DataF$Mean_F.100)
  Summary[i,19]<-std.error(DataF$Mean_F.100)

}

write.csv(Summary,file.path(Folder,"Summary table.csv"),sep=";", dec=",")

# normal distribution

shapiro.test(BC_PI$Mean_Biomass)
shapiro.test(BC_PI$Mean_S.1m) ### normality (>0.05 normal, <0.05 no normal)
shapiro.test(BC_PI$Mean_F.100)

ggplot(BC_PI, aes(x=Mean_Biomass)) +
  geom_histogram()







ggplot(BC_PI, aes(Site.ID, Mean_Biomass))+
  geom_point()

ggplot(BC_PI,aes(Ecosystem, Mean_Biomass))+
  geom_boxplot()+
  geom_jitter(aes(color=factor(Genus)))

ggplot(BC_PI,aes(Coast, Mean_Biomass))+
  geom_boxplot()+
  geom_jitter(aes(color=factor(Ecosystem)))

ggplot(BC_PI,aes(Genus, Mean_S.1m))+
  geom_boxplot()+
  geom_jitter(aes(color=factor(Coast)))

ggplot(BC_PI,aes(Region, Mean_S.1m))+
  geom_boxplot()+
  geom_jitter(aes(color=factor(Ecosystem)))


########### Seagrass ############


Sg<-subset(BC_PI, Ecosystem=='Seagrass')


Sg$Genus <- factor(Sg$Genus, levels = c("Posidonia oceanica","Cymodocea nodosa","Zostera marina","Zostera noltii", "Mixed Seagrass"),ordered = TRUE)

ggplot(Sg,aes(Genus, Mean_Biomass))+
  geom_boxplot()+
  geom_jitter(aes(color=factor(Coast)))+
  ylim(0,2)

# normal distribution

shapiro.test(Sg$Mean_S.1m) ### normality (>0.05 normal, <0.05 no normal)

ggplot(Sg, aes(x=Mean_S.1m)) +
  geom_histogram()

listids <- list()
for (ids in unique(Sg$Genus)){
  subSg <- subset(x=Sg, subset=Genus==ids)
  # apply the rest of your analysis there using subdf, for instance
  listids[[ids]] <- shapiro.test(subSg$Mean_Biomass)

  print(ggplot(subSg, aes(x=Mean_Biomass)) + ggtitle(ids)+
    geom_histogram())

}





#significate differences

pairwise.wilcox.test(Sg$Mean_Biomass, Sg$Genus, #### are significantly different (p < 0.05)
                     p.adjust.method = "BH")



ggplot(Sg,aes(Genus, Mean_Biomass))+
  geom_boxplot()+
  geom_jitter(aes(color=factor(Coast)))

ggplot(Sg,aes(Genus, Mean_F.100))+
  geom_boxplot()+
  geom_jitter(aes(color=factor(Coast)))


SS<-ggplot(Sg,aes(Genus, Mean_S.1m))+ ylab(expression(paste("Soil C stock (kg"," ", m^-2, ")")))+
  geom_boxplot()+
  geom_jitter(aes(color=factor(Coast)))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

SB<-ggplot(Sg,aes(Genus, Mean_Biomass))+ ylab(expression(paste("Biomass C stock (kg"," ", m^-2, ")")))+
  geom_boxplot()+
  geom_jitter(aes(color=factor(Coast)))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylim(0,2)

F100<-ggplot(Sg,aes(Genus, Mean_F.100))+ ylab(expression(paste("C flux last 100 years (kg"," ", m^-2,yr^-1, ")")))+
  geom_boxplot()+
  geom_jitter(aes(color=factor(Coast)))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(face="italic"))


SgPlot<-grid.arrange(SB,SS, F100, nrow=3, top="Seagrass specie")

ggsave(path = Folder,"Summary seagrass.jpg",SgPlot, units="cm", width = 19, height = 20)

#Bathymetry

Sg %>% group_by(Genus) %>% summarise(r =cor(Water.Depth,Mean_S.1m, use="complete.obs"))

Sg1 = filter(Sg, !is.na(Mean_F.100))
Sg1 %>% group_by(Genus) %>% summarise(r =cor(Water.Depth,Mean_F.100,use="complete.obs"))


Sg %>% group_by(Locality) %>% summarise(r =cor(Water.Depth,Mean_S.1m))

Sg1 = filter(Sg, !is.na(Mean_F.100))
Sg1 %>% group_by(Locality) %>% summarise(r =cor(Water.Depth,Mean_F.100,use="complete.obs"))




SS<-ggplot(Sg,aes(Water.Depth, Mean_S.1m))+
  geom_point(aes(color=Sg$Genus))+
  geom_errorbar(aes(ymin=Mean_S.1m-SD_S.1m, ymax=Mean_S.1m+SD_S.1m), width=.1)

FS<-ggplot(Sg,aes(Water.Depth, Mean_F.100))+
  geom_point(aes(color=Sg$Genus))+
  geom_errorbar(aes(ymin=Mean_F.100-SD_F.100, ymax=Mean_F.100+SD_F.100), width=.1)




########## Salt marshes #########

Sm<-BC_PI[!(BC_PI$Ecosystem=="Seagrass"),]
Sm = filter(Sm, !is.na(Site.ID))


Sm$Tidal.R <- factor(Sm$Tidal.R, levels = c("High","Medium","Low","Microtidal"),ordered = TRUE)

pairwise.wilcox.test(Sm$Mean_F.100, Sm$Tidal.R, #### are significantly different (p < 0.05)
                     p.adjust.method = "BH")

ggplot(Sm,aes(Tidal.R, Mean_F.100))+
  geom_boxplot()+
  geom_jitter(aes(color=factor(Coast)))

ggplot(Sm,aes(Tidal.R, Mean_S.1m))+
  geom_boxplot()+
  geom_jitter(aes(color=factor(Coast)))


shapiro.test(Sm$Mean_S.1m) ### normality (>0.05 normal, <0.05 no normal)

ggplot(Sm, aes(x=Mean_S.1m)) +
  geom_histogram()



#sumary plot salt marshes
SS<-ggplot(Sm,aes(Tidal.R, Mean_S.1m))+ ylab(expression(paste("Soil C stock (kg"," ", m^-2, ")")))+
  geom_boxplot()+
  geom_jitter(aes(color=factor(Coast)))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

SB<-ggplot(Sm,aes(Tidal.R, Mean_Biomass))+ ylab(expression(paste("Biomass C stock (kg"," ", m^-2, ")")))+
  geom_boxplot()+
  geom_jitter(aes(color=factor(Coast)))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

F100<-ggplot(Sm,aes(Tidal.R, Mean_F.100))+ ylab(expression(paste("C flux last 100 years (kg"," ", m^-2,yr^-1, ")")))+
  geom_boxplot()+
  geom_jitter(aes(color=factor(Coast)))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(face="italic"))


SmPlot<-grid.arrange(SB,SS, F100, nrow=3, top="Salt Marsh Tidal Range")
ggsave(path = Folder,"Summary salt marsh.jpg",SmPlot, units="cm", width = 19, height = 20)

# Summary figure


EB<-ggplot(Summary[c(1:2),], aes(x=Category, y=Av.Biomass))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=Av.Biomass-SE.Biomass, ymax=Av.Biomass+SE.Biomass), width=.2)

SgB<-ggplot(Summary[c(3:7),], aes(x=Category, y=Av.Biomass))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=Av.Biomass-SE.Biomass, ymax=Av.Biomass+SE.Biomass), width=.2)

SmB<-ggplot(Summary[c(8:10),], aes(x=Category, y=Av.Biomass))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=Av.Biomass-SE.Biomass, ymax=Av.Biomass+SE.Biomass), width=.2)


ES<-ggplot(Summary[c(1:2),], aes(x=Category, y=Av.Stock))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=Av.Stock-SE.Stock, ymax=Av.Stock+SE.Stock), width=.2)

SgS<-ggplot(Summary[c(3:7),], aes(x=Category, y=Av.Stock))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=Av.Stock-SE.Stock, ymax=Av.Stock+SE.Stock), width=.2)

SmS<-ggplot(Summary[c(8:10),], aes(x=Category, y=Av.Stock))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=Av.Stock-SE.Stock, ymax=Av.Stock+SE.Stock), width=.2)


EF<-ggplot(Summary[c(1:2),], aes(x=Category, y=Av.Flux))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=Av.Flux-SE.Flux, ymax=Av.Flux+SE.Flux), width=.2)

SgF<-ggplot(Summary[c(3:7),], aes(x=Category, y=Av.Flux))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=Av.Flux-SE.Flux, ymax=Av.Flux+SE.Flux), width=.2)

SmF<-ggplot(Summary[c(8:10),], aes(x=Category, y=Av.Flux))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=Av.Flux-SE.Flux, ymax=Av.Flux+SE.Flux), width=.2)

