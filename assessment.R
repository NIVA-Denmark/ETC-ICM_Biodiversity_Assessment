library("dplyr")
library("tidyr")

#===============================================================================
# function Assessment
Assessment<- function(assessmentdata,summarylevel=1){
  
  
  requiredcols <- c("Category","Indicator","Threshold","Status","Reference","Bad")
  extracols <- c("SpatialAssessmentUnit")
  
  
  #Check column names in the imported data
  cnames<-names(assessmentdata)
  nimp = ncol(assessmentdata)
  nreq = length(requiredcols)
  nextra = length(extracols)
  
  ok <- rep(0, nreq)
  okextra <- rep(0, nextra)
  foundresponse=FALSE
  
  for (i in 1:nimp){
    for (j in 1:nreq){
      if(toupper(requiredcols[j])==toupper(cnames[i])){
        names(assessmentdata)[i] <- requiredcols[j]
        ok[j]=1
      }
    }
    for (j in 1:nextra){
      if(toupper(extracols[j])==toupper(cnames[i])){
        names(assessmentdata)[i] <- extracols[j]
        okextra[j]=1
      }
    }
  }
  
  
  
  n<-sum(ok, na.rm = TRUE)
  
  if(n<nreq){
    # The required columns were not found in the input data
    message("Error! Required column(s) were not found in the input data:")
    for (j in 1:nreq){
      if(ok[j]!=1){
        message(paste("    ",requiredcols[j]))
      }
    }
    if(summarylevel==1){
      return(assessmentdata)
    }else{    return(NA)}
  }else{
    # The required columns are present - do the assessment
    for(j in 1:nextra){
      if(okextra[j]==0){
        if(extracols[j]=="SpatialAssessmentUnit"){
          assessmentdata[[extracols[j]]]<-"All"
        }else{
          assessmentdata[[extracols[j]]]<-1
        }
      }
    }
    

    # Change order of category factors
    cat1<-data.frame(unique(assessmentdata$Category))
    names(cat1)[1] <- 'Category'
    cat1$char<-as.character(cat1$Category)
    cat1$len<-nchar(cat1$char)
    cat1<-arrange(cat1,len)
    
    assessmentdata$Category <- factor(assessmentdata$Category, levels = cat1$char)
    
    # All combinations of categories and waterbodies
    # This is used to ensure that a NA is returned where the combinations are missing
    waterbodies<-unique(assessmentdata$SpatialAssessmentUnit)
    categories<-unique(assessmentdata$Category)
    categories<-expand.grid(waterbodies, categories)
    names(categories)[1] <- 'SpatialAssessmentUnit'
    names(categories)[2] <- 'Category'
    
    # Reorder assessment units

    df<-as.data.frame(unique(assessmentdata$SpatialAssessmentUnit))
    names(df)[1]<-"SpatialAssesmentUnit"
    df$SpatialAssesmentUnit<-as.character(df$SpatialAssesmentUnit)
    df$pos<-regexpr(" ",df$SpatialAssesmentUnit)
    df$n1<-as.numeric(substr(df$SpatialAssesmentUnit, 1, df$pos-4))
    df$n2<-as.numeric(substr(df$SpatialAssesmentUnit, df$pos-2, df$pos-1))
    df<-arrange(df,n1,n2)
    #df$SpatialAssesmentUnit

    assessmentdata$SpatialAssessmentUnit<- factor(assessmentdata$SpatialAssessmentUnit, levels = df$SpatialAssesmentUnit)
    
    
    #"Category","Indicator","Threshold","Status","Reference","Bad"
    
    temp<-EQR(indData=assessmentdata,Obs="Status",Bad="Bad",ModGood="Threshold",High="Reference")
    assessmentdata$EQR<-temp$EQR
    
    QEdata<-summarise(group_by(assessmentdata,SpatialAssessmentUnit,Category),
                      IndCount=n(),EQR=mean(EQR,na.rm = TRUE))

    QEspr<-spread(QEdata,Category,EQR)
    
    QEdata$CategoryClass<-EQRStatus(QEdata$EQR)
    QEdata<-left_join(categories,QEdata,c('SpatialAssessmentUnit','Category'))
    QEdata<-arrange(QEdata,SpatialAssessmentUnit,Category)
    
    Overall<-summarise(group_by(QEdata,SpatialAssessmentUnit), EQR=min(EQR, na.rm = TRUE))
    
    OverallQE<-inner_join(select(QEdata,-c(IndCount)), Overall, 
                          c("EQR"="EQR","SpatialAssessmentUnit"="SpatialAssessmentUnit"))
    OverallQE<-rename(OverallQE,Class=CategoryClass,Worst=Category)

    QEspr<-inner_join(QEspr, OverallQE, 'SpatialAssessmentUnit')
    
    
    for(j in 1:nextra){
      if(extracols[j]=='SpatialAssessmentUnit' & okextra[j]==0){
        #assessmentdata[[extracols[j]]]<-NULL
        #QEdata[[extracols[j]]]<-NULL
      }
    }
    Indicators<-assessmentdata #%>%
      #select(-c(Bad,Reference,Threshold,Status))
    
    #return(n)
    if(summarylevel==1){
      return(Indicators)
    }else if(summarylevel==2){
      return(QEspr)
    }else if(summarylevel==3){
      return(QEdata)
    }else if(summarylevel==4){
      return(OverallQE)
    }else{
      return(assessmentdata)
    }
    #
  }
}

#===============================================================================
# function EQR

# Functions to Calculate EQR --------------------------------------------------------------------
EQR<-function(indData,Obs,Bad,BadPoor="BP",PoorMod="PM",ModGood,GoodHigh="GH",High,GoodHigh2,ModGood2,PoorMod2,BadPoor2,Bad2){
  if (missing(Obs))
    stop("Need to specify variable 'Obs' containing the observed indicator value")
  if (missing(Bad))
    stop("Need to specify variable 'Bad' containing the indicator value corresponding to EQR=0.0 \n  (The worst possible value the indicator can take)")
  if (missing(ModGood))
    stop("Need to specify variable 'ModGood' containing the indicator value corresponding to EQR=0.6 \n   (The GES or 'Moderate/Good' boundary)  ")
  if (missing(High))
    stop("Need to specify variable 'High' containing the indicator value corresponding to EQR=1.0 \n  (The best possible value the indicator can take).")
  
  if ((High %in% names(indData))==FALSE) stop(paste0("Variable not found: High='",High,"'"))
  if ((Bad %in% names(indData))==FALSE) stop(paste0("Variable not found: Bad='",Bad,"'"))
  if ((ModGood %in% names(indData))==FALSE) stop(paste0("Variable not found: ModGood='",ModGood,"'"))
  
  temp<-indData
  
  if ((BadPoor %in% names(indData))==FALSE)  temp[,BadPoor]<-NA
  if ((PoorMod %in% names(indData))==FALSE)  temp[,PoorMod]<-NA
  if ((GoodHigh %in% names(indData))==FALSE)  temp[,GoodHigh]<-NA
  
  
  # Type 1 - most common indicator type - increasing monotonically
  # Type 2 - optimum range
  temp$Err<-""
  temp$OK<-0
  temp$Sign<-0
  temp$EQR<-NA
  
  for (i in 1:nrow(temp)){
    
    if(is.na(temp[i,Bad])||is.na(temp[i,ModGood])||is.na(temp[i,High])
    ){
      #Missing a key value
      temp[i,'Err']<-"Missing  boundary value"
    }
    else{
      if(is.na(temp[i,BadPoor])&&is.na(temp[i,PoorMod])){
        temp[i,BadPoor]=(2*temp[i,Bad]+temp[i,ModGood])/3
        temp[i,PoorMod]=(temp[i,Bad]+2*temp[i,ModGood])/3
      }
      if(is.na(temp[i,BadPoor])){
        temp[i,BadPoor]=(temp[i,Bad]+temp[i,PoorMod])/2
      }      
      if(is.na(temp[i,PoorMod])){
        temp[i,PoorMod]=(temp[i,BadPoor]+temp[i,ModGood])/2
      }      
      if(is.na(temp[i,GoodHigh])){
        temp[i,GoodHigh]=(temp[i,ModGood]+temp[i,High])/2
      }      
      
      
        #Check if values are increasing monotonically
        if (temp[i,Bad]<temp[i,BadPoor]&&temp[i,BadPoor]<temp[i,PoorMod]&&temp[i,PoorMod]<temp[i,ModGood]&&temp[i,ModGood]<temp[i,GoodHigh]&&temp[i,GoodHigh]<temp[i,High]){
          temp[i,'OK']<- 1
          temp[i,'Sign']<- 1
        }
        #Check if values are decreasing monotonically
        if (temp[i,Bad]>temp[i,BadPoor]&&temp[i,BadPoor]>temp[i,PoorMod]&&temp[i,PoorMod]>temp[i,ModGood]&&temp[i,ModGood]>temp[i,GoodHigh]&&temp[i,GoodHigh]>temp[i,High]){
          temp[i,'OK']<- 1
          temp[i,'Sign']<- -1
        }
        if(temp[i,'OK']==0){
          temp[i,'Err']<-"Boundary values not monotonic"
        }else{
          temp[i,"EQR"]<-EQR1(temp[i,'Sign'],temp[i,Obs],temp[i,Bad],temp[i,BadPoor],
                              temp[i,PoorMod],temp[i,ModGood],temp[i,GoodHigh],
                              temp[i,High])
          
        }
        
    }
  }
  
  
  return(temp)
}

EQR1<-function(sgn,obs,b0,b02,b04,b06,b08,b1){
  if(sgn>0){
    if(obs<b0) {n=0}
    else if (obs<b02){n=0.2*((obs-b0)/(b02-b0))}
    else if (obs<b04){n=0.2+0.2*((obs-b02)/(b04-b02))}
    else if (obs<b06){n=0.4+0.2*((obs-b04)/(b06-b04))}
    else if (obs<b08){n=0.6+0.2*((obs-b06)/(b08-b06))}
    else if (obs<b1){n=0.8+0.2*((obs-b08)/(b1-b08))}
    else{n=1}
  }else{
    if(obs>b0) {n=0}
    else if (obs>b02){n=0.2*((obs-b0)/(b02-b0))}
    else if (obs>b04){n=0.2+0.2*((obs-b02)/(b04-b02))}
    else if (obs>b06){n=0.4+0.2*((obs-b04)/(b06-b04))}
    else if (obs>b08){n=0.6+0.2*((obs-b06)/(b08-b06))}
    else if (obs>b1){n=0.8+0.2*((obs-b08)/(b1-b08))}
    else{n=1}
  }
  return(n)
}


EQRx<-function(sgn,b0,b1,b06,obs){
  if(sgn>0){
    if(obs<b0) {n=0}
    else if (obs<b06){n=0.6*((obs-b0)/(b06-b0))}
    else if (obs<b1){n=0.6+0.4*((obs-b06)/(b1-b06))}
    else{n=1}
  }else{
    if(obs>b0) {n=0}
    else if (obs>b06){n=0.6*((obs-b0)/(b06-b0))}
    else if (obs>b1){n=0.6+0.4*((obs-b06)/(b1-b06))}
    else{n=1}
  }
  return(n)
}

#===============================================================================
# function ContaminationRatio
ContaminationRatio<- function(threshold, status, response=1){
  # If response is not specified, it will be assumed to be positive
  # i.e. ContaminationRatio increases (worsens) with increasing status value
  if (missing(response)){
    response=1
  }
  response<-ifelse(is.na(response), 1, response)
  
  # ContaminationRatio calculated depending on Response direction
  cr<-ifelse(response>0, status/threshold, threshold/status)
  return(cr)
}

#===============================================================================
#Function CHASEStatus
EQRStatus<-function(eqr){
  status<-ifelse(eqr<0.8, "Good", "High")
  status<-ifelse(eqr<0.6, "Moderate", status)
  status<-ifelse(eqr<0.4, "Poor", status)
  status<-ifelse(eqr<0.2, "Bad",status )
  return(status)
}

AddColours<-function(CRsum){
  co<-ifelse(CRsum>0.5, '#66FF66', '#3399FF')
  co<-ifelse(CRsum>1, '#FFFF66', co)
  co<-ifelse(CRsum>5, '#FF9933', co)
  co<-ifelse(CRsum>10, '#FF6600',co)
  return(co)
}





