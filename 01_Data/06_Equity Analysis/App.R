###################### ITHIM application for Equity Analysis - Web Interface - Shiny App ######################

# read R scripts for physical activity module and traffic injury module
setwd("~/Documents/02_Work/14_GitHub/00_ITHIM/00_R scripts")
source('EquityAnalysis_ITHIM_PA.R')

setwd("~/Documents/02_Work/14_GitHub/00_ITHIM/00_R scripts")
source('EquityAnalysis_ITHIM_Injuries_TwoRaces.R')

# Parameter description
# countyID: 1-ELD; 2-PLA; 3-SAC; 4-SUT; 5-YOL; 6-YUB; 7-All
# barID: 1-future years,2-scenarios,3-customized
# outcomeID: 1-physical activity; 2-injury; 3-both
# demogrID: 1-Race/ethnicty; 2-household income
# yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std; 5-physical activity data

#plot.shiny.app.PA(countyID = 1,dbID = 1,typeID = 1,demogrID = 1,barID = 1)
#plot.shiny.app.injury(countyID = 1,barID = 1,yaxisID = 6)

# function for combining two moduels (PA and injury)
integrated.shiny.app <- function(countyID,barID,outcomeID,demogrID,yaxisID){
  if (outcomeID == 1){ #PA
    if (yaxisID == 1){ # death total
      plot.shiny.app.PA(countyID = countyID,dbID = 1,typeID = 1,demogrID = demogrID,barID = barID)
    }else if (yaxisID==2){ # death age.std
      plot.shiny.app.PA(countyID = countyID,dbID = 1,typeID = 2,demogrID = demogrID,barID = barID)
    }else if (yaxisID==3){
      plot.shiny.app.PA(countyID = countyID,dbID = 2,typeID = 1,demogrID = demogrID,barID = barID)
    }else if (yaxisID==4){
      plot.shiny.app.PA(countyID = countyID,dbID = 2,typeID = 2,demogrID = demogrID,barID = barID)
    }else if (yaxisID==5){
      plot.shiny.app.PA(countyID = countyID,dbID = 1,typeID = 3,demogrID = demogrID,barID = barID)
    }else{
      message('wrong input')
    }
    
  }else if (outcomeID==2){ #injury
    if (yaxisID%in%c(1:4)){
      plot.shiny.app.injury(countyID = countyID,barID = barID,yaxisID = yaxisID)
    }else{
      message('wrong input')
    }
    
    
  }else if (outcomeID==3){ # both
    if (countyID%in%(1:6)){
      
      value <- NULL

      if (yaxisID==1){ #total deaths
        #test
        #countyID=1
        #barID = 1
        
        df.result.PA <- DFforFigure(RawReductionOutcome[c((1*18+1*9-26):(1*18+1*9-18))],1,countyID,barID)
        df.result.PA.aggr.white <- df.result.PA[c(1,5,9),]
        for (i in 1:3) {
          value[i] <- sum(df.result.PA[((i+1):(i+3)),3])
        }
        df.result.PA.aggr.other <- data.frame(Scenario=unique(df.result.PA[,1]),DemogrGroup=rep('2.Other',3),V1 =(value))
        
        df.result.PA.aggr <- rbind(df.result.PA.aggr.white,df.result.PA.aggr.other)
        df.result.PA.aggr <- df.result.PA.aggr[order(df.result.PA.aggr$Scenario),]
        df.result.PA.aggr$type <- 'a.physical activity'
        
        df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 1)
        df.result.injury <- df.result.injury$df.fatality
        df.result.injury$type <- 'b.traffic injury'
        
        df.result.integration.temp <- df.result.PA.aggr[,1:2]
        df.result.integration.temp$V1 <- df.result.PA.aggr$V1+df.result.injury$V1
        df.result.integration.temp$type <- 'c. both'
        
        df.result.integration <- rbind(df.result.PA.aggr,df.result.injury,df.result.integration.temp)
        
        plot.title <- paste0(countyNames[countyID],': Reduction in Total Deaths')
        
        # ggplot(data = df.result.integration,mapping = aes(x = factor(DemogrGroup), y = V1,color = factor(Scenario),shape = factor(type)))+
        #    geom_point(stat = 'identity',size=3,position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Total Death Reduction')+
        #    ggtitle(plot.title)
        # 
        # ggplot(data = df.result.integration,mapping = aes(x = factor(DemogrGroup), y = V1,color = factor(Scenario),shape = factor(type)))+
        #   geom_point(stat = 'identity',size=3,position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Total Death Reduction')+
        #   ggtitle(plot.title)
        
        # ggplot(data = df.result.integration,mapping = aes(x = factor(DemogrGroup), y = V1,color = factor(Scenario)))+
        #   geom_dotplot(binaxis = "y",position = 'dodge',binwidth = 0.1)+xlab('Demographic Group')+ylab('Total Death Reduction')+
        #   ggtitle(plot.title)
        
        
        ggplot(data = df.result.integration, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
          geom_bar(stat = 'identity',position = position_dodge(0.5),width = 0.5)+xlab('Demographic Group')+ylab('Total Death Reduction')+
          geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
          facet_grid(.~type,scales = "free")+ggtitle(plot.title)
        
        
      }else if (yaxisID==3){ #total DALYs
        #test
        #countyID=1
        #barID = 1
        
        df.result.PA <- DFforFigure(RawReductionOutcome[c((1*18+2*9-26):(1*18+2*9-18))],1,countyID,barID)
        
        df.result.PA.aggr.white <- df.result.PA[c(1,5,9),]
        for (i in 1:3) {
          value[i] <- sum(df.result.PA[((i+1):(i+3)),3])
        }
        df.result.PA.aggr.other <- data.frame(Scenario=unique(df.result.PA[,1]),DemogrGroup=rep('2.Other',3),V1 =(value))
        
        df.result.PA.aggr <- rbind(df.result.PA.aggr.white,df.result.PA.aggr.other)
        df.result.PA.aggr <- df.result.PA.aggr[order(df.result.PA.aggr$Scenario),]
        df.result.PA.aggr$type <- 'a. physical activity'
        
        df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 1)
        df.result.injury <- df.result.injury$df.DALYs
        df.result.injury$type <- 'b. traffic injury'
        
        df.result.integration.temp <- df.result.PA.aggr[,1:2]
        df.result.integration.temp$V1 <- df.result.PA.aggr$V1+df.result.injury$V1
        df.result.integration.temp$type <- 'c. both'
        
        df.result.integration <- rbind(df.result.PA.aggr,df.result.injury,df.result.integration.temp)
        
        plot.title <- paste0(countyNames[countyID],': Reduction in Total DALYs')
        
        ggplot(data = df.result.integration, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
          geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Total DALYs Reduction')+
          geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
          facet_grid(.~type,scales = "free")+ggtitle(plot.title)
      }else if (yaxisID==2){ # age.std deaths
        #test
        #countyID=1
        #barID = 1
        
        df.result.PA <- DFforFigure(AgeStdReductionOutcome[c((1*18+1*9-26):(1*18+1*9-18))],1,countyID,barID)
        
        df.result.PA.aggr.white <- df.result.PA[c(1,5,9),]
        for (i in 1:3) {
          value[i] <- sum(df.result.PA[((i+1):(i+3)),3])
        }
        df.result.PA.aggr.other <- data.frame(Scenario=unique(df.result.PA[,1]),DemogrGroup=rep('2.Other',3),V1 =(value))
        
        df.result.PA.aggr <- rbind(df.result.PA.aggr.white,df.result.PA.aggr.other)
        df.result.PA.aggr <- df.result.PA.aggr[order(df.result.PA.aggr$Scenario),]
        df.result.PA.aggr$type <- 'a. physical activity'
        
        df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 2)
        df.result.injury <- df.result.injury$df.fatality
        df.result.injury$type <- 'b. traffic injury'
        
        
        df.result.integration.temp <- df.result.PA.aggr[,1:2]
        df.result.integration.temp$V1 <- df.result.PA.aggr$V1+df.result.injury$V1
        df.result.integration.temp$type <- 'c. both'
        
        df.result.integration <- rbind(df.result.PA.aggr,df.result.injury,df.result.integration.temp)
        
        plot.title <- paste0(countyNames[countyID],': Age-Standardized Reduction in Total Deaths')
        
        ggplot(data = df.result.integration, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
          geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Death reduction rate (per 100,000 population)')+
          geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
          facet_grid(.~type,scales = "free")+ggtitle(plot.title)
        
        
      }else if (yaxisID==4){#age.std dalys
        
        df.result.PA <- DFforFigure(AgeStdReductionOutcome[c((1*18+2*9-26):(1*18+2*9-18))],1,countyID,barID)
        
        df.result.PA.aggr.white <- df.result.PA[c(1,5,9),]
        for (i in 1:3) {
          value[i] <- sum(df.result.PA[((i+1):(i+3)),3])
        }
        df.result.PA.aggr.other <- data.frame(Scenario=unique(df.result.PA[,1]),DemogrGroup=rep('2.Other',3),V1 =(value))
        
        df.result.PA.aggr <- rbind(df.result.PA.aggr.white,df.result.PA.aggr.other)
        df.result.PA.aggr <- df.result.PA.aggr[order(df.result.PA.aggr$Scenario),]
        df.result.PA.aggr$type <- 'a. physical activity'
        
        df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 2)
        df.result.injury <- df.result.injury$df.DALYs
        df.result.injury$type <- 'b. traffic injury'
        
        plot.title <- paste0(countyNames[countyID],': Age-Standardized Reduction in Total DALYs')
        
        df.result.integration.temp <- df.result.PA.aggr[,1:2]
        df.result.integration.temp$V1 <- df.result.PA.aggr$V1+df.result.injury$V1
        df.result.integration.temp$type <- 'c. both'
        
        df.result.integration <- rbind(df.result.PA.aggr,df.result.injury,df.result.integration.temp)
        
        ggplot(data = df.result.integration, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
          geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('DALYs reduction rate (per 100,000 population)')+
          geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
          facet_grid(.~type,scales = "free")+ggtitle(plot.title)
        
      }
    }else if(countyID==7){ #region wide
      #test
      #yaxisID=1
      #barID=1
      
      df.region <- NULL
      
      if (yaxisID==1){ #death total
        
        for (countyID in c(1:6)){
          
          value <- NULL
          
          df.result.PA <- DFforFigure(RawReductionOutcome[c((1*18+1*9-26):(1*18+1*9-18))],1,countyID,barID)
          df.result.PA.aggr.white <- df.result.PA[c(1,5,9),]
          for (i in 1:3) {
            value[i] <- sum(df.result.PA[((i+1):(i+3)),3])
          }
          df.result.PA.aggr.other <- data.frame(Scenario=unique(df.result.PA[,1]),DemogrGroup=rep('2.Other',3),V1 =(value))
          
          df.result.PA.aggr <- rbind(df.result.PA.aggr.white,df.result.PA.aggr.other)
          df.result.PA.aggr <- df.result.PA.aggr[order(df.result.PA.aggr$Scenario),]
          df.result.PA.aggr$type <- 'a. physical activity'
          
          df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 1)
          df.result.injury <- df.result.injury$df.fatality
          df.result.injury$type <- 'b. traffic injury'
          
          df.result.integration.temp <- df.result.PA.aggr[,1:2]
          df.result.integration.temp$V1 <- df.result.PA.aggr$V1+df.result.injury$V1
          #df.result.integration.temp$type <- 'integration'
          
          #df.result.integration <- rbind(df.result.PA.aggr,df.result.injury,df.result.integration.temp)
          
          df.region <- rbind(df.region,df.result.integration.temp)
          
          
        }
        
        df.region$county <- rep(countyNames,each = 6)
        
        ggplot(data = df.region, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
          geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Total Death Reduction')+
          geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
          facet_wrap(~county,scales = "free") +ggtitle("Region-Wide: Reduction of Total Deaths ")
        
        #return(df.region = df.region)
      
      }else if (yaxisID==2){#death age.std
        
        for (countyID in c(1:6)){
          value <- NULL
          
          df.result.PA <- DFforFigure(AgeStdReductionOutcome[c((1*18+2*9-26):(1*18+2*9-18))],1,countyID,barID)
  
          df.result.PA.aggr.white <- df.result.PA[c(1,5,9),]
          for (i in 1:3) {
            value[i] <- sum(df.result.PA[((i+1):(i+3)),3])
          }
          df.result.PA.aggr.other <- data.frame(Scenario=unique(df.result.PA[,1]),DemogrGroup=rep('2.Other',3),V1 =(value))
          
          df.result.PA.aggr <- rbind(df.result.PA.aggr.white,df.result.PA.aggr.other)
          df.result.PA.aggr <- df.result.PA.aggr[order(df.result.PA.aggr$Scenario),]
          df.result.PA.aggr$type <- 'a. physical activity'
          
          df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 2)
          df.result.injury <- df.result.injury$df.fatality
          df.result.injury$type <- 'b. traffic injury'
          
          
          df.result.integration.temp <- df.result.PA.aggr[,1:2]
          df.result.integration.temp$V1 <- df.result.PA.aggr$V1+df.result.injury$V1
          #df.result.integration.temp$type <- 'integration'
          
          df.region <- rbind(df.region,df.result.integration.temp)
        }
        
        
        df.region$county <- rep(countyNames,each = 6)
        
        ggplot(data = df.region, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
          geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Death reduction rate (per 100,000 population)')+
          geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
          facet_wrap(~county,scales = "free") +ggtitle("Region-Wide: Age-Standardized Reduction in Total Deaths")
        
        #return(df.region = df.region)
        
      }else if (yaxisID==3){# total DALYs
        
        for(countyID in 1:6){
          
          df.result.PA <- DFforFigure(RawReductionOutcome[c((1*18+2*9-26):(1*18+2*9-18))],1,countyID,barID)
          
          df.result.PA.aggr.white <- df.result.PA[c(1,5,9),]
          for (i in 1:3) {
            value[i] <- sum(df.result.PA[((i+1):(i+3)),3])
          }
          df.result.PA.aggr.other <- data.frame(Scenario=unique(df.result.PA[,1]),DemogrGroup=rep('2.Other',3),V1 =(value))
          
          df.result.PA.aggr <- rbind(df.result.PA.aggr.white,df.result.PA.aggr.other)
          df.result.PA.aggr <- df.result.PA.aggr[order(df.result.PA.aggr$Scenario),]
          df.result.PA.aggr$type <- 'a. physical activity'
          
          df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 1)
          df.result.injury <- df.result.injury$df.DALYs
          df.result.injury$type <- 'b. traffic injury'
          
          df.result.integration.temp <- df.result.PA.aggr[,1:2]
          df.result.integration.temp$V1 <- df.result.PA.aggr$V1+df.result.injury$V1
          #df.result.integration.temp$type <- 'integration'
          
          df.region <- rbind(df.region,df.result.integration.temp)
        }
        
        df.region$county <- rep(countyNames,each = 6)
        
        ggplot(data = df.region, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
          geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Total DALYs Reduction')+
          geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
          facet_wrap(~county,scales = "free") +ggtitle("Region-Wide: Reduction of Total DALYs ")
        
        #return(df.region = df.region)
        
      }else if (yaxisID==4){#age.std DALYs
        
        for (countyID in 1:6){
          df.result.PA <- DFforFigure(AgeStdReductionOutcome[c((1*18+2*9-26):(1*18+2*9-18))],1,countyID,barID)
          
          df.result.PA.aggr.white <- df.result.PA[c(1,5,9),]
          for (i in 1:3) {
            value[i] <- sum(df.result.PA[((i+1):(i+3)),3])
          }
          df.result.PA.aggr.other <- data.frame(Scenario=unique(df.result.PA[,1]),DemogrGroup=rep('2.Other',3),V1 =(value))
          
          df.result.PA.aggr <- rbind(df.result.PA.aggr.white,df.result.PA.aggr.other)
          df.result.PA.aggr <- df.result.PA.aggr[order(df.result.PA.aggr$Scenario),]
          df.result.PA.aggr$type <- 'a. physical activity'
          
          df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 2)
          df.result.injury <- df.result.injury$df.DALYs
          df.result.injury$type <- 'b. traffic injury'
          
          #plot.title <- paste0(countyNames[countyID],': Age-Standardized Reduction in Total DALYs')
          
          df.result.integration.temp <- df.result.PA.aggr[,1:2]
          df.result.integration.temp$V1 <- df.result.PA.aggr$V1+df.result.injury$V1
          
          df.region <- rbind(df.region,df.result.integration.temp)
        }
        
        df.region$county <- rep(countyNames,each = 6)
        
        ggplot(data = df.region, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
          geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('DALYs reduction rate (per 100,000 population)')+
          geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
          facet_wrap(~county,scales = "free") +ggtitle("Region-Wide: Age-Standardized Reduction in Total DALYs")
        
        #return(df.region = df.region)
        
      }else{
        message('wrong input')
      }
    

    }
    
  }
}

# Parameter description
# countyID: 1-ELD; 2-PLA; 3-SAC; 4-SUT; 5-YOL; 6-YUB; 7-All
# barID: 1-future years,2-scenarios,3-customized
# outcomeID: 1-physical activity; 2-injury; 3-both
# demogrID: 1-Race/ethnicty; 2-household income
# yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std; 5-physical activity data


aggr.outcome.shiny.app <- function(barID,yaxisID){
  
  #TEST
  #barID =1
  #yaxisID=1
  
  #demogr.name <- c('white','others')
  
  if(barID==1){
    scenario.name.sep <- c('2020','2027','2036')
    scenairo.name<-rep(scenario.name.sep,6)
  }else if(barID==2){
    scenario.name.sep <- c('S1','S2','S3')
    scenairo.name<-rep(scenario.name.sep,6)
  }
  
  if (yaxisID==1){#death total
    # PA module
    value<-NULL
    
    PA.disaggr <- DFforRegionWide(ReductionOutcome = RawReductionOutcome,demogrID = 1,dbID = 1,barID = barID)
    
    PA.disaggr.white <- PA.disaggr[seq(from=1,to=69,by=4),]
    
    for (i in 1:3){
      value[i] <-sum(PA.disaggr.white[c(i,i+3,i+6,i+9,i+12,i+15),3]) 
    }
    PA.disaggr.white.temp <- data.frame(Scenario = scenario.name.sep,DemogrGroup = '1. white',V1=value)
    
    value<-value.temp<-NULL
    for (i in 1:18){
      #value[i]<-sum(PA.disaggr[(4*i-3):(4*i),3])
      value[i] <- sum(PA.disaggr[(4*i-2):(4*i),3])
    }
    for (j in 1:3){
      value.temp[j]<-sum(value[c(j,j+3,j+6,j+9,j+12,j+15)])
    }
    PA.disaggr.other.temp <- data.frame(Scenario = scenario.name.sep,DemogrGroup = '2. other',V1=value.temp)
    
    df.PA.aggr <- rbind(PA.disaggr.white.temp,PA.disaggr.other.temp)
    df.PA.aggr <- df.PA.aggr[order(df.PA.aggr$Scenario),]
    df.PA.aggr$type <- 'a. physical activity'
    
    #df.PA.aggr.temp <- data.frame(Scenario = scenairo.name,county = rep(countyNames,each=3),V1=value,type='physical activity')
    # value <- NULL
    # for (i in 1:3){
    #   value[i]<-sum(df.PA.aggr.temp[c(i,i+3,i+6,i+9,i+12,i+15),3])
    # }
    # 
    # df.PA.aggr <- data.frame(Scenario = scenario.name.sep,type = 'a. physical activity',V1=value)
    
    ####injury module
    df.injury.region <- NULL
    
    for (i in 1:6){ #county
      #value <-NULL
      
      df.temp <- DFforFigure.injury(barID = barID,i,typeID = 1)
      
      #for (j in 1:3){
       # value[j] <- sum(df.temp$df.fatality[(2*j-1):(2*j),3]) 
      #}
      
      df.injury.aggr.temp <- df.temp$df.fatality
      
      df.injury.region <- rbind(df.injury.region,df.injury.aggr.temp)
      
    }
    
    df.injury.region<-data.frame(df.injury.region,county = rep(countyNames,each=6))
    #df.injury.region<-data.frame(Scenario = scenairo.name,df.injury.region)
    
    value <- NULL
    for (i in 1:6){
      value[i]<-sum(df.injury.region[c(i,i+6,i+12,i+18,i+24,i+30),3])
    }
    
    df.injury.aggr <- data.frame(Scenario = rep(scenario.name.sep,each = 2),DemogrGroup = rep(c('1. white','2. other'),3),V1=value, type = 'b. traffic injury')
    
    
    # sum of two module
    df.result.integration.temp <- df.PA.aggr[,1:2]
    df.result.integration.temp$V1 <- df.PA.aggr$V1+df.injury.aggr$V1
    df.result.integration.temp$type <- 'c. both'
    
    df.integration.aggr <- rbind(df.PA.aggr,df.injury.aggr,df.result.integration.temp)
    
    ggplot(data = df.integration.aggr, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
      geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demograhic Group')+ylab('Total Deaths Reduction')+
      geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
      facet_grid(.~type,scales = "free")+ggtitle("Region-Wide: Reduction in Total Deaths")
    
    
    
  }else if (yaxisID==2){#death age.std
    # PA module
    value<-NULL
    
    PA.disaggr <- DFforRegionWide(ReductionOutcome = AgeStdReductionOutcome,demogrID = 1,dbID = 1,barID = barID)
    
    PA.disaggr.white <- PA.disaggr[seq(from=1,to=69,by=4),]
    
    for (i in 1:3){
      value[i] <-sum(PA.disaggr.white[c(i,i+3,i+6,i+9,i+12,i+15),3]) 
    }
    PA.disaggr.white.temp <- data.frame(Scenario = scenario.name.sep,DemogrGroup = '1. white',V1=value)
    
    value<-value.temp<-NULL
    for (i in 1:18){
      #value[i]<-sum(PA.disaggr[(4*i-3):(4*i),3])
      value[i] <- sum(PA.disaggr[(4*i-2):(4*i),3])
    }
    for (j in 1:3){
      value.temp[j]<-sum(value[c(j,j+3,j+6,j+9,j+12,j+15)])
    }
    PA.disaggr.other.temp <- data.frame(Scenario = scenario.name.sep,DemogrGroup = '2. other',V1=value.temp)
    
    df.PA.aggr <- rbind(PA.disaggr.white.temp,PA.disaggr.other.temp)
    df.PA.aggr <- df.PA.aggr[order(df.PA.aggr$Scenario),]
    df.PA.aggr$type <- 'a. physical activity'
    
    #df.PA.aggr.temp <- data.frame(Scenario = scenairo.name,county = rep(countyNames,each=3),V1=value,type='physical activity')
    # value <- NULL
    # for (i in 1:3){
    #   value[i]<-sum(df.PA.aggr.temp[c(i,i+3,i+6,i+9,i+12,i+15),3])
    # }
    # 
    # df.PA.aggr <- data.frame(Scenario = scenario.name.sep,type = 'a. physical activity',V1=value)
    
    ####injury module
    df.injury.region <- NULL
    
    for (i in 1:6){ #county
      #value <-NULL
      
      df.temp <- DFforFigure.injury(barID = barID,i,typeID = 2)
      
      #for (j in 1:3){
      # value[j] <- sum(df.temp$df.fatality[(2*j-1):(2*j),3]) 
      #}
      
      df.injury.aggr.temp <- df.temp$df.fatality
      
      df.injury.region <- rbind(df.injury.region,df.injury.aggr.temp)
      
    }
    
    df.injury.region<-data.frame(df.injury.region,county = rep(countyNames,each=6))
    #df.injury.region<-data.frame(Scenario = scenairo.name,df.injury.region)
    
    value <- NULL
    for (i in 1:6){
      value[i]<-sum(df.injury.region[c(i,i+6,i+12,i+18,i+24,i+30),3])
    }
    
    df.injury.aggr <- data.frame(Scenario = rep(scenario.name.sep,each = 2),DemogrGroup = rep(c('1. white','2. other'),3),V1=value, type = 'b. traffic injury')
    
    
    # sum of two module
    df.result.integration.temp <- df.PA.aggr[,1:2]
    df.result.integration.temp$V1 <- df.PA.aggr$V1+df.injury.aggr$V1
    df.result.integration.temp$type <- 'c. both'
    
    df.integration.aggr <- rbind(df.PA.aggr,df.injury.aggr,df.result.integration.temp)
    
    ggplot(data = df.integration.aggr, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
      geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demograhic Group')+ylab('Deaths reduction rate (per 100,000 population)')+
      geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
      facet_grid(.~type,scales = "free")+ggtitle("Region-Wide: Age-Standardized Reduction in Total Deaths")
    
  }else if(yaxisID==3){# DALYs total
    # PA module
    value<-NULL
    
    PA.disaggr <- DFforRegionWide(ReductionOutcome = RawReductionOutcome,demogrID = 1,dbID = 2,barID = barID)
    
    PA.disaggr.white <- PA.disaggr[seq(from=1,to=69,by=4),]
    
    for (i in 1:3){
      value[i] <-sum(PA.disaggr.white[c(i,i+3,i+6,i+9,i+12,i+15),3]) 
    }
    PA.disaggr.white.temp <- data.frame(Scenario = scenario.name.sep,DemogrGroup = '1. white',V1=value)
    
    value<-value.temp<-NULL
    for (i in 1:18){
      #value[i]<-sum(PA.disaggr[(4*i-3):(4*i),3])
      value[i] <- sum(PA.disaggr[(4*i-2):(4*i),3])
    }
    for (j in 1:3){
      value.temp[j]<-sum(value[c(j,j+3,j+6,j+9,j+12,j+15)])
    }
    PA.disaggr.other.temp <- data.frame(Scenario = scenario.name.sep,DemogrGroup = '2. other',V1=value.temp)
    
    df.PA.aggr <- rbind(PA.disaggr.white.temp,PA.disaggr.other.temp)
    df.PA.aggr <- df.PA.aggr[order(df.PA.aggr$Scenario),]
    df.PA.aggr$type <- 'a. physical activity'
    
    #df.PA.aggr.temp <- data.frame(Scenario = scenairo.name,county = rep(countyNames,each=3),V1=value,type='physical activity')
    # value <- NULL
    # for (i in 1:3){
    #   value[i]<-sum(df.PA.aggr.temp[c(i,i+3,i+6,i+9,i+12,i+15),3])
    # }
    # 
    # df.PA.aggr <- data.frame(Scenario = scenario.name.sep,type = 'a. physical activity',V1=value)
    
    ####injury module
    df.injury.region <- NULL
    
    for (i in 1:6){ #county
      #value <-NULL
      
      df.temp <- DFforFigure.injury(barID = barID,i,typeID = 1)
      
      #for (j in 1:3){
      # value[j] <- sum(df.temp$df.fatality[(2*j-1):(2*j),3]) 
      #}
      
      df.injury.aggr.temp <- df.temp$df.DALYs
      
      df.injury.region <- rbind(df.injury.region,df.injury.aggr.temp)
      
    }
    
    df.injury.region<-data.frame(df.injury.region,county = rep(countyNames,each=6))
    #df.injury.region<-data.frame(Scenario = scenairo.name,df.injury.region)
    
    value <- NULL
    for (i in 1:6){
      value[i]<-sum(df.injury.region[c(i,i+6,i+12,i+18,i+24,i+30),3])
    }
    
    df.injury.aggr <- data.frame(Scenario = rep(scenario.name.sep,each = 2),DemogrGroup = rep(c('1. white','2. other'),3),V1=value, type = 'b. traffic injury')
    
    
    # sum of two module
    df.result.integration.temp <- df.PA.aggr[,1:2]
    df.result.integration.temp$V1 <- df.PA.aggr$V1+df.injury.aggr$V1
    df.result.integration.temp$type <- 'c. both'
    
    df.integration.aggr <- rbind(df.PA.aggr,df.injury.aggr,df.result.integration.temp)
    
    ggplot(data = df.integration.aggr, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
      geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Total DALYs Reduction')+
      geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
      facet_grid(.~type,scales = "free")+ggtitle("Region-Wide: Reduction in Total DALYs")
    
  }else if (yaxisID==4){#DALYs age.std
    # PA module
    value<-NULL
    
    PA.disaggr <- DFforRegionWide(ReductionOutcome = AgeStdReductionOutcome,demogrID = 1,dbID = 2,barID = barID)
    
    PA.disaggr.white <- PA.disaggr[seq(from=1,to=69,by=4),]
    
    for (i in 1:3){
      value[i] <-sum(PA.disaggr.white[c(i,i+3,i+6,i+9,i+12,i+15),3]) 
    }
    PA.disaggr.white.temp <- data.frame(Scenario = scenario.name.sep,DemogrGroup = '1. white',V1=value)
    
    value<-value.temp<-NULL
    for (i in 1:18){
      #value[i]<-sum(PA.disaggr[(4*i-3):(4*i),3])
      value[i] <- sum(PA.disaggr[(4*i-2):(4*i),3])
    }
    for (j in 1:3){
      value.temp[j]<-sum(value[c(j,j+3,j+6,j+9,j+12,j+15)])
    }
    PA.disaggr.other.temp <- data.frame(Scenario = scenario.name.sep,DemogrGroup = '2. other',V1=value.temp)
    
    df.PA.aggr <- rbind(PA.disaggr.white.temp,PA.disaggr.other.temp)
    df.PA.aggr <- df.PA.aggr[order(df.PA.aggr$Scenario),]
    df.PA.aggr$type <- 'a. physical activity'
    
    #df.PA.aggr.temp <- data.frame(Scenario = scenairo.name,county = rep(countyNames,each=3),V1=value,type='physical activity')
    # value <- NULL
    # for (i in 1:3){
    #   value[i]<-sum(df.PA.aggr.temp[c(i,i+3,i+6,i+9,i+12,i+15),3])
    # }
    # 
    # df.PA.aggr <- data.frame(Scenario = scenario.name.sep,type = 'a. physical activity',V1=value)
    
    ####injury module
    df.injury.region <- NULL
    
    for (i in 1:6){ #county
      #value <-NULL
      
      df.temp <- DFforFigure.injury(barID = barID,i,typeID = 2)
      
      #for (j in 1:3){
      # value[j] <- sum(df.temp$df.fatality[(2*j-1):(2*j),3]) 
      #}
      
      df.injury.aggr.temp <- df.temp$df.DALYs
      
      df.injury.region <- rbind(df.injury.region,df.injury.aggr.temp)
      
    }
    
    df.injury.region<-data.frame(df.injury.region,county = rep(countyNames,each=6))
    #df.injury.region<-data.frame(Scenario = scenairo.name,df.injury.region)
    
    value <- NULL
    for (i in 1:6){
      value[i]<-sum(df.injury.region[c(i,i+6,i+12,i+18,i+24,i+30),3])
    }
    
    df.injury.aggr <- data.frame(Scenario = rep(scenario.name.sep,each = 2),DemogrGroup = rep(c('1. white','2. other'),3),V1=value, type = 'b. traffic injury')
    
    
    # sum of two module
    df.result.integration.temp <- df.PA.aggr[,1:2]
    df.result.integration.temp$V1 <- df.PA.aggr$V1+df.injury.aggr$V1
    df.result.integration.temp$type <- 'c. both'
    
    df.integration.aggr <- rbind(df.PA.aggr,df.injury.aggr,df.result.integration.temp)
    
    ggplot(data = df.integration.aggr, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
      geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Groups')+ylab('DALYs reduction rate (per 100,000 population)')+
      geom_text(aes(label=round(V1,1)),color="black",size=3.5,vjust=-0.5,position = position_dodge(0.5))+
      facet_grid(.~type,scales = "free")+ggtitle("Region-Wide: Age-Standardized Reduction in Total DALYs")
  }else{
    message('wrong input')
  }
  
  
  
  }

# Parameter description
# countyID: 1-ELD; 2-PLA; 3-SAC; 4-SUT; 5-YOL; 6-YUB; 7-All
# barID: 1-future years,2-scenarios,3-customized
# outcomeID: 1-physical activity; 2-injury; 3-both
# demogrID: 1-Race/ethnicty; 2-household income
# yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std; 5-physical activity data
integrated.shiny.app(countyID = 2, barID = 3,outcomeID = 2,demogrID = 1, yaxisID =1)



# Parameter description
# barID: 1-future years,2-scenarios
# yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std
aggr.outcome.shiny.app(barID = 1,yaxisID=1)


###################### ITHIM application for Equity Analysis - Web Interface - Shiny App - Server/UI ######################


setwd("~/Documents/02_Work/14_GitHub/00_ITHIM/01_Data/06_Equity Analysis")

require(shiny)

ui <- fluidPage(
  titlePanel("ITHIM APP"),  
  navbarPage("ITHIM APP",
             
             # Pulls About page from Markdown File
             tabPanel("About",
                      fluidRow(
                        column(6, 
                               includeHTML("ITHIM_About.html")
                        )
                      )           
             ),
             # Creates simple aggregated plot
             tabPanel("Simple Aggregated Plots",
                      sidebarLayout(
                        # Creates sidebar with Radio buttons
                        sidebarPanel(
                          # Parameter description
                          # barID: 1-future years,2-scenarios
                          # yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std
                          radioButtons("selectbarID", label = h3("Select Scenario"), 
                                       choices = list("Future Years" = 1, "Scenarios" = 2), 
                                       selected = 1),
                          radioButtons("selectyaxisID", label = h3("Select Units"), 
                                       choices = list("Deaths - [Total]" = 1, "Death - [Age Standardized]" = 2, 
                                                      "Disability Adjusted Life Years (DALYs) - [Total]" = 3, "DALYs - [Age Standardized]" = 4
                                       ), 
                                       selected = 1)
                        ),
                        mainPanel(
                          plotOutput("SimplePlot")
                        )
                      )
             ),
             
             tabPanel("Advanced Plots",
                      sidebarLayout(
                        sidebarPanel(
                          # Parameter description
                          # countyID: 1-ELD; 2-PLA; 3-SAC; 4-SUT; 5-YOL; 6-YUB; 7-All
                          # barID: 1-future years,2-scenarios,3-customized
                          # outcomeID: 1-physical activity; 2-injury; 3-both
                          # demogrID: 1-Race/ethnicty; 2-household income
                          # yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std; 5-physical activity data
                          radioButtons("selectCounty", label = h3("Select County"), 
                                       choices = list("El Dorado" = 1, "Placer" = 2, "Sacramento" = 3, "Sutter"= 4, "Yolo"= 5, "Yuba"= 6, "All"= 7), 
                                       selected = 1),
                          radioButtons("selectbarID", label = h3("Select Scenario"), 
                                       choices = list("Future Years" = 1, "Scenarios" = 2, "Customized" = 3), 
                                       selected = 1),
                          radioButtons("selectoutcomeID", label = h3("Select Outcome"), 
                                       choices = list("Physical Activity" = 1, "Injury" = 2, "Both" = 3), 
                                       selected = 1),
                          radioButtons("selectdemogrID", label = h3("Select Demographic"), 
                                       choices = list("Race/Ethnicity" = 1, "Household Income" = 2), 
                                       selected = 1),
                          radioButtons("selectyaxisID", label = h3("Select Units"), 
                                       choices = list("Deaths - [Total]" = 1, "Death - [Age Standardized]" = 2, 
                                                      "Disability Adjusted Life Years (DALYs) - [Total]" = 3, "DALYs - [Age Standardized]" = 4, 
                                                      "Physical Activity Data" = 5), 
                                       selected = 1)
                          # sliderInput(inputId = "mwt",
                          #             label = "Mean Walking Time (min per week)",
                          #             value = 47.49, min = 20, max = 100),
                        ),
                        mainPanel(
                          plotOutput("AdvancedPlot")
                        )
                      )
             ),
             #Upload Panel from http://shiny.rstudio.com/gallery/upload-file.html
             # 01_Data/EQ/ActiveTransport/c1-c2-c3
             # 01_Data/EQ/PVD/c1-2-3
             
             tabPanel("Custom Scenarios", 
                      
                      sidebarLayout(
                        sidebarPanel(
                          fileInput('file1', 'Choose file to upload',
                                    accept = c(
                                      '.zip'
                                    )
                          ),
                          tags$hr(),
                          p('Please use the following as a template for the data structure of your custom scenarios',
                            a(href = "CustomScenarioTemplate.zip", "CustomScenarioTemplate.zip")
                          ),
                          radioButtons("selectCounty", label = h3("Select County"), 
                                       choices = list("El Dorado" = 1, "Placer" = 2, "Sacramento" = 3, "Sutter"= 4, "Yolo"= 5, "Yuba"= 6, "All"= 7), 
                                       selected = 1),
                          radioButtons("selectbarID", label = h3("Select Scenario"), 
                                       choices = list("Future Years" = 1, "Scenarios" = 2, "Customized" = 3), 
                                       selected = 1),
                          radioButtons("selectoutcomeID", label = h3("Select Outcome"), 
                                       choices = list("Physical Activity" = 1, "Injury" = 2, "Both" = 3), 
                                       selected = 1),
                          radioButtons("selectdemogrID", label = h3("Select Demographic"), 
                                       choices = list("Race/Ethnicity" = 1, "Household Income" = 2), 
                                       selected = 1),
                          radioButtons("selectyaxisID", label = h3("Select Units"), 
                                       choices = list("Deaths - [Total]" = 1, "Death - [Age Standardized]" = 2, 
                                                      "Disability Adjusted Life Years (DALYs) - [Total]" = 3, "DALYs - [Age Standardized]" = 4, 
                                                      "Physical Activity Data" = 5), 
                                       selected = 1)
                        ),
                        mainPanel(
                          plotOutput("CustomizablePlot")
                        )
                        
                      )
             )
  )
)

server <- function(input, output) {
  
  # data <- reactive({
  #       (input$select)
  #     })
  
  output$CustomizablePlot <- renderPlot({
    
    # Parameter description
    # countyID: 1-ELD; 2-PLA; 3-SAC; 4-SUT; 5-YOL; 6-YUB; 7-All
    # barID: 1-future years,2-scenarios,3-customized
    # outcomeID: 1-physical activity; 2-injury; 3-both
    # demogrID: 1-Race/ethnicty; 2-household income
    # yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std; 5-physical activity data
    integrated.shiny.app(countyID = as.integer(input$selectCounty), barID = as.integer(input$selectbarID),
                         outcomeID = as.integer(input$selectoutcomeID),demogrID = as.integer(input$selectdemogrID), 
                         yaxisID = as.integer(input$selectyaxisID))
  })
  
  output$AdvancedPlot <- renderPlot({
    
    # Parameter description
    # countyID: 1-ELD; 2-PLA; 3-SAC; 4-SUT; 5-YOL; 6-YUB; 7-All
    # barID: 1-future years,2-scenarios,3-customized
    # outcomeID: 1-physical activity; 2-injury; 3-both
    # demogrID: 1-Race/ethnicty; 2-household income
    # yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std; 5-physical activity data
    integrated.shiny.app(countyID = as.integer(input$selectCounty), barID = as.integer(input$selectbarID),
                         outcomeID = as.integer(input$selectoutcomeID),demogrID = as.integer(input$selectdemogrID), 
                         yaxisID = as.integer(input$selectyaxisID))
  })
  output$SimplePlot <- renderPlot({
    # Parameter description
    # barID: 1-future years,2-scenarios
    # yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std
    aggr.outcome.shiny.app(barID = as.integer(input$selectbarID),yaxisID = as.integer(input$selectyaxisID))
  })
  
  
}

shinyApp(ui = ui, server = server)



