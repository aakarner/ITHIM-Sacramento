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

integrated.shiny.app <- function(countyID,barID,outcomeID,demogrID,yaxisID){
  if (outcomeID == 1){
    if (yaxisID == 1){
      plot.shiny.app.PA(countyID = countyID,dbID = 1,typeID = 1,demogrID = demogrID,barID = barID)
    }else if (yaxisID==2){
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
    
  }else if (outcomeID==2){
    if (yaxisID%in%c(1:4)){
      plot.shiny.app.injury(countyID = countyID,barID = barID,yaxisID = yaxisID)
    }else{
      message('wrong input')
    }
    
    
  }else if (outcomeID==3){
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
        df.result.PA.aggr$type <- 'physical activity'
        
        df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 1)
        df.result.injury <- df.result.injury$df.fatality
        df.result.injury$type <- 'traffic injury'
        
        df.result.twomodule <- rbind(df.result.PA.aggr,df.result.injury)
        
        plot.title <- paste0(countyNames[countyID],': Reduction in Total Deaths')
        
        ggplot(data = df.result.twomodule, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
          geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Total Death Reduction')+
          facet_grid(.~type,scales = "free")+ggtitle(plot.title)
        
        
      }else if (yaxisID==2){ #total DALYs
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
        df.result.PA.aggr$type <- 'physical activity'
        
        df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 1)
        df.result.injury <- df.result.injury$df.serious
        df.result.injury$type <- 'traffic injury'
        
        df.result.twomodule <- rbind(df.result.PA.aggr,df.result.injury)
        
        plot.title <- paste0(countyNames[countyID],': Reduction in Total DALYs')
        
        ggplot(data = df.result.twomodule, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
          geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Total DALYs Reduction')+
          facet_grid(.~type,scales = "free")+ggtitle(plot.title)
      }else if (yaxisID==3){ # age.std deaths
        #test
        #countyID=1
        #barID = 1
        
        df.result.PA <- DFforFigure(AgeStdReductionOutcome[c((1*18+2*9-26):(1*18+2*9-18))],1,countyID,barID)
        
        df.result.PA.aggr.white <- df.result.PA[c(1,5,9),]
        for (i in 1:3) {
          value[i] <- sum(df.result.PA[((i+1):(i+3)),3])
        }
        df.result.PA.aggr.other <- data.frame(Scenario=unique(df.result.PA[,1]),DemogrGroup=rep('2.Other',3),V1 =(value))
        
        df.result.PA.aggr <- rbind(df.result.PA.aggr.white,df.result.PA.aggr.other)
        df.result.PA.aggr <- df.result.PA.aggr[order(df.result.PA.aggr$Scenario),]
        df.result.PA.aggr$type <- 'physical activity'
        
        df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 2)
        df.result.injury <- df.result.injury$df.fatality
        df.result.injury$type <- 'traffic injury'
        
        df.result.twomodule <- rbind(df.result.PA.aggr,df.result.injury)
        
        plot.title <- paste0(countyNames[countyID],': Age-Standardized Reduction in Total Deaths')
        
        ggplot(data = df.result.twomodule, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
          geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Death reduction rate (per 100,000 population)')+
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
        df.result.PA.aggr$type <- 'physical activity'
        
        df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 2)
        df.result.injury <- df.result.injury$df.serious
        df.result.injury$type <- 'traffic injury'
        
        plot.title <- paste0(countyNames[countyID],': Age-Standardized Reduction in Total DALYs')
        
        df.result.twomodule <- rbind(df.result.PA.aggr,df.result.injury)
        
        ggplot(data = df.result.twomodule, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
          geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('DALYs reduction rate (per 100,000 population)')+
          facet_grid(.~type,scales = "free")+ggtitle(plot.title)
        
        
      }
    }else{
      message('wrong input')
    }
    
  }
}

# Parameter description
# countyID: 1-ELD; 2-PLA; 3-SAC; 4-SUT; 5-YOL; 6-YUB; 7-All
# barID: 1-future years,2-scenarios,3-customized
# outcomeID: 1-physical activity; 2-injury; 3-both
# demogrID: 1-Race/ethnicty; 2-household income
# yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std; 5-physical activity data
integrated.shiny.app(countyID = 7, barID = 3,outcomeID = 1,demogrID = 1, yaxisID = 1)



