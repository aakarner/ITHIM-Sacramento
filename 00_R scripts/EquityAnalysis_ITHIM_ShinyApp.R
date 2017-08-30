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
# yaxisID: 1-Death total; 2-death age.std; 3-DALYs total; 4-DALYs age.std; 5-physical activity data
#          6-total fatality; 7-total serious injuries; 8-death total for two module;9-DALYs total for two module

#plot.shiny.app.PA(countyID = 1,dbID = 1,typeID = 1,demogrID = 1,barID = 1)
#plot.shiny.app.injury(countyID = 1,barID = 1,yaxisID = 6)

integrated.shiny.app <- function(countyID,barID,outcomeID,demogrID,yaxisID){
  if (outcomeID == 1){
    if (yaxisID == 1){
      plot.shiny.app.PA(countyID = countyID,dbID = 1,typeID = 1,demogrID = demogrID,barID = 1)
    }else if (yaxisID==2){
      plot.shiny.app.PA(countyID = countyID,dbID = 1,typeID = 2,demogrID = demogrID,barID = 1)
    }else if (yaxisID==3){
      plot.shiny.app.PA(countyID = countyID,dbID = 2,typeID = 1,demogrID = demogrID,barID = 1)
    }else if (yaxisID==4){
      plot.shiny.app.PA(countyID = countyID,dbID = 2,typeID = 2,demogrID = demogrID,barID = 1)
    }else if (yaxisID==5){
      plot.shiny.app.PA(countyID = countyID,dbID = 1,typeID = 3,demogrID = demogrID,barID = 1)
    }else{
      message('wrong input')
    }
    
  }else if (outcomeID==2){
    if (yaxisID%in%c(6,7)){
      plot.shiny.app.injury(countyID = countyID,barID = 1,yaxisID = yaxisID)
    }else{
      message('wrong input')
    }
    
    
  }else if (outcomeID==3){
    if (yaxisID==8){
      #test
      #countyID=1
      #barID = 1
      
      df.result.PA <- DFforFigure(RawReductionOutcome[c((1*18+1*9-26):(1*18+1*9-18))],1,countyID,barID)
      df.result.PA$type <- 'physical activity'
      df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID)
      df.result.injury <- df.result.injury$df.fatality
      df.result.injury$type <- 'traffic injury'
      
      df.result.twomodule <- rbind(df.result.PA,df.result.injury)
      
      ggplot(data = df.result.twomodule, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
        geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Total deaths')+
        facet_grid(.~type,scales = "free")+ggtitle("Reduction in total deaths")
      
    }else if (yaxisID==9){
      df.result.PA <- DFforFigure(RawReductionOutcome[c((1*18+2*9-26):(1*18+2*9-18))],1,countyID,barID)
      df.result.PA$type <- 'physical activity'
      df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID)
      df.result.injury <- df.result.injury$df.serious
      df.result.injury$type <- 'traffic injury'
      
      df.result.twomodule <- rbind(df.result.PA,df.result.injury)
      
      ggplot(data = df.result.twomodule, mapping = aes(x = factor(DemogrGroup), y = V1,fill = Scenario)) + 
        geom_bar(stat = 'identity',width = 0.5, position = position_dodge(0.5))+xlab('Demographic Group')+ylab('Total DALYs')+
        facet_grid(.~type,scales = "free")+ggtitle("Reduction in total DALYs")
    }else{
      message('wrong input')
    }
    
    
    
  }
}
