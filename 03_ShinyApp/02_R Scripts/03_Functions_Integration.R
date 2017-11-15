# This file is part of ITHIM Sacramento.

# File: 03_Functions_Integration.R
# Purpose: Functions for combining two types of health outcomes from physical activity and traffic injury

# Two functions included: one for "Advanced Plots" panel, and the other for "Simple Aggregated Plots" panel

# Function 1 Outputs for "Advanced Plots" panel  -------------------------------------------------------------
# compute the data frame for the integration of two modules
compute.df.integration <- function(countyID,barID,outcomeID,demogrID,yaxisID){
  if (countyID %in% (1:6)) {
    
    value <- NULL
    
    if (yaxisID == 1) { #total deaths
      
      df.result.PA <- DFforFigure(RawReductionOutcome[c((1*18+1*9-26):(1*18+1*9-18))],1,countyID,barID)
      
      # white group
      df.result.PA.aggr.white <- df.result.PA[c(1,5,9),]
      for (i in 1:3) {
        value[i] <- sum(df.result.PA[((4*i-2):(4*i)),3])
      }
      # people of color group
      df.result.PA.aggr.other <- data.frame(Scenario=unique(df.result.PA[,1]),DemogrGroup=rep("2.People of color",3),V1 =(value))
      
      df.result.PA.aggr <- rbind(df.result.PA.aggr.white,df.result.PA.aggr.other)
      df.result.PA.aggr <- df.result.PA.aggr[order(df.result.PA.aggr$Scenario),]
      
      # injury module
      df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 1)
      df.result.injury <- df.result.injury$df.fatality
      
    }else if (yaxisID == 3){ #total DALYs
      
      df.result.PA <- DFforFigure(RawReductionOutcome[c((1*18+2*9-26):(1*18+2*9-18))],1,countyID,barID)
      
      df.result.PA.aggr.white <- df.result.PA[c(1,5,9),]
      for (i in 1:3) {
        value[i] <- sum(df.result.PA[((4*i-2):(4*i)),3])
      }
      df.result.PA.aggr.other <- data.frame(Scenario=unique(df.result.PA[,1]),DemogrGroup=rep("2.People of color",3),V1 =(value))
      
      df.result.PA.aggr <- rbind(df.result.PA.aggr.white,df.result.PA.aggr.other)
      df.result.PA.aggr <- df.result.PA.aggr[order(df.result.PA.aggr$Scenario),]
      
      df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 1)
      df.result.injury <- df.result.injury$df.DALYs
      
    }else if (yaxisID == 2) { # age.std deaths
      
      df.result.PA.aggr <- DFforFigure.PA.twoRaces(AgeStdReductionOutcome.twoRaces[c((1*18+1*9-26):(1*18+1*9-18))],countyID,barID)
      
      df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 2)
      df.result.injury <- df.result.injury$df.fatality
      
    }else if (yaxisID == 4) { #age.std dalys
      
      df.result.PA.aggr <- DFforFigure.PA.twoRaces(AgeStdReductionOutcome.twoRaces[c(10:18)],countyID,barID)
      
      df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 2)
      df.result.injury <- df.result.injury$df.DALYs
    }
    
    df.result.PA.aggr$type <- "a. physical activity"
    df.result.injury$type <- "b. traffic injury"
    
    df.result.integration.temp <- df.result.PA.aggr[,1:2]
    df.result.integration.temp$V1 <- df.result.PA.aggr$V1+df.result.injury$V1
    df.result.integration.temp$type <- "c. both"
    
    df.result.integration <- rbind(df.result.PA.aggr,df.result.injury,df.result.integration.temp)
    
    return(df.result.integration = df.result.integration)
    
  }else if(countyID == 7){ #region wide
    
    df.region <- NULL
    
    if (yaxisID==1){ #death total
      
      for (countyID in c(1:6)){
        
        value <- NULL
        
        df.result.PA <- DFforFigure(RawReductionOutcome[c((1*18+1*9-26):(1*18+1*9-18))],1,countyID,barID)
        df.result.PA.aggr.white <- df.result.PA[c(1,5,9),]
        for (i in 1:3) {
          value[i] <- sum(df.result.PA[((4*i-2):(4*i)),3])
        }
        df.result.PA.aggr.other <- data.frame(Scenario=unique(df.result.PA[,1]),DemogrGroup=rep("2.People of color",3),V1 =(value))
        
        df.result.PA.aggr <- rbind(df.result.PA.aggr.white,df.result.PA.aggr.other)
        df.result.PA.aggr <- df.result.PA.aggr[order(df.result.PA.aggr$Scenario),]
        df.result.PA.aggr$type <- "a. physical activity"
        
        df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 1)
        df.result.injury <- df.result.injury$df.fatality
        df.result.injury$type <- "b. traffic injury"
        
        df.result.integration.temp <- df.result.PA.aggr[,1:2]
        df.result.integration.temp$V1 <- df.result.PA.aggr$V1+df.result.injury$V1
        
        df.region <- rbind(df.region,df.result.integration.temp)
        
      }
      
    }else if (yaxisID==2){#death age.std
      
      for (countyID in c(1:6)){
        value <- NULL
        
        df.result.PA.aggr <- DFforFigure.PA.twoRaces(AgeStdReductionOutcome.twoRaces[c((1*18+1*9-26):(1*18+1*9-18))],countyID,barID)
        df.result.PA.aggr$type <- "a. physical activity"
        
        df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 2)
        df.result.injury <- df.result.injury$df.fatality
        df.result.injury$type <- "b. traffic injury"
        
        df.result.integration.temp <- df.result.PA.aggr[,1:2]
        df.result.integration.temp$V1 <- df.result.PA.aggr$V1+df.result.injury$V1
        
        df.region <- rbind(df.region,df.result.integration.temp)
      }
      
    }else if (yaxisID == 3){# total DALYs
      
      for(countyID in 1:6){
        
        value <- NULL
        
        df.result.PA <- DFforFigure(RawReductionOutcome[c((1*18+2*9-26):(1*18+2*9-18))],1,countyID,barID)
        
        df.result.PA.aggr.white <- df.result.PA[c(1,5,9),]
        for (i in 1:3) {
          value[i] <- sum(df.result.PA[((4*i-2):(4*i)),3])
        }
        df.result.PA.aggr.other <- data.frame(Scenario=unique(df.result.PA[,1]),DemogrGroup=rep("2.People of color",3),V1 =(value))
        
        df.result.PA.aggr <- rbind(df.result.PA.aggr.white,df.result.PA.aggr.other)
        df.result.PA.aggr <- df.result.PA.aggr[order(df.result.PA.aggr$Scenario),]
        df.result.PA.aggr$type <- "a. physical activity"
        
        df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 1)
        df.result.injury <- df.result.injury$df.DALYs
        df.result.injury$type <- "b. traffic injury"
        
        df.result.integration.temp <- df.result.PA.aggr[,1:2]
        df.result.integration.temp$V1 <- df.result.PA.aggr$V1+df.result.injury$V1
        
        df.region <- rbind(df.region,df.result.integration.temp)
      }
      
    }else if (yaxisID == 4){#age.std DALYs
      
      for (countyID in 1:6){
        value <- NULL
        
        df.result.PA.aggr <- DFforFigure.PA.twoRaces(AgeStdReductionOutcome.twoRaces[c(10:18)],countyID,barID)
        df.result.PA.aggr$type <- "a. physical activity"
        
        df.result.injury <- DFforFigure.injury(barID = barID,countyID = countyID,typeID = 2)
        df.result.injury <- df.result.injury$df.DALYs
        df.result.injury$type <- "b. traffic injury"
        
        df.result.integration.temp <- df.result.PA.aggr[,1:2]
        df.result.integration.temp$V1 <- df.result.PA.aggr$V1+df.result.injury$V1
        
        df.region <- rbind(df.region,df.result.integration.temp)
      }
    }
    
    df.region$county <- rep(countyNames,each = 6)
    
    return(df.region = df.region)
    
  }
}

# plot the result according to the inputs
integrated.shiny.app <- function(countyID,barID,outcomeID,demogrID,yaxisID){
  if (outcomeID == 1){
    
    if (yaxisID == 1){ # death total
      plot.shiny.app.PA(countyID = countyID,dbID = 1,typeID = 1,demogrID = demogrID,barID = barID)
    }else if (yaxisID == 2) { # death age.std
      plot.shiny.app.PA(countyID = countyID,dbID = 1,typeID = 2,demogrID = demogrID,barID = barID)
    }else if (yaxisID == 3) { # DALYs total
      plot.shiny.app.PA(countyID = countyID,dbID = 2,typeID = 1,demogrID = demogrID,barID = barID)
    }else if (yaxisID == 4) { # DALYs age.std
      plot.shiny.app.PA(countyID = countyID,dbID = 2,typeID = 2,demogrID = demogrID,barID = barID)
    }else if (yaxisID == 5) { # physical activity time
      plot.shiny.app.PA(countyID = countyID,dbID = 1,typeID = 3,demogrID = demogrID,barID = barID)
    }
    
  }else if (outcomeID == 2){
    
    if (yaxisID %in% (1:4)){
      plot.shiny.app.injury(countyID = countyID,barID = barID,yaxisID = yaxisID)
    }
    
  }else if (outcomeID == 3){ # both modules
    
    if (countyID %in% c(1:6)){
      
      if (yaxisID == 1){
        
        plot.title <- paste0(countyNames[countyID],": Reduction in Total Deaths")
        ylabel <- "Reduced deaths (total)"
        
      }else if (yaxisID == 2){
        
        plot.title <- paste0(countyNames[countyID],": Reduction in Total Deaths\n",
                             "Standardized by Age and Population")
        ylabel <- "Reduction in deaths\n(per 100,000 population)"
        
      }else if (yaxisID == 3){
        
        plot.title <- paste0(countyNames[countyID],": Reduction in Total DALYs")
        ylabel <- "Reduction in DALYs (total)"
        
      }else if (yaxisID == 4){
        
        plot.title <- paste0(countyNames[countyID],": Reduction in Total DALYs\n",
                             "Standardized by Age and Population")
        ylabel <- "Reduction in DALYs\n(per 100,000 population)"
        
      }
      
      df.result.integration <- compute.df.integration(countyID,barID,outcomeID,demogrID,yaxisID)
      
      ggplot(data = df.result.integration, aes(x = factor(DemogrGroup), y = V1, fill = Scenario)) + 
        geom_bar(stat = "identity", width = 0.5, position = position_dodge(0.5)) + 
        scale_fill_brewer(palette = "Set1") + 
        xlab(NULL) + 
        ylab(ylabel) +
        geom_text(aes(label = round(V1, 1)), color = "black", size = 4, vjust = "inward", 
                  position = position_dodge(width = 0.5)) +
        ggtitle(plot.title) + 
        theme_bw(base_size = 15) +
        theme(legend.position = "bottom",
              plot.caption = element_text(hjust = 0, margin = margin(t = 15))) +
        labs(caption = plot.caption.text) +
        facet_grid(.~type, scales = "free")
      
    }else if (countyID == 7){
      
      if (yaxisID == 1){
        
        plot.title <- "Region-Wide: Reduction in Total Deaths"
        ylabel <- "Reduction in deaths (total)"
        
      }else if (yaxisID == 2){
        
        plot.title <- "Region-Wide: Reduction in Total Deaths\nStandardized by Age and Population"
        ylabel <- "Reduction in deaths\n(per 100,000 population)"
        
      }else if (yaxisID ==3 ){
        
        plot.title <- "Region-Wide: Reduction in Total DALYs"
        ylabel <- "Reduction in DALYs (total)"
        
      }else if (yaxisID == 4){
        
        plot.title <- "Region-Wide: Reduction in Total DALYs\nStandardized by Age and Population"
        ylabel <- "DALYs reduction rate\n(per 100,000 population)"
        
      }
      
      df.region <- compute.df.integration(countyID,barID,outcomeID,demogrID,yaxisID)
      
      ggplot(data = df.region, aes(x = factor(DemogrGroup), y = V1, fill = Scenario)) + 
        geom_bar(stat = "identity", width = 0.5, position = position_dodge(0.5)) + 
        scale_fill_brewer(palette = "Set1") + 
        xlab(NULL) + 
        ylab(ylabel) +
        theme_bw(base_size = 15) +
        theme(legend.position = "bottom",
              plot.caption = element_text(hjust = 0, margin = margin(t = 15))) +
        labs(caption = plot.caption.text) +
        ggtitle(plot.title) +
        facet_wrap(~ county, scales = "free")
    }
    
  }
}

# Function 2 Outputs for "Simple Aggregated Plots" panel -----------------------------------------
aggr.outcome.shiny.app <- function(barID,yaxisID){
  
  if(barID==1){ # future years
    
    scenario.name.sep <- c("2020","2027","2036")
    scenairo.name<-rep(scenario.name.sep,6)
    
  }else if(barID==2){ # scenarios
    
    scenario.name.sep <- c("S1","S2","S3")
    scenairo.name<-rep(scenario.name.sep,6)
  }
  
  if (yaxisID==1){#death total
    # PA module
    value<-NULL
    
    PA.disaggr <- DFforRegionWide(ReductionOutcome = RawReductionOutcome,demogrID = 1,dbID = 1,barID = barID)
    
    for (i in 1:18){
      value[i]<-sum(PA.disaggr[(4*i-3):(4*i),3])
    }
    
    df.PA.aggr.temp <- data.frame(Scenario = scenairo.name,county = rep(countyNames,each=3),V1=value,type="physical activity")
    
    value <- NULL
    for (i in 1:3){
      value[i]<-sum(df.PA.aggr.temp[c(i,i+3,i+6,i+9,i+12,i+15),3])
    }
    
    df.PA.aggr <- data.frame(Scenario = scenario.name.sep,type = "a. physical activity",V1=value)
    
    # injury module
    df.injury.region <- NULL
    
    for (i in 1:6){ #county
      value <-NULL
      
      df.temp <- DFforFigure.injury(barID = barID,i,typeID = 1)
      
      for (j in 1:3){
        value[j] <- sum(df.temp$df.fatality[(2*j-1):(2*j),3]) 
      }
      
      df.injury.aggr.temp <- data.frame(V1=value,type="b. traffic injury")
      df.injury.region <- rbind(df.injury.region,df.injury.aggr.temp)
      
    }
    
    df.injury.region <- data.frame(county = rep(countyNames,each=3),df.injury.region)
    df.injury.region <- data.frame(Scenario = scenairo.name,df.injury.region)
    
    value <- NULL
    for (i in 1:3){
      value[i]<-sum(df.injury.region[c(i,i+3,i+6,i+9,i+12,i+15),3])
    }
    
    df.injury.aggr <- data.frame(Scenario = scenario.name.sep,type = "b. traffic injury",V1=value)
    
    # sum of two module
    df.result.integration.temp <- df.PA.aggr[,1:2]
    df.result.integration.temp$V1 <- df.PA.aggr$V1+df.injury.aggr$V1
    df.result.integration.temp$type <- "c. both"
    
    df.integration.aggr <- rbind(df.PA.aggr,df.injury.aggr,df.result.integration.temp)
    
    plot.title <- "Region-Wide: Reduction in Total Deaths"
    ylabel <- "Reduced deaths (total)"
    
  }else if (yaxisID == 2) { # death age.std
    # PA module
    value <- NULL
    value.injury <- NULL
    
    #aggragation
    if(barID == 1) { # future years
      sum.temp <- 0
      sum.temp.injury <- 0
      
      for (i in 1:6) { # six counties
        temp.a <- output.HealthOutcome(i)
        temp.b <- temp.a$HealthOutcome_byRace.2020$`1.NHW`$delta.Burden+temp.a$HealthOutcome_byRace.2020$`2.NHB`$delta.Burden+
          temp.a$HealthOutcome_byRace.2020$`3.NHO`$delta.Burden+temp.a$HealthOutcome_byRace.2020$`4.HO`$delta.Burden
        sum.temp <- sum.temp+temp.b
        
        temp.a.injury <- createInjuryResults(countyID =  i,scenarioID =  1)
        temp.b.injury <- temp.a.injury$Reduction.Death.white.disaggr+temp.a.injury$Reduction.Death.other.disaggr
        sum.temp.injury <- sum.temp.injury+temp.b.injury
      }
      
      value[1]=-sum(sum.temp[,1]/Pop.file.region*100000*US.pop)/sum(US.pop)
      value.injury[1]=sum(sum.temp.injury[,1]/Pop.file.region*100000*US.pop)/sum(US.pop)
      
      sum.temp <- 0
      sum.temp.injury <- 0
      
      for (i in 1:6){# six counties
        temp.a <- output.HealthOutcome(i)
        temp.b <- temp.a$HealthOutcome_byRace.2027$`1.NHW`$delta.Burden+temp.a$HealthOutcome_byRace.2027$`2.NHB`$delta.Burden+
          temp.a$HealthOutcome_byRace.2027$`3.NHO`$delta.Burden+temp.a$HealthOutcome_byRace.2027$`4.HO`$delta.Burden
        sum.temp <- sum.temp+temp.b
        
        temp.a.injury <- createInjuryResults(countyID =  i,scenarioID =  3)
        temp.b.injury <- temp.a.injury$Reduction.Death.white.disaggr+temp.a.injury$Reduction.Death.other.disaggr
        sum.temp.injury <- sum.temp.injury+temp.b.injury
      }
      
      value[2]=-sum(sum.temp[,1]/Pop.file.region*100000*US.pop)/sum(US.pop)
      value.injury[2]=sum(sum.temp.injury[,1]/Pop.file.region*100000*US.pop)/sum(US.pop)
      
      sum.temp <- 0
      sum.temp.injury <- 0
      
      for (i in 1:6){# six counties
        temp.a <- output.HealthOutcome(i)
        temp.b <- temp.a$HealthOutcome_byRace.2036$`1.NHW`$delta.Burden+temp.a$HealthOutcome_byRace.2036$`2.NHB`$delta.Burden+
          temp.a$HealthOutcome_byRace.2036$`3.NHO`$delta.Burden+temp.a$HealthOutcome_byRace.2036$`4.HO`$delta.Burden
        sum.temp <- sum.temp+temp.b
        
        temp.a.injury <- createInjuryResults(countyID =  i,scenarioID =  2)
        temp.b.injury <- temp.a.injury$Reduction.Death.white.disaggr+temp.a.injury$Reduction.Death.other.disaggr
        sum.temp.injury <- sum.temp.injury+temp.b.injury
        
      }
      
      value[3]=-sum(sum.temp[,1]/Pop.file.region*100000*US.pop)/sum(US.pop)
      value.injury[3]=sum(sum.temp.injury[,1]/Pop.file.region*100000*US.pop)/sum(US.pop)
      
    }else{#scenarios
      sum.temp <- 0
      sum.temp.injury <- 0
      
      for (i in 1:6){# six counties
        temp.a <- output.HealthOutcome(i)
        temp.b <- temp.a$HealthOutcome_byRace.S1$`1.NHW`$delta.Burden+temp.a$HealthOutcome_byRace.S1$`2.NHB`$delta.Burden+
          temp.a$HealthOutcome_byRace.S1$`3.NHO`$delta.Burden+temp.a$HealthOutcome_byRace.S1$`4.HO`$delta.Burden
        sum.temp <- sum.temp+temp.b
        
        temp.a.injury <- createInjuryResults(countyID =  i,scenarioID =  4)
        temp.b.injury <- temp.a.injury$Reduction.Death.white.disaggr+temp.a.injury$Reduction.Death.other.disaggr
        sum.temp.injury <- sum.temp.injury+temp.b.injury
      }
      
      value[1]=-sum(sum.temp[,1]/Pop.file.region*100000*US.pop)/sum(US.pop)
      value.injury[1]=sum(sum.temp.injury[,1]/Pop.file.region*100000*US.pop)/sum(US.pop)
      
      sum.temp <- 0
      sum.temp.injury <- 0
      
      for (i in 1:6){# six counties
        temp.a <- output.HealthOutcome(i)
        temp.b <- temp.a$HealthOutcome_byRace.S2$`1.NHW`$delta.Burden+temp.a$HealthOutcome_byRace.S2$`2.NHB`$delta.Burden+
          temp.a$HealthOutcome_byRace.S2$`3.NHO`$delta.Burden+temp.a$HealthOutcome_byRace.S2$`4.HO`$delta.Burden
        sum.temp <- sum.temp+temp.b
        
        temp.a.injury <- createInjuryResults(countyID =  i,scenarioID =  5)
        temp.b.injury <- temp.a.injury$Reduction.Death.white.disaggr+temp.a.injury$Reduction.Death.other.disaggr
        sum.temp.injury <- sum.temp.injury+temp.b.injury
      }
      
      value[2]=-sum(sum.temp[,1]/Pop.file.region*100000*US.pop)/sum(US.pop)
      value.injury[2]=sum(sum.temp.injury[,1]/Pop.file.region*100000*US.pop)/sum(US.pop)
      
      sum.temp <- 0
      sum.temp.injury <- 0
      
      for (i in 1:6){# six counties
        temp.a <- output.HealthOutcome(i)
        temp.b <- temp.a$HealthOutcome_byRace.S3$`1.NHW`$delta.Burden+temp.a$HealthOutcome_byRace.S3$`2.NHB`$delta.Burden+
          temp.a$HealthOutcome_byRace.S3$`3.NHO`$delta.Burden+temp.a$HealthOutcome_byRace.S3$`4.HO`$delta.Burden
        sum.temp <- sum.temp+temp.b
        
        temp.a.injury <- createInjuryResults(countyID =  i,scenarioID =  6)
        temp.b.injury <- temp.a.injury$Reduction.Death.white.disaggr+temp.a.injury$Reduction.Death.other.disaggr
        sum.temp.injury <- sum.temp.injury+temp.b.injury
      }
      
      value[3]=-sum(sum.temp[,1]/Pop.file.region*100000*US.pop)/sum(US.pop)
      value.injury[3]=sum(sum.temp.injury[,1]/Pop.file.region*100000*US.pop)/sum(US.pop)
    }
    
    df.PA.aggr <- data.frame(Scenario = scenario.name.sep,type = "a. physical activity",V1=value)
    
    ####injury module
    
    df.injury.aggr <- data.frame(Scenario = scenario.name.sep,type = "b. traffic injury",V1=value.injury)
    
    # sum of two module
    df.result.integration.temp <- df.PA.aggr[,1:2]
    df.result.integration.temp$V1 <- df.PA.aggr$V1+df.injury.aggr$V1
    df.result.integration.temp$type <- "c. both"
    
    df.integration.aggr <- rbind(df.PA.aggr,df.injury.aggr,df.result.integration.temp)
    
    plot.title <- "Region-Wide: Reduction in Total Deaths\nStandardized by Age and Population"
    ylabel <- "Deaths reduced\n(per 100,000 population)"
    
  }else if(yaxisID == 3) { # DALYs total
    # PA module
    value <- NULL
    
    PA.disaggr <- DFforRegionWide(ReductionOutcome = RawReductionOutcome,demogrID = 1, dbID = 2, barID = barID)
    
    for (i in 1:18) {
      value[i]<-sum(PA.disaggr[(4*i-3):(4*i),3])
    }
    
    df.PA.aggr.temp <- data.frame(Scenario = scenairo.name,county = rep(countyNames,each=3),V1=value,type="a. physical activity")
    
    value <- NULL
    for (i in 1:3){
      value[i]<-sum(df.PA.aggr.temp[c(i,i+3,i+6,i+9,i+12,i+15),3])
    }
    
    df.PA.aggr <- data.frame(Scenario = scenario.name.sep,type = "a. physical activity",V1=value)
    
    # injury module
    df.injury.region <- NULL
    
    for (i in 1:6){ #county
      value <-NULL
      
      df.temp <- DFforFigure.injury(barID = barID,i,typeID = 1)
      
      for (j in 1:3){
        value[j] <- sum(df.temp$df.DALYs[(2*j-1):(2*j),3]) 
      }
      
      df.injury.aggr.temp <- data.frame(V1=value,type="b. traffic injury")
      
      df.injury.region <- rbind(df.injury.region,df.injury.aggr.temp)
      
    }
    
    df.injury.region<-data.frame(county = rep(countyNames,each=3),df.injury.region)
    df.injury.region<-data.frame(Scenario = scenairo.name,df.injury.region)
    
    value <- NULL
    for (i in 1:3){
      value[i]<-sum(df.injury.region[c(i,i+3,i+6,i+9,i+12,i+15),3])
    }
    
    df.injury.aggr <- data.frame(Scenario = scenario.name.sep,type = "b. traffic injury",V1=value)
    
    # sum of two module
    df.result.integration.temp <- df.PA.aggr[,1:2]
    df.result.integration.temp$V1 <- df.PA.aggr$V1+df.injury.aggr$V1
    df.result.integration.temp$type <- "c. both"
    
    df.integration.aggr <- rbind(df.PA.aggr,df.injury.aggr,df.result.integration.temp)
    
    plot.title <- "Region-Wide: Reduction in Total DALYs"
    ylabel <- "Reduction in DALYs (total)"
    
  }else if (yaxisID == 4) { # DALYs age.std
    # PA module
    value<-NULL
    value.injury <- NULL
    
    #aggragation
    if(barID ==1 ){#future years
      
      sum.temp <- 0
      sum.temp.injury <- 0
      
      for (i in 1:6){# six counties
        temp.a <- output.HealthOutcome(i)
        temp.b <- temp.a$HealthOutcome_byRace.2020$`1.NHW`$delta.Burden+temp.a$HealthOutcome_byRace.2020$`2.NHB`$delta.Burden+
          temp.a$HealthOutcome_byRace.2020$`3.NHO`$delta.Burden+temp.a$HealthOutcome_byRace.2020$`4.HO`$delta.Burden
        sum.temp <- sum.temp+temp.b
        
        temp.a.injury <- createInjuryResults(countyID =  i,scenarioID =  1)
        temp.b.injury <- temp.a.injury$Reduction.DALYs.white.disaggr+temp.a.injury$Reduction.DALYs.other.disaggr
        sum.temp.injury <- sum.temp.injury+temp.b.injury
      }
      
      value[1]=-sum(sum.temp[,4]/Pop.file.region*100000*US.pop)/sum(US.pop)
      value.injury[1]=sum(sum.temp.injury[,1]/Pop.file.region*100000*US.pop)/sum(US.pop)
      
      sum.temp <- 0
      sum.temp.injury <- 0
      
      for (i in 1:6) { # six counties
        temp.a <- output.HealthOutcome(i)
        temp.b <- temp.a$HealthOutcome_byRace.2027$`1.NHW`$delta.Burden+temp.a$HealthOutcome_byRace.2027$`2.NHB`$delta.Burden+
          temp.a$HealthOutcome_byRace.2027$`3.NHO`$delta.Burden+temp.a$HealthOutcome_byRace.2027$`4.HO`$delta.Burden
        sum.temp <- sum.temp+temp.b
        
        temp.a.injury <- createInjuryResults(countyID =  i,scenarioID =  3)
        temp.b.injury <- temp.a.injury$Reduction.DALYs.white.disaggr+temp.a.injury$Reduction.DALYs.other.disaggr
        sum.temp.injury <- sum.temp.injury+temp.b.injury
      }
      
      value[2]=-sum(sum.temp[,4]/Pop.file.region*100000*US.pop)/sum(US.pop)
      value.injury[2]=sum(sum.temp.injury[,1]/Pop.file.region*100000*US.pop)/sum(US.pop)
      
      sum.temp <- 0
      sum.temp.injury <- 0
      
      for (i in 1:6){# six counties
        temp.a <- output.HealthOutcome(i)
        temp.b <- temp.a$HealthOutcome_byRace.2036$`1.NHW`$delta.Burden+temp.a$HealthOutcome_byRace.2036$`2.NHB`$delta.Burden+
          temp.a$HealthOutcome_byRace.2036$`3.NHO`$delta.Burden+temp.a$HealthOutcome_byRace.2036$`4.HO`$delta.Burden
        sum.temp <- sum.temp+temp.b
        
        temp.a.injury <- createInjuryResults(countyID =  i,scenarioID =  2)
        temp.b.injury <- temp.a.injury$Reduction.DALYs.white.disaggr+temp.a.injury$Reduction.DALYs.other.disaggr
        sum.temp.injury <- sum.temp.injury+temp.b.injury
        
      }
      
      value[3]=-sum(sum.temp[,4]/Pop.file.region*100000*US.pop)/sum(US.pop)
      value.injury[3]=sum(sum.temp.injury[,1]/Pop.file.region*100000*US.pop)/sum(US.pop)
      
    }else{ #scenarios
      sum.temp <- 0
      sum.temp.injury <- 0
      
      for (i in 1:6){# six counties
        temp.a <- output.HealthOutcome(i)
        temp.b <- temp.a$HealthOutcome_byRace.S1$`1.NHW`$delta.Burden+temp.a$HealthOutcome_byRace.S1$`2.NHB`$delta.Burden+
          temp.a$HealthOutcome_byRace.S1$`3.NHO`$delta.Burden+temp.a$HealthOutcome_byRace.S1$`4.HO`$delta.Burden
        sum.temp <- sum.temp+temp.b
        
        temp.a.injury <- createInjuryResults(countyID =  i,scenarioID =  4)
        temp.b.injury <- temp.a.injury$Reduction.DALYs.white.disaggr+temp.a.injury$Reduction.DALYs.other.disaggr
        sum.temp.injury <- sum.temp.injury+temp.b.injury
      }
      
      value[1]=-sum(sum.temp[,4]/Pop.file.region*100000*US.pop)/sum(US.pop)
      value.injury[1]=sum(sum.temp.injury[,1]/Pop.file.region*100000*US.pop)/sum(US.pop)
      
      sum.temp <- 0
      sum.temp.injury <- 0
      
      for (i in 1:6){# six counties
        temp.a <- output.HealthOutcome(i)
        temp.b <- temp.a$HealthOutcome_byRace.S2$`1.NHW`$delta.Burden+temp.a$HealthOutcome_byRace.S2$`2.NHB`$delta.Burden+
          temp.a$HealthOutcome_byRace.S2$`3.NHO`$delta.Burden+temp.a$HealthOutcome_byRace.S2$`4.HO`$delta.Burden
        sum.temp <- sum.temp+temp.b
        
        temp.a.injury <- createInjuryResults(countyID =  i,scenarioID =  5)
        temp.b.injury <- temp.a.injury$Reduction.DALYs.white.disaggr+temp.a.injury$Reduction.DALYs.other.disaggr
        sum.temp.injury <- sum.temp.injury+temp.b.injury
      }
      
      value[2]=-sum(sum.temp[,4]/Pop.file.region*100000*US.pop)/sum(US.pop)
      value.injury[2]=sum(sum.temp.injury[,1]/Pop.file.region*100000*US.pop)/sum(US.pop)
      
      sum.temp <- 0
      sum.temp.injury <- 0
      
      for (i in 1:6){# six counties
        temp.a <- output.HealthOutcome(i)
        temp.b <- temp.a$HealthOutcome_byRace.S3$`1.NHW`$delta.Burden+temp.a$HealthOutcome_byRace.S3$`2.NHB`$delta.Burden+
          temp.a$HealthOutcome_byRace.S3$`3.NHO`$delta.Burden+temp.a$HealthOutcome_byRace.S3$`4.HO`$delta.Burden
        sum.temp <- sum.temp+temp.b
        
        temp.a.injury <- createInjuryResults(countyID =  i,scenarioID =  6)
        temp.b.injury <- temp.a.injury$Reduction.DALYs.white.disaggr+temp.a.injury$Reduction.DALYs.other.disaggr
        sum.temp.injury <- sum.temp.injury+temp.b.injury
      }
      
      value[3]=-sum(sum.temp[,4]/Pop.file.region*100000*US.pop)/sum(US.pop)
      value.injury[3]=sum(sum.temp.injury[,1]/Pop.file.region*100000*US.pop)/sum(US.pop)
    }
    
    df.PA.aggr <- data.frame(Scenario = scenario.name.sep,type = "a. physical activity",V1=value)
    
    # injury module
    
    df.injury.aggr <- data.frame(Scenario = scenario.name.sep,type = "b. traffic injury",V1=value.injury)
    
    # sum of two module
    df.result.integration.temp <- df.PA.aggr[,1:2]
    df.result.integration.temp$V1 <- df.PA.aggr$V1+df.injury.aggr$V1
    df.result.integration.temp$type <- "c. both"
    
    df.integration.aggr <- rbind(df.PA.aggr,df.injury.aggr,df.result.integration.temp)
    
    plot.title <- "Region-Wide: Reduction in Total DALYs\nStandardized by Age and Population"
    ylabel <- "Reduction in DALYs\n(per 100,000 population)"
    
  }
  
  ggplot(data = df.integration.aggr, aes(x = factor(type), y = V1, fill = Scenario)) + 
    geom_bar(stat = "identity",width = 0.5, position = position_dodge(0.5)) + 
    scale_fill_brewer(palette = "Set1") + 
    xlab(NULL) + 
    ylab(ylabel) +
    geom_text(aes(label = round(V1, 1)), color = "black", size = 4, vjust = "inward", 
              position = position_dodge(width = 0.5)) +
    ggtitle(plot.title) + 
    theme_bw(base_size = 15) +
    theme(legend.position = "bottom",
          plot.caption = element_text(hjust = 0, margin = margin(t = 15))) +
    labs(caption = paste("Planning scenarios and future years are shown relative to\n",
                         "the baseline year 2012."))
  
  
}
