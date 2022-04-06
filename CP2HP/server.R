library(tidyverse)
library(DT)
# Define server logic to read selected file ----
coverage <- function (sched)
{
  schedule1<- sched
  schedule2<- sched
  
  # if end is less than start, end = end + 24. 
  schedule1$TEnd <- ifelse(schedule1$Tstart > schedule1$TEnd, schedule1$TEnd + 24, schedule1$TEnd) 
  
  # 1 if working in that interval, 0 otherwise. 
  for (i in c(0:23))
  {
    schedule1[as.character(i)] <- ifelse(schedule1$Tstart <= i & schedule1$TEnd > i, 1,0)
  }
  
  
  # schedule start is midnight. 
  # take only those shifts that overflow. end = start =0 otherwise
  schedule2$TEnd <- ifelse(schedule2$Tstart > schedule2$TEnd, schedule2$TEnd, 0)
  schedule2$Tstart <- 0
  
  # the next day
  schedule2$DStart <- schedule2$DStart + 1
  schedule2$DEnd <- schedule2$DEnd + 1
  
  # 1 if working in that interval, 0 otherwise. 
  for (i in c(0:23))
  {
    schedule2[as.character(i)] <- ifelse(schedule2$Tstart <= i & schedule2$TEnd > i, 1,0)
  }
  
  
  schedule_12 <- rbind(schedule1, schedule2) # bind the two parts
  
  covrge <- data.frame(matrix(ncol = 7, nrow = 24)) # column interval
  colnames(covrge) <- lapply(1:7, paste0,"_Date")
  
  for (datee in 1:7)
  {
    covrge[paste0(datee,"_Date")] <- schedule_12 %>% filter(DStart==datee)%>% select(!(DStart:TEnd)) %>% colSums(na.rm = T)
    
  }
  
  return(covrge)
  
}

accuracy_F <- function(covrge,cap)
{
  accuracy <- 1 - (sum(abs(covrge-cap),na.rm = T)/sum(cap,na.rm = T))
}


find_e <- function(text){
  short <- substring(text,1,which(strsplit(text, "")[[1]]=="e"))
  return(short)
}



server <- function(input, output) {
  
  cap_plan <- reactive({
    cap_plan <- if (is.null(input$file1)) {
      read.csv("capacityPlan22.csv")
    } else {
      read.csv(input$file1$datapath, sep = input$sep)
    }
    
    colnames(cap_plan) <- lapply(1:7, paste0,"_Date")
    cap_plan$Interval <- 0:23
    cap_plan
  })
  
  
  
  output$capplan <- renderDataTable(
                
                  cap_plan()[,-8],
                  rownames= FALSE
                )
  
  n = 0
  DaysCombo <- reactive(
    {
      combn(1:7, input$weeklydays)
    }
  )
  shiftLength <- reactive(
    {
      input$shiftlength
    }
  )
  req_effi <- reactive({
    input$eff_required
  })
  
combo_output <- eventReactive(input$Calculate, {
  
  ScheduleShells <- data.frame(AgentID = numeric(), days = numeric(),shift = numeric())
  TotalCoverage <- data.frame(matrix(0, ncol = 7, nrow = 24)) 
  colnames(TotalCoverage)<- lapply(1:7, paste0,"_Date")
  
  
  totalEff <- 0
  
  
  while(totalEff < req_effi()){
    n = n+1 # next agent
    # define schedule
    
    effi = list()
    covrge = data.frame(Interval = 0:23)
    
    for (startTime in 0:23)
    {
      # define days
      
      for(i in 1:ncol(DaysCombo()))
      {
        dayStart = dayEnd <- DaysCombo()[,i]
        endTime <- startTime+shiftLength()
        
        
        sched <- data.frame(DStart = dayStart,
                            DEnd = dayEnd,
                            Tstart = startTime,
                            TEnd = endTime)
        # add to existing coverage
        index <- paste(startTime,i,sep = "_")
        
        # covrge[index] <- TotalCoverage + coverage(sched)
        newTotalCov <- TotalCoverage + coverage(sched)
        colnames(newTotalCov) <- lapply(c("1_Date","2_Date","3_Date","4_Date","5_Date","6_Date","7_Date"), paste0,index)
        covrge <- cbind(covrge,newTotalCov)
        
        # check efficiency
        effi[index] <- accuracy_F(newTotalCov, cap_plan()[,-8])
        
        
      }
      
      
    }
    maxEff <- effi[which.max(effi)]
    
    if(as.numeric(maxEff)>=as.numeric(totalEff))
    {
      totalEff <- as.numeric(maxEff)
      
      bestcombo <- names(which.max(effi))
      # which(strsplit(bestcombo, "")[[1]]=="_")
      TotalCoverage <- covrge %>% select(ends_with(paste0("e",bestcombo)))
      
      ScheduleShells <- ScheduleShells %>% add_row(AgentID = n, 
                                                   days = DaysCombo()[,as.numeric(substring(bestcombo,which(strsplit(bestcombo, "")[[1]]=="_")+1,nchar(bestcombo)))],
                                                   shift = as.numeric(substring(bestcombo,1,which(strsplit(bestcombo, "")[[1]]=="_")-1)))
      
    }
    
    else { break}
    
  }
  
  
  
  colnames(TotalCoverage) <- lapply(colnames(TotalCoverage), find_e)
  TotalCoverage$Interval<- 0:23
  
  coverage_melt<- pivot_longer(TotalCoverage, -c(Interval), values_to = "StaffedHeads", names_to = "Date")
  capPlan_melt <- pivot_longer(cap_plan(), -c(Interval), values_to = "RequiredHeads", names_to = "Date")
  
  Requ_cover<- full_join(capPlan_melt,coverage_melt, by=c("Date","Interval"))
  
  
  
  
  combo <- list(Tcover = TotalCoverage, sshells = ScheduleShells, Teff = totalEff,reqCoverage = Requ_cover)
  combo
  
  
})
  
#Output 
combo <- reactive(
  {
    combo_output()
  }
)

#headcount
output$HeadsCOunt <- renderText(
  {
    length(unique(combo()$sshells$AgentID))
  }
)

#efficiency
output$Efficiency <- renderText(
  {
    paste(round(100*combo()$Teff,1),'%', sep = "")
  }
)

#schedule shells
output$scheduleshells <- renderDataTable(
  combo()$sshells,
  rownames= FALSE
) 
output$download <- downloadHandler(
  filename = function(){"thename.csv"}, 
  content = function(fname){
    write.csv(combo()$sshells, fname)
  }
)

  ## Plot coverage
output$coverageplots <- renderPlot({

  ggplot(combo()$reqCoverage, aes(x = Interval)) +
    geom_col(aes(y = StaffedHeads),fill="tomato4")+
    geom_line(aes(y = RequiredHeads),fill="darkgreen", size = 1.5)+
    facet_wrap(~Date,ncol = 2)+
    scale_x_continuous(breaks = seq(0,23,2))

})

 
  
}

