# server.R
require("jsonlite")
require("RCurl")
require("ggplot2")
require("dplyr")
require("shiny")
library("shiny")
shinyServer(function(input, output) {
  
  output$scatplot <- renderPlot({
  Beg_Year <- reactive({input$mindate})
  End_Year <- reactive({input$maxdate})
    
    df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="SELECT * from BATTING;"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_cca628', PASS='orcl_cca628', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE) ))
    
    ndf <- df %>% select(PLAYERID, YEARID, HR) %>% filter(PLAYERID %in% c("mcgwima01", "sosasa01", "bondsba01"))%>% filter(YEARID < End_Year(), YEARID > Beg_Year())%>% tbl_df 

    plot <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +

      labs(title='') +
      labs(x=paste("Year"), y=paste("HR")) +
      layer(data=ndf, 
            mapping=aes(x=as.numeric(as.character(YEARID)), y=as.numeric(as.character(HR)), color=PLAYERID), 
            stat="identity", 
            stat_params=list(), 
            geom="line",
              geom_params=list(), 
            position=position_jitter(width=0.3, height=0)
      )
    
    # End your code here.
    return(plot)
  })
  team_name <- eventReactive(input$clicks,{input$teams})
  
  output$barplot <- renderPlot({
    df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from BATTING where AB is not NULL"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_cca628', PASS='orcl_cca628', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE) ))
    
    ndf <- df %>% filter(TEAMID %in% team_name()) %>% group_by(TEAMID)%>% summarize(absum = sum(AB), hrsum = sum(HR), value = absum/hrsum)
    
    ndf1 <- ndf %>% ungroup %>% summarize(trend=mean(value))
    plot_bar <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_continuous() +
      labs(title='Power Hitting by Team') +
      labs(x=paste("TeamID"), y=paste("At bats per homerun")) +
      layer(data=ndf, 
            mapping=aes(x=TEAMID, y=value), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(colour="blue"), 
            position=position_identity()
      ) +
      layer(data=ndf1, 
            mapping=aes(yintercept = trend), 
            geom="hline",
            geom_params=list(colour="red")
      ) 
    
    return(plot_bar)
  })
  
  low_OBP <- reactive({input$minOBP})
  high_OBP <- reactive({input$maxOBP})

  output$crossplot <- renderPlot({
    
    df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from BATTING where AB > 100"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_cca628', PASS='orcl_cca628', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
    
    ndf <- df %>% select(YEARID,AB,H,HR, BB, HBP, SF, PLAYERID, TEAMID) %>% filter(TEAMID %in% c("TEX","SFN"), YEARID == 2010) %>% mutate(obp = round((H + BB + HBP)/(AB + BB + HBP + SF),4)) %>% filter(obp > low_OBP(), obp < high_OBP())
    
    
    
  plot_cross <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_discrete() +
      labs(title='On base percentage for World Series Teams') +
      labs(x=paste("Team"), y=paste("Player")) +
      layer(data=ndf, 
            mapping=aes(x=TEAMID, y=PLAYERID, label=obp), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black"), 
            position=position_identity()
      ) 
  return(plot_cross)
  })
})
