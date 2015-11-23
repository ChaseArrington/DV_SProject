# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)

shinyServer(function(input, output) {
  
  output$scatplot <- renderPlot({

    
    KPI_Low_Max_value = input$KPI1     
    KPI_Medium_Max_value = input$KPI2
    
    df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
 "select color, clarity, sum_price, round(sum_carat) as sum_carat, kpi as ratio, 
  case
  when kpi < "p1" then \\\'03 Low\\\'
  when kpi < "p2" then \\\'02 Medium\\\'
  else \\\'01 High\\\'
  end kpi
  from (select color, clarity, 
  sum(price) as sum_price, sum(carat) as sum_carat, 
  sum(price) / sum(carat) as kpi
  from diamonds
  group by color, clarity)
  order by clarity;"
  ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_cca628', PASS='orcl_cca628', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
    
    
    ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +

      labs(title='HR') +
      labs(x="Age", y=paste("Fare")) +
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
  output$barplot <- renderPlot({
    dfbar <-data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
 "select color, clarity, sum_price, round(sum_carat) as sum_carat, kpi as ratio, 
  case
  when kpi < "p1" then \\\'03 Low\\\'
  when kpi < "p2" then \\\'02 Medium\\\'
  else \\\'01 High\\\'
  end kpi
  from (select color, clarity, 
  sum(price) as sum_price, sum(carat) as sum_carat, 
  sum(price) / sum(carat) as kpi
  from diamonds
  group by color, clarity)
  order by clarity;"
  ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_cca628', PASS='orcl_cca628', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  
    ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_continuous() +
      #  facet_wrap(~CLARITY, ncol=1) +
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
    
    })
  
  output$crossplot <- renderPlot({
    dfbar <-data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
  "select color, clarity, sum_price, round(sum_carat) as sum_carat, kpi as ratio, 
  case
  when kpi < "p1" then \\\'03 Low\\\'
  when kpi < "p2" then \\\'02 Medium\\\'
  else \\\'01 High\\\'
  end kpi
  from (select color, clarity, 
  sum(price) as sum_price, sum(carat) as sum_carat, 
  sum(price) / sum(carat) as kpi
  from diamonds
  group by color, clarity)
  order by clarity;"
                                                      ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_cca628', PASS='orcl_cca628', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
    
})
