require("jsonlite")
require("RCurl")
require(ggplot2)
require("jsonlite")
require("RCurl")
# Change the USER and PASS below to be your UTEid
df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from BATTING where AB is not NULL"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_alk2223', PASS='orcl_alk2223', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

ndf <- df %>% group_by(TEAMID)%>% summarize(absum = sum(AB), hrsum = sum(HR), value = absum/hrsum)

ndf1 <- ndf %>% ungroup %>% summarize(trend=mean(value))

ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_continuous() +
#  facet_wrap(~CLARITY, ncol=1) +
  labs(title='Barchart') +
  labs(x=paste("TeamID"), y=paste("AB/HR")) +
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
