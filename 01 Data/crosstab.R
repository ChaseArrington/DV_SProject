require("jsonlite")
require("RCurl")
require(ggplot2)
require("dplyr")

# Change the USER and PASS below to be your UTEid
df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from BATTING where AB > 100"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_alk2223', PASS='orcl_alk2223', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

ndf <- df %>% select(YEARID,AB,H,HR, BB, HBP, SF, PLAYERID, TEAMID) %>% filter(TEAMID %in% c("TEX","SFN"), YEARID == 2010) %>% mutate(obp = round((H + BB + HBP)/(AB + BB + HBP + SF),4))



ggplot() + 
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