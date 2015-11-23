require("jsonlite")
require("RCurl")
require(ggplot2)
require("jsonlite")
require("RCurl")
# Change the USER and PASS below to be your UTEid
df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="SELECT * from BATTING"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_alk2223', PASS='orcl_alk2223', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))


ndf <- df %>% select(PLAYERID, YEARID, HR) %>% filter(PLAYERID %in% c("mcgwima01", "sosasa01", "bondsba01")) %>% tbl_df # Equivalent SQL: select cut, clarity from diamonds where cut in ('Good', 'Fair');# or Equivalent SQL:  select cut, clarity from diamonds where cut = 'Good' or cut = 'Fair';



ggplot() + 
  coord_cartesian() + 
  scale_x_continuous() +
  scale_y_continuous() +
  #facet_wrap(~SURVIVED) +
 # facet_grid(.~SURVIVED, labeller=label_both) + # Same as facet_wrap but with a label.
  #facet_grid(PCLASS~SURVIVED, labeller=label_both) +
  labs(title='HR') +
  labs(x="Age", y=paste("Fare")) +
  layer(data=ndf, 
        mapping=aes(x=as.numeric(as.character(YEARID)), y=as.numeric(as.character(HR)), color=PLAYERID), 
        stat="identity", 
        stat_params=list(), 
        geom="line",
        geom_params=list(), 
        #position=position_identity()
        position=position_jitter(width=0.3, height=0)
  )
