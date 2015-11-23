require(tidyr)
require(dplyr)
require(ggplot2)

setwd("~/Desktop/CS/Data Visualization/DV_TProject1/01 Data")

file_path <- "WHO.csv"

whodf <- read.csv(file_path, stringsAsFactors = FALSE)

# Replace "." (i.e., period) with "_" in the column names.
names(whodf) <- gsub("\\.+", "_", names(whodf))

# str(whodf) # Uncomment this and  run just the lines to here to get column types to use for getting the list of measures.

measures <- c("Adolescent fertility rate (%)",	"Adult literacy rate (%)",	"Gross national income per capita (PPP international $)",	"Net primary school enrolment ratio female (%)",	"Net primary school enrolment ratio male (%)",	"Population (in thousands) total",	"Population annual growth rate (%)",	"Population in urban areas (%)",	"Population living below the poverty line (% living on &lt; US$1 per day)",	"Population median age (years)",	"Population proportion over 60 (%)",	"Population proportion under 15 (%)",	"Registration coverage of births (%)",	"Total fertility rate (per woman)",	"Antenatal care coverage - at least four visits (%)",	"Antiretroviral therapy coverage among HIV-infected pregt women for PMTCT (%)",	"Antiretroviral therapy coverage among people with advanced HIV infections (%)",	"Births attended by skilled health personnel (%)",	"Births by caesarean section (%)")
#measures <- NA # Do this if there are no measures.

# Get rid of special characters in each column.
# Google ASCII Table to understand the following:
for(n in names(whodf)) {
  whodf[n] <- data.frame(lapply(whodf[n], gsub, pattern="[^ -~]",replacement= ""))
}

dimensions <- c("Country", "Continent")

library(lubridate)
# Fix date columns, this needs to be done by hand because | needs to be correct.
#                                                        \_/
whodf$Order_Date <- gsub(" [0-9]+:.*", "", gsub(" UTC", "", mdy(as.character(whodf$Order_Date), tz="UTC")))
whodf$Ship_Date  <- gsub(" [0-9]+:.*", "", gsub(" UTC", "", mdy(as.character(whodf$Ship_Date),  tz="UTC")))

# The following is an example of dealing with special cases like making state abbreviations be all upper case.
# whodf["State"] <- data.frame(lapply(whodf["State"], toupper))

# Get rid of all characters in measures except for numbers, the - sign, and period.dimensions
if( length(measures) > 1 || ! is.na(measures)) {
  for(m in measures) {
    whodf[m] <- data.frame(lapply(whodf[m], gsub, pattern="[^--.0-9]",replacement= ""))
  }
}

write.csv(whodf, paste(gsub(".csv", "", file_path), ".reformatted.csv", sep=""), row.names=FALSE, na = "")

tableName <- gsub(" +", "_", gsub("[^A-z, 0-9, ]", "", gsub(".csv", "", file_path)))
sql <- paste("CREATE TABLE", tableName, "(\n-- Change table_name to the table name you want.\n")
if( length(measures) > 1 || ! is.na(dimensions)) {
  for(d in dimensions) {
    sql <- paste(sql, paste(d, "varchar2(4000),\n"))
  }
}
if( length(measures) > 1 || ! is.na(measures)) {
  for(m in measures) {
    if(m != tail(measures, n=1)) sql <- paste(sql, paste(m, "number(38,4),\n"))
    else sql <- paste(sql, paste(m, "number(38,4)\n"))
  }
}
sql <- paste(sql, ");")
cat(sql)
