install.packages("magrittr")
install.packages("dplyr")
install.packages("gganimate")
install.packages("pivottabler")
install.packages("kniter")
install.packages("rmarkdown")
install.packages("raster")
install.packages("rgdal")
install.packages("tibble")
install.packages("tidyverse")
install.packages(c("readxl","writexl")) 
install.packages("png")
install.packages("grid") 
install.packages("distill")
install.packages("tinytex")


tinytex::install_tinytex()

rm(list = ls())
library(readxl)
library(writexl)
library(tidyr)
library(ggplot2)
library(tibble)
library(dplyr)
library(tidyverse)
library(xlsx)
library(magrittr)

#Set directory to where the files are stored
setwd("D:/FRESENIUS/SEMESTER 2/Data Science/Data")
getwd()



# Import those files in R
webUsageExcelsLists <- list.files(pattern = ".xls")
webUsageExcelsLists

# Get the data of these Excels Files
webUsageDataLists = lapply(webUsageExcelsLists, function(i){
  # choose sheet number of that excel file
  x = read_excel(i, sheet=1)
  # choose columns of that excel file
  x <- x[,c(2,4,5,6,7,10)]
  x$file = i
  x
})
# Now we can access all the data from excel individually by:
webUsageDataLists[[5]]

# Binding the data from all the Excels
allUsageData = do.call("rbind.data.frame",webUsageDataLists)

#AVG. RESPONSE TIME for all available files
allUsageData$AVG_RESPONSE_TIME <- round(allUsageData$RESPONSE_TIME  / allUsageData$COUNTER, digits = 4)

# Create an Excel
#write_xlsx(allUsageData,"D:/FRESENIUS/SEMESTER 2/Data Science/Data/more data/allAPIResponse.xlsx")
#View(allUsageData)

data1 <- data.frame(OPERATION = allUsageData$OPERATION,
                    COUNTER = allUsageData$COUNTER,
                    RESPONSE_TIME = allUsageData$RESPONSE_TIME
)

#COUNTER vs OPERATION
usageCouterOperationBased <- aggregate(COUNTER ~ OPERATION, allUsageData, sum)

#RESPONSE TIME vs OPERATION
usageResponseTimeOperationBased <- aggregate(RESPONSE_TIME ~ OPERATION, allUsageData, sum)

# Collect all the data in one file
data2 <- data.frame(OPERATION <-  usageResponseTimeOperationBased$OPERATION,
                    COUNTER = usageCouterOperationBased$COUNTER,
                    RESPONSE_TIME = usageResponseTimeOperationBased$RESPONSE_TIME
)
#AVG. RESPONSE TIME vs OPERATION
data2$AVG_RESPONSE_TIME <- round(data2$RESPONSE_TIME  / data2$COUNTER, digits = 4)
data2

# Create an Excel
#write_xlsx(data2,"D:/FRESENIUS/SEMESTER 2/Data Science/Data/more data/yearBaseAvgAPIResponse.xlsx")

# Plot the Graphs
library(RCurl)
x <- getURL("https://raw.githubusercontent.com/adityadande/DataSciene/main/yearBaseAvgAPIResponse.csv")
graphData <- read.csv(text = x)

graphData %>%
  dplyr::arrange(graphData, desc(graphData$AVG_RESPONSE_TIME)) %>%    
  dplyr::mutate(name=factor(graphData$OPERATION....usageResponseTimeOperationBased.OPERATION, levels=graphData$OPERATION....usageResponseTimeOperationBased.OPERATION)) %>%  
  ggplot( aes(x=graphData$OPERATION....usageResponseTimeOperationBased.OPERATION, y=graphData$AVG_RESPONSE_TIME)) +
  geom_segment( aes(xend=graphData$OPERATION....usageResponseTimeOperationBased.OPERATION, yend=0)) +
  geom_point( size=4, color="orange") +
  coord_flip() +
  theme_bw() +
  xlab("OPERATIONS") +
  ylab("AVG RESPONSE TIME (sec)")  +
  geom_text(aes(label = signif(graphData$AVG_RESPONSE_TIME, digits = 5)), nudge_y = 0.1, color="blue")

# Users Analysis

library(RCurl)
x <- getURL("https://raw.githubusercontent.com/adityadande/DataSciene/main/allAPIResponse.csv")
apiData <- read.csv(text = x)

# Username vs Counter
agg_tbl <- apiData %>% dplyr::group_by(USERNAME) %>% dplyr::summarise(sum(COUNTER))
agg_tbl <- agg_tbl[order(-agg_tbl$`sum(COUNTER)`), ]
iris2 <- head(agg_tbl)

# Username vs No response
agg_tb2 <- apiData %>% dplyr::group_by(USERNAME) %>% dplyr::summarise(sum(NORESULTS))
agg_tb2 <- agg_tb2[order(-agg_tb2$`sum(NORESULTS)`), ]
iris3 <- head(agg_tb2)

knitr::kable( 
  list(
    iris2, iris3
  ), align = "lccrr", caption = "Users with the most API request & result", digits = 10, format.args = list(big.mark = ",",
                                                                                                            scientific = FALSE), booktabs = TRUE, valign = 't')
# NORESULT / COUNTER
apiData$NORESULT_AVG <- apiData$NORESULTS  / apiData$COUNTER
agg_tb3 <- apiData[,c(1,2,3,4,8)]
agg_tb3 <- agg_tb3 %>% dplyr::group_by(USERNAME) %>%       
  dplyr::summarise(sum(NORESULT_AVG)) 

iris4 <- head(agg_tb3[order(-agg_tb3$`sum(NORESULT_AVG)`),])

# BLOCKED / COUNTER
apiData$BLOCKED_AVG <- apiData$BLOCKED  / apiData$COUNTER
agg_tb4 <- apiData[,c(1,2,3,5,9)]
agg_tb4 <- agg_tb4 %>% dplyr::group_by(USERNAME) %>%       
  dplyr::summarise(sum(BLOCKED_AVG)) 

iris5 <- head(agg_tb4[order(-agg_tb4$`sum(BLOCKED_AVG)`),])

knitr::kable( 
  list(
    iris5, iris4
  ), align = "lccrr", caption = "Avg. Most NO RESULT & BLOCKED per COUNTER For each Customer Analysis", digits = 10, format.args = list(big.mark = ",",
 scientific = FALSE), booktabs = TRUE, valign = 't')
