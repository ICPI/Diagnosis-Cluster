# Script to pull in PSNU-IM dataset and create dataset for
# TB target setting tool
# Programmer: Imran Mujawar, lrz5@cdc.gov
# Date: 11/12/18
# Date modified: 

# Loading the TidyVerse package
if(!require(tidyverse)){
  install.packages("tidyverse")
}
library(tidyverse)


# Rough data pull to get variable names and assign datatype
foo <- read_tsv(file="./TB_HIV/RawData/MER_Structured_Dataset_PSNU_IM_FY17-18_20181115_v1_2.txt",
                col_names = TRUE)

foonames <- tolower(names(foo))

# Creating the vector of column types
colvecx <- as.vector(ifelse(grepl("fy20", foonames), "d", "c"))
colvec <- paste(colvecx, collapse = '')


# Pulling in the data with correct datatype for variables  
datim <- read_tsv(file="./TB_HIV/RawData/MER_Structured_Dataset_PSNU_IM_FY17-18_20181115_v1_2.txt",
                  col_names = TRUE,
                  col_types = colvec)

names(datim) <- tolower(names(datim))  


# Creating the following variables
fy19_target_TX_NEW
i_fy20_target_TX_NEW     keep blank
i_TX_TB_D_TX_prop        keep blank
fy18_TX_TB_D_newart_neg   
fy18_TX_TB_D_newart_pos
fy18_TX_TB_N_newart
fy18_TB_PREV_D_newart
fy18_TB_PREV_N_newart
fy18_TX_CURR
fy19_TX_CURR_TARGET








