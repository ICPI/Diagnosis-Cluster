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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Creating basic functions to show top few rows of data
View50 <- function(x){View(x[1:50,])}
View100 <- function(x){View(x[1:100,])}

# Creating the 'not in' function
`%ni%` <- Negate(`%in%`) 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Pulling in MSD PSNU-IM data ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Restructuring & Recoding vars ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# indicator list used
indlist <- c("TX_NEW",
             "TX_CURR",
             "TX_TB",
             "TB_PREV"
             )


# Restructuring the dataset
datim1 <- datim %>% 
  filter(indicator %in% indlist) %>% # filtering for indicators
  # going long by period
  gather(period, value, foonames[grepl("fy20", foonames)]) %>% 
  # Only keeping required periods
  filter(period %in% c("fy2019_targets",
                       "fy2018apr",
                       "fy2017apr")) %>% 
  #Changing the periods to shorter versions
  mutate(periodx = gsub("20", "", period)) %>% 
  # Creating suffix variable for future column names 
  mutate(tbvars = case_when(
    indicator == "TX_TB" & numeratordenom == "D" ~
      case_when(
       otherdisaggregate %in% c("Life-long ART, Already, TB Screen - Negative") ~ "oldart_neg",
       otherdisaggregate %in% c("Life-long ART, Already, TB Screen - Positive") ~ "oldart_pos",
       otherdisaggregate %in% c("Life-long ART, New, TB Screen - Negative")     ~ "newart_neg",
       otherdisaggregate %in% c("Life-long ART, New, TB Screen - Positive")     ~ "newart_pos"
               ),
    indicator == "TX_TB" & numeratordenom == "N" ~
      case_when(
        categoryoptioncomboname %in% c("Life-long ART, Already, Positive") ~ "oldart",
        categoryoptioncomboname %in% c("Life-long ART, New, Positive")     ~ "newart"
                ),
    indicator == "TB_PREV" &  
      standardizeddisaggregate == "TherapyType/NewExistingArt/HIVStatus" ~ 
      case_when(
        grepl("Already", categoryoptioncomboname)      ~  "oldart",
        grepl("New", categoryoptioncomboname)          ~  "newart"
               )
  )) %>% 
  # Creating the final column variable names
  mutate(colvar = case_when(
    indicator %in% c("TX_TB", "TB_PREV") & 
      standardizeddisaggregate %ni% c("Total Numerator") ~ 
      paste(periodx, indicator, numeratordenom, tbvars, sep="_"),
    indicator %in% c("TX_CURR", "TX_NEW", "TB_PREV") & 
      standardizeddisaggregate=="Total Numerator" ~ 
      paste(periodx, indicator, numeratordenom, sep="_")
  )) %>% 
  # Keeping on the combinations you need
  filter(colvar %in% c(
    "fy19_targets_TX_NEW_N",
    "fy18apr_TX_TB_D_newart_neg",
    "fy18apr_TX_TB_D_newart_pos",
    "fy18apr_TX_TB_N_newart",
    "fy18apr_TB_PREV_D_newart",
    "fy18apr_TB_PREV_N_newart",
    "fy18apr_TX_CURR_N",
    "fy19apr_targets_TX_CURR",
    "fy18apr_TX_TB_D_oldart_neg",
    "fy18apr_TX_TB_D_oldart_pos",
    "fy18apr_TX_TB_N_oldart",
    "fy18apr_TB_PREV_D_oldart",
    "fy18apr_TB_PREV_N_oldart",
    "fy19_targets_TX_NEW_N",
    "fy19_targets_TX_CURR_N",
    "fy19_targets_TB_PREV_N",
    "fy18apr_TB_PREV_N",
    "fy17apr_TB_PREV_N"
  )) %>% 
  # Select final categorical variables
  select(operatingunit, 
         countryname,
         snu1,
         psnu, 
         psnuuid,
         fundingagency,
         primepartner,
         mechanismid,
         implementingmechanismname,
         colvar,
         value)


# Aggregating data by required categorical variables and going wide
w <- datim1 %>% 
  group_by_if(is.character) %>% 
  summarise(val = sum(value, na.rm=T)) %>% 
  ungroup() %>% 
  spread(colvar, val) %>% 
  mutate(i_fy20_targets_TX_NEW = NA,
         i_TX_TB_D_TX_prop     = NA    ) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Exporting the data into CSV/Excel ~~~~~=========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dx <- as.character(format(Sys.time(), "%Y %b %d"))
t <- as.character(format(Sys.time(), "%H %M %S" ))
tm <- str_replace_all(t, "[ ]", "_")
dt <- str_replace_all(dx, "[ ]", "")

write_csv(w, 
          paste("./TB_HIV/Output/", dt, "_TB_targets_draft_", tm, ".csv", 
                sep="" ), na="")

  
  









