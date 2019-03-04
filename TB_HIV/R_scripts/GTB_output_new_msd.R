# Loading packages

library(tidyverse)
library(janitor)
library(plotly)

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
# ============= Pulling in MSD PSNU-IM data ~~~~~~~=============
# ==================== NEW FORMAT FY19 =========================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rough data pull to get variable names and assign datatype

setwd("//cdc.gov/project/CGH_DGHT_GTB/PEPFAR TB_HIV Country Reports/Data_and_code")

foo <- read_tsv(file="MER/MER_Structured_Dataset_PSNU_IM_FY17-18_20181221_v2_1_Namibia_new_format.txt",
                col_names = TRUE)

foonames <- tolower(names(foo))

# Creating the vector of column types
colvecx <- as.vector(ifelse(grepl("qtr", foonames)|foonames=="targets"|foonames =="cumulative", "d", "c"))
colvec <- paste(colvecx, collapse = '')

# Pulling in the data with correct datatype for variables  
new <- read_tsv(file="MER/MER_Structured_Dataset_PSNU_IM_FY17-18_20181221_v2_1_new_format.txt",
                  col_names = TRUE,
                  col_types = colvec)

names(new) <- tolower(names(new))  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Restructuring & Recoding vars ~~~~~~~===========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# indicator list used
indlist <- c("TX_NEW",
             "TX_CURR",
             "TX_TB",
             "TB_PREV",
             "TB_STAT",
             "TB_ART"
)

# Restructuring the dataset
new1 <- subset(new, select=-c(qtr1, qtr3))%>%
  filter(indicator %in% indlist)%>% # filtering for indicators
  #going long by period
  gather(period, value, targets:cumulative)%>%
  #reconfiguring variable names
  mutate(fy = gsub("20", "fy", fiscal_year))%>%
  mutate(periodx = case_when(
    period == "qtr2" ~ "q2",
    period == "qtr4" ~ "q4",
    period == "cumulative" ~ "apr",
    period == "targets" ~ "targets"
    ))%>%
  mutate(fyper = paste(fy,periodx,sep=""))%>%
  #creating suffix variable for column names for disaggregates
  mutate(tbvars = case_when(
    indicator == "TX_TB" & numeratordenom == "D" ~
      case_when(
        otherdisaggregate %in% c("Life-long ART, Already, TB Screen - Negative") ~ "TBneg",
        otherdisaggregate %in% c("Life-long ART, Already, TB Screen - Positive") ~ "TBpos",
        otherdisaggregate %in% c("Life-long ART, New, TB Screen - Negative")     ~ "TBneg",
        otherdisaggregate %in% c("Life-long ART, New, TB Screen - Positive")     ~ "TBpos",
        otherdisaggregate == "Positive"     ~ "TBpos",
        otherdisaggregate == "Negative"     ~ "TBneg",
        standardizeddisaggregate == "Specimen Sent/HIVStatus" ~ "sent",
        standardizeddisaggregate == "Specimen Sent" ~ "sent"
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
      ),
    indicator == "TB_STAT" &  
      numeratordenom == "N" ~ 
      case_when(
        grepl("Positive", categoryoptioncomboname)    ~  "HIVpos",
        grepl("Negative", categoryoptioncomboname)     ~  "HIVneg"
      )
  ))%>%
  mutate(colvar = case_when(
    standardizeddisaggregate=="Total Numerator" | 
      standardizeddisaggregate=="Total Denominator"~ 
      paste(fyper, indicator, numeratordenom, sep="_"),
    indicator == "TB_STAT" &
      standardizeddisaggregate == "Age Aggregated/Sex/KnownNewPosNeg" ~ 
      paste(fyper, indicator, tbvars, sep="_"),
    indicator == "TX_TB" & 
      standardizeddisaggregate == "PositiveScreen" |
      standardizeddisaggregate == "TBScreen/NewExistingART/HIVStatus"  ~
      paste(fyper, indicator, tbvars, sep="_"),
    indicator == "TX_TB" & 
      standardizeddisaggregate == "Specimen Sent/HIVStatus" |
      standardizeddisaggregate == "Specimen Sent"  ~
      paste(fyper, indicator, tbvars, sep="_")
  ))

new2 <- new1%>%  
  select(operatingunit, 
         countryname,
         snu1,
         psnu, 
         fundingagency,
         primepartner,
         colvar,
         value)


# Aggregating data by required categorical variables and going wide
wide <- new2 %>% 
  group_by_if(is.character) %>% 
  summarise(val = sum(value, na.rm=T)) %>% 
  ungroup() %>% 
  spread(colvar, val)

#calculating ou-level indicators
ind_calc <- wide%>%
  mutate(fy18_TB_STAT = fy18apr_TB_STAT_N/fy18apr_TB_STAT_D ,
         fy17_TB_STAT = fy17apr_TB_STAT_N/fy17apr_TB_STAT_D ,
         fy18_TB_PREV = fy18apr_TB_PREV_N/fy18apr_TB_PREV_D,
         fy17_TB_PREV = fy17apr_TB_PREV_N/fy17apr_TB_PREV_D,
         fy18_TB_ART  = fy18apr_TB_ART_N/fy18apr_TB_STAT_HIVpos,
         fy17_TB_ART  = fy17apr_TB_ART_N/fy17apr_TB_STAT_HIVpos
  )
is.na(ind_calc) <- sapply(ind_calc,is.infinite)
is.na(ind_calc) <- sapply(ind_calc,is.nan)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#======= Country-level datasets =================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# One country at a time
ou_test <- wide%>%
filter(operatingunit =="Malawi")
namnames <- names(ou_test)

# Splitting dataset into OU datasets- ALL
ou_split <- split(wide, wide$operatingunit)
ou_names <- unlist(distinct(wide,operatingunit))
for (i in 1:length(ou_split)) {
  assign(ou_names[i], ou_split[[i]])
}

# List of GTB 14 high-priority countries
hipri <- c("Botswana","Eswatini", "Kenya", "Lesotho", "Malawi", "Mozambique", "Namibia",
              "Nigeria", "Rwanda", "South Africa", "Tanzania", "Uganda", "Zambia", "Zimbabwe")

test <- c("Nigeria", "South Africa", "Namibia")

# Splitting dataset into OU datasets- 14 HIPRI
wide1 <- filter(wide, operatingunit %in% hipri)
ou_split1 <- split(wide1, wide1$operatingunit)
ou_names1 <- unlist(distinct(wide1,operatingunit))
for (i in 1:length(ou_split1)) {
  assign(ou_names1[i], ou_split1[[i]])
}

# Trying to split here and feed into Rmd
wide2 <- filter(wide,operatingunit %in% test)
ou_split2 <- split(wide2, wide2$operatingunit)
ou_names2 <- unlist(distinct(wide2,operatingunit))
for (i in 1:length(ou_split2)) {
  assign(ou_names2[i], ou_split2[[i]])
  rmarkdown::render('//cdc.gov/project/CGH_DGHT_GTB/PEPFAR TB_HIV Country Reports/data_and_code/loop_test.Rmd',  # file 2
                    output_file =  paste("report_", operatingunit, '_', Sys.Date(), ".html", sep=''), 
                    output_dir = '//cdc.gov/project/CGH_DGHT_GTB/PEPFAR TB_HIV Country Reports/Data_and_code')
}

  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Creating dataset with OU-level indicators- NEW STRUCTURE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

oulevel <- new2%>%
  group_by(operatingunit, colvar)%>%
  summarise(outotal = sum(value, na.rm = TRUE )) %>% 
  ungroup()

oulevelw <- oulevel %>% 
  group_by_if(is.character) %>% 
  summarise(val = sum(outotal, na.rm=T)) %>% 
  ungroup() %>% 
  spread(colvar, val)%>%
  mutate(fy18_TB_STAT = fy18apr_TB_STAT_N/fy18apr_TB_STAT_D ,
         fy17_TB_STAT = fy17apr_TB_STAT_N/fy17apr_TB_STAT_D ,
         fy18_TB_PREV = fy18apr_TB_PREV_N/fy18apr_TB_PREV_D,
         fy17_TB_PREV = fy17apr_TB_PREV_N/fy17apr_TB_PREV_D,
         fy18_TB_ART  = fy18apr_TB_ART_N/fy18apr_TB_STAT_HIVpos,
         fy17_TB_ART  = fy17apr_TB_ART_N/fy17apr_TB_STAT_HIVpos
  )
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Creating dataset with agency indicators- NEW STRUCTURE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

agencylev <- new2%>%
  group_by(fundingagency, colvar)%>%
  summarise(outotal = sum(value, na.rm = TRUE )) %>% 
  ungroup()

agencyw <- agencylev %>% 
  group_by_if(is.character) %>% 
  summarise(val = sum(outotal, na.rm=T)) %>% 
  ungroup() %>% 
  spread(colvar, val)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Creating list of all disaggs/combo options
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

disaggs <- new1%>% 
  select(indicator, numeratordenom,otherdisaggregate, standardizeddisaggregate,categoryoptioncomboname, value)%>% 
  filter(!is.na(value))%>%
  distinct(indicator, numeratordenom, otherdisaggregate, standardizeddisaggregate, categoryoptioncomboname)%>%
  arrange(indicator, numeratordenom, standardizeddisaggregate)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ======== Feeding country datasets into markdown ~~~~~=========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Making a report for each OU
# these reports are saved in output_dir with the name specified by output_file
for (operatingunit in unique(rownames(wide2))){
  rmarkdown::render('//cdc.gov/project/CGH_DGHT_GTB/PEPFAR TB_HIV Country Reports/data_and_code/loop_test.Rmd',  # file 2
                    output_file =  paste("report_", operatingunit, '_', Sys.Date(), ".html", sep=''), 
                    output_dir = '//cdc.gov/project/CGH_DGHT_GTB/PEPFAR TB_HIV Country Reports/Data_and_code')
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Exporting the data into CSV/Excel ~~~~~=========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dx <- as.character(format(Sys.time(), "%Y %b %d"))
t <- as.character(format(Sys.time(), "%H %M %S" ))
tm <- str_replace_all(t, "[ ]", "_")
dt <- str_replace_all(dx, "[ ]", "")

# CSV for all ou's at ou level
write_csv(oulevelw, 
          paste(dt, "_ou_level_new", tm, ".csv", 
                sep="" ), na="")

# CSV files for each OU -ALL
dir.create(paste(dt,"_OU_datasets_",tm,sep=""))
dfs <- split(wide, wide$operatingunit) # list of dfs
lapply(ou_names,
       function(x){write.csv(dfs[[x]],
                             file = paste0(dt,x,"_new_", tm,".csv",sep=""), na="",
                             row.names = FALSE)})

# CSV files for each OU- 14 hipri
dir.create(paste(dt,"_OU_datasets_",tm,sep=""))
dfs1 <- split(wide1, wide1$operatingunit) # list of dfs
lapply(ou_names1,
       function(x1){write.csv(dfs1[[x1]],
                             file = paste0(dt,x1,"_new_", tm,".csv",sep=""), na="",
                             row.names = FALSE)})

# CSV file for single OU
write_csv(ou_test,
          paste(dt, ouname, "_new_", tm, ".csv", sep=""), na="")
