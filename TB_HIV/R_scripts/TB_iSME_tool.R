library(tidyverse)
library(leaflet)
library(readr)
library(rgdal)
library(sp)
library(rgeos)
library(RColorBrewer)
library(crosstalk)
library(ggplot2)
library(DT)
library(shiny)
library(scales)
library(htmltools)
library(htmlwidgets)
library(r2d3)

# read shape files as Spatial Polygon data for PSNU & OU
got.psnu <- readOGR(dsn='RawData/GoT_PSNUs', layer = 'GoT_PSNUs')
 
got.regions <- readOGR(dsn= 'RawData/GoT_Regions', layer= 'GoT_Regions')

## ---- Examine Spatial Polygon DF ----
# lets see what the spatial polygon dataframes look like
summary(got.psnu)
summary(got.regions)


## --- CRS Projections ----

# Set CRS to standard WGS84 
# You can check WGS 
proj4string(got.psnu)<- CRS("+proj=longlat +datum=WGS84 +no_defs")
proj4string(got.regions)<- CRS("+proj=longlat +datum=WGS84 +no_defs")


# Getting the Geographical data 
s <- readr::read_tsv("RawData/ICPI_TRAINING_GoT_site_lat_long_20181012.txt")

# Creating implementing partner and agency data
ims <- toupper(letters[c(1:5)])
mechs <- paste("Partner", ims, sep="_")

# randomly allocating binary one or zero to partner columns
Partner_A <- sample( c("X", "Y"), nrow(s), replace=TRUE, prob=c(0.6, 0.4) )
Partner_B <- sample( c("X", "Y"), nrow(s), replace=TRUE, prob=c(0.4, 0.6) )
Partner_C <- sample( c("X", "Y"), nrow(s), replace=TRUE, prob=c(0.5, 0.5) )
Partner_D <- sample( c("X", "Y"), nrow(s), replace=TRUE, prob=c(0.7, 0.3) )
Partner_E <- sample( c("X", "Y"), nrow(s), replace=TRUE, prob=c(0.3, 0.7) )

sp <- cbind(s, 
            Partner_A,
            Partner_B,
            Partner_C,
            Partner_D,
            Partner_E) %>% 
  gather(partner, val, 
         Partner_A,
         Partner_B,
         Partner_C,
         Partner_D,
         Partner_E) %>% 
  filter(val=="Y")

# random sample of size 8 from sequence [5, 15]
set.seed(124)

# no of rows in the dataframe
n_sp <- nrow(sp)

# Creating the TB variables:

# TB_STAT_D
# TB_STAT_N
# TB_STAT_N_target
# TB_STAT_POS
# TB_ART

# TX_CURR
# TX_TB_D
# TX_TB_D_POS
# TX_TB_N
# TX_TB_D_NEG
# TX_TB_N
# TB_PREV_D
# TB_PREV_N





# New and relapsed TB cases
TB_STAT_D <- sample(seq(100, 3000), n_sp)
# % of New and relapsed TB cases with documented HIV status
tb_stat_r <- sample(seq(.70, .99, by=.001), n_sp, replace=T)
TB_STAT_N <- round(TB_STAT_D*tb_stat_r, 0)

tb_stat_ax <- sample(seq(.70, 1.3, by=.001), n_sp, replace=T)

TB_STAT_N_tr <- round(TB_STAT_N*tb_stat_ax, 0)

# The yield of TB cases that know status
yld <- sample(seq(.04, .15, by=.001), n_sp, replace=T)

TB_STAT_POS <- round(TB_STAT_N*yld, 0)

# of the TB cases who are HIV positive, how many put on Tx
linkg <- sample(seq(.50, .95, by=.001), n_sp)

TB_ART <- round(TB_STAT_POS*linkg, 0)

tb_art_ax <- sample(seq(.70, 1.3, by=.001), n_sp, replace=T)
TB_ART_Tr <- round(TB_ART*tb_art_ax, 0)


spx <- cbind(sp, 
             TB_STAT_D, 
             TB_STAT_N,
             tb_stat_r,
             TB_STAT_N_tr,
             tb_stat_ax,
             TB_STAT_POS,
             yld,
             TB_ART,
             TB_ART_Tr,
             tb_art_ax
             ) %>% 
  filter(f_c=="facility") %>% 
  select(-c(f_c, val, PSNUuid)) %>% 
  mutate(idvar = paste(sitename, partner))

sitecoord <- spx %>% select(sitename, lat, long) %>% unique()

sx <- spx %>% mutate(partner = "_All_") %>% 
  select(PSNU, sitename, partner, 
         TB_STAT_D, 
         TB_STAT_N,
         TB_STAT_N_tr,
         TB_STAT_POS,
         TB_ART,
         TB_ART_Tr) %>% 
  group_by(PSNU, sitename, partner) %>% 
  summarize_if(is.numeric, funs(sum), na.rm=T) %>% 
  mutate(tb_stat_r = TB_STAT_N/TB_STAT_D,
         tb_stat_ax = TB_STAT_N/TB_STAT_N_tr,
         yld = TB_STAT_POS/TB_STAT_N,
         tb_art_ax = TB_ART/TB_ART_Tr) %>% 
  ungroup() %>% 
  mutate(idvar = paste(sitename, partner))


# Merge back the lat long
sx1 <- left_join(sx, sitecoord)

# Overall dataset containing data by each partner and all
df_final <- bind_rows(sx1, spx) %>% 
  arrange(partner, desc(TB_STAT_N)) %>%
  group_by(partner) %>% 
  mutate(tb_stat_n_rank = row_number()) %>% 
  ungroup() %>% 
  arrange(partner, desc(TB_ART)) %>%
  group_by(partner) %>% 
  mutate(tb_art_rank = row_number()) %>% 
  ungroup() %>% 
  group_by(partner) %>%
  mutate(countX= sum(TB_STAT_N, na.rm=T)) %>%
  group_by(sitename, add=TRUE) %>%
  mutate(tb_stat_volpercent=100*TB_STAT_N/countX) %>% 
  ungroup() %>% 
  group_by(partner) %>%
  mutate(countT= sum(TB_ART, na.rm=T)) %>%
  group_by(sitename, add=TRUE) %>%
  mutate(tb_art_volpercent=100*TB_ART/countT) %>% 
  ungroup() %>% 
  group_by(partner) %>% 
  mutate(cum_tb_art_volp = cumsum(tb_art_volpercent)) %>% 
  ungroup() %>% 
  select(-countT) %>% 
  mutate(tags = "") %>% 
  mutate(tb_stat_r = round(100*tb_stat_r, 1),
         tb_stat_ax = round(100*tb_stat_ax, 1),
         yld = round(100*yld, 1),
         tb_art_ax = round(100*tb_art_ax, 1),
         cum_tb_art_volp = round(cum_tb_art_volp, 1),
         tb_stat_volpercent = round(tb_stat_volpercent, 1)
         )
  


### Creating the shared dataset for table, map, and graph
shrd <- SharedData$new(df_final, key = ~idvar)


tbl <-  SharedData$new(df_final %>% 
                             select(sitename, tags, PSNU, partner, 
                                    TB_STAT_D, TB_STAT_N, tb_stat_r,
                                    tb_stat_n_rank, tb_stat_volpercent,
                                    TB_STAT_N_tr, tb_stat_ax,
                                    TB_STAT_POS, yld, 
                                    TB_ART, TB_ART_Tr, tb_art_ax,
                                    tb_art_rank, cum_tb_art_volp, idvar), 
                           key = ~idvar, shrd$groupName())



dtx <-     
  datatable(tbl, editable = F,
            caption = 'Table: Site-IM level dataset for TB cascade',
            rownames=F,
            colnames = c("Site"                   = 1,
                         "tags"                   = 2,
                         "PSNU"                   = 3,
                         "Partner"                = 4,
                         "TB_STAT Den."    = 5,
                         "TB_STAT Num."      = 6,
                         "% HIV stat. known"     = 7,
                         "TB_STAT vol. rank"    = 8,
                         "TB_STAT % vol."       =9,
                         "TB_STAT N targets"  = 10,
                         "TB_STAT % achv."       = 11,
                         "TB_STAT_POS"           = 12,
                         "Yield %"               = 13,
                         "TB_ART"                = 14,
                         "TB_ART targets"        = 15,
                         "TB_ART % achv."        = 16,
                         "TB_ART vol. rank"      = 17,
                         "TB_ART cum. vol %"     = 18,
                         "ID"                    = 19
                         ),
            extensions = c('Buttons', "Scroller", 'ColReorder'), 
            filter = 'top',
            options = list(
              colReorder = TRUE,
              dom = 'Bfrtip',
              buttons = 
                list(I('colvis'), list(
                  extend = 'collection',
                  buttons = c('csv', 'excel', 'pdf'),
                  text = 'Download'
                )),
              deferRender=TRUE, scrollY=600, scroller=TRUE), 
            style="bootstrap",
            class="compact", width="100%", height = '30%') %>% 
  formatString(suffix = "%",columns = c("% HIV stat. known",
                                        "TB_STAT % vol.",
                                        "TB_STAT % achv.",
                                        "Yield %",
                                        "TB_ART % achv.",
                                        "TB_ART cum. vol %"))



#  pallate based on ART coverage name
colfn <- colorRampPalette(c('#335b8e', 
                            '#6ca18f', 
                            '#b5b867',
                            '#cc5234', 
                            '#d9812c', 
                            '#948d79'))(11)
pal <- colorFactor(palette = colfn, domain = got.psnu$PSNU)

palx <- colorFactor(palette = colfn, domain = shrd$PSNU)


state_popup <- paste0("<strong>PSNU: </strong>", 
                      got.psnu$PSNU)

#---------------------------------------------------------------------------------
# ---- Final Map ----- 
#---------------------------------------------------------------------------------

# Checking all the data 
icons <- awesomeIcons(
  icon = 'hospital-o',
  iconColor = palx(shrd$PSNU),
  library = 'fa',
  markerColor = "white"
)



basemap <- leaflet(got.regions) %>%   
  addPolygons(color='grey', weight=4, opacity=.5, fillOpacity = 0) %>% 
  addPolygons(fillColor=~pal(PSNU),
              data=got.psnu, color='blue', weight=1, opacity=.7, fillOpacity = 0.7, 
              popup = state_popup) %>% 
  addAwesomeMarkers(data=shrd, lng=~long,lat=~lat,
                    icon=icons,
                    label=~paste(sitename, ", PSNU:", PSNU, sep=""))
  # addCircleMarkers(data = shrd, lat = ~lat, lng = ~long, popup = ~sitename, radius = 0.7)

f1 <- filter_checkbox("partner", "Partner", shrd, 
                      ~partner, inline=T)
f2 <- filter_checkbox("PSNU", "PSNU", shrd, 
                      ~PSNU, inline=T)
brk0 <- bscols(div(style = 
                     css(width="100%", height="50px", background_color=NA)))

title1 <- bscols("Table for Site Drill-Down")


tbx <- bscols(list(f1, f2, brk0, title1, dtx, basemap))

htmltools::save_html(tbx, "TB_GoT_table_map.html", background = "white", libdir = "dependencies")


#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------







# 
# # Aggregate up to PSNU level to get PSNU color background
# s_psnu <- spx %>% 
#   group_by(PSNU) %>% 
#   summarize_at(vars("TB_STAT_D", "TB_STAT_N"), funs(sum), na.rm=T) %>% 
#   ungroup() %>% 
#   mutate(tb_stat_percent = TB_STAT_N/TB_STAT_D)
# 
# 
# intersect(names(got.psnu@data), names(s_psnu))
# 

# # now merge the new wide df with spatial df
# got.psnu.msd <- merge(got.psnu, s_psnu, by="PSNU")

# #check if the data was merged
# head(got.psnu.msd@data,5)

# Create a choropleth on TX_CURR FY2018Q1 values by PSNU

# # first create a color scale based on TX_CURR values; use colorBin 
# col_tb <- colorBin(palette = 'YlOrRd', domain = got.psnu.msd@data$tb_stat_percent)


# -- 10. Lengend - add legend to the map ----
map_tb <- leaflet() %>%  
  addPolygons(data=got.regions, color = 'red', fillOpacity = 0) %>%
  addPolygons(data=got.psnu.msd, color='black', weight=2, opacity=.8, 
              fillColor = ~col_tb(tb_stat_percent), fillOpacity = .7, 
              label = ~PSNU) %>% 
  addLegend('bottomright', pal = col_tb, values = got.psnu.msd@data$tb_stat_percent, 
            title = '% TB case with known HIV status')

map_tb

## --- 11. Point Data ----

myColors <- c("#335B8E",
              "#B5B867",
              "#CC5234",
              "#D9812C",
              "#948D79",
              "#A379BB")

names(myColors) <- levels(drc_shrd$newpartnername)
colScale <- scale_colour_manual(name = "newpartnername", values = myColors)

## Creating the cross-reactive table with map and scatter graph


# Add Facility markers in each PSNU
map_txcurr %>% 
  addMarkers(data = spx, lat = ~lat, lng = ~long, popup = ~PSNU) # you can use a non saptial df here, just need to point to lat/long variables in the df

# Change Markers
map_txcurr %>% 
  addCircleMarkers(data = fac_xy, lat = ~lat, lng = ~long, popup = ~sitename, radius = 0.7)

# Display Facilities in the North PSNU
map_txcurr %>% 
  addCircleMarkers(data = fac_xy %>% filter(PSNU=='The North'), lat = ~lat, lng = ~long, popup = ~sitename, radius = 1)


# Change the radius of facility markers based on their HTS_TST_POS values 

map_txcurr %>% 
  addCircleMarkers(data = fac_xy %>% filter(PSNU== c('The North', 'Riverlands')), lat = ~lat, lng = ~long, 
                   popup = ~sitename, radius = ~(hts_tst_pos)/50, fillOpacity = .8)


## -- Exercise 3: ----
# Display facilities in the southern part of the country i.e. latitude < 0




## -- 13. Awesome Markers ----
 # let's have some fun with Markers
# create icons based on facility
# checkout the list of options here: https://github.com/lvoogdt/Leaflet.awesome-markers 

icons <- awesomeIcons(
  icon = ifelse(fac_xy$f_c=="facility",'hospital-o', 'ambulance'),
  library = 'fa',
  markerColor = "white"
)

map_txcurr %>% 
  addAwesomeMarkers(data = fac_xy %>% filter(PSNU== c('The North', 'Riverlands')), lat = ~lat, lng = ~long, 
                   popup = ~sitename, icon = icons)


## -- 12. Saving Leaflet Maps ----
# Leaflet maps can be saved as PDF, JPEG or interactive HTML files
# HTML files are stored as web pages that can be opened in any browser

htmlwidgets::saveWidget(map_txcurr, 'Output/map_txcurr.html') # you can do the same by clicking on Export in the Viewer window
