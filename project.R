# German 108 Final Project
# German voting data choropleth

library(sp)
library(maptools)
library(rMaps)
library(rgdal)
library(plotly)
library(ggmap)
library(ggplot2)
library(plotly)
setwd("~/Documents/GitHub/german-refugee")
data2 <- read.csv("arvig.csv")
data <- read.csv("data.csv", header = F)
names(data) <- c("state","afd","ref_per")
gadm <- readRDS("DEU_adm1.rds")

gadm@data$afd <- data$afd
gadm@data$ref_per <- data$ref_per


gadm@data$id <- rownames(gadm@data)

# create a data.frame from our spatial object
gadm_fort <- fortify(gadm, region = "id")

gadm_df <- merge(gadm_fort, gadm@data, by = "id")

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Helvetica", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 3),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA),
      legend.box = "vertical",
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      #legend.spacing.y = .5,
      legend.key = element_blank(),
      panel.border = element_blank(),
      ...
    )
}

attack <- 
  ggplot() +
  theme_map() +
  geom_polygon(data = gadm_df,
               aes(x = long, y = lat, group = group, fill = ref_per), 
               color = "white", size = 0.25) + 
  scale_fill_distiller(name="Percent of Total Refugees", palette = 1, direction=1)+
  geom_point(data =data2, size=.75,
             aes(x=longitude, y = latitude, col = category_en, label=state, label2=location, label3=date, label4=source))+
  coord_map()+
  labs(x=NULL,
       y=NULL,
       title="Anti-Refugee Attacks",
       subtitle="Each dot represents an action commited against refugees",
       color = "Type of Attack")

plot <- ggplotly(attack, width = 1100, height = 900)

