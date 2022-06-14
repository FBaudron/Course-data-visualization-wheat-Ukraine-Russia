
setwd('C:\\Users\\FBaudron\\Documents\\R\\0. Dataviz wheat\\')

wt <- read.csv("wheat.csv")
rg <- read.csv("fao countries.csv")
cp <- read.csv("commodity_prices.csv")
ex <- read.csv("export.csv")
co <- read.csv("cereal_comp.csv")
wg <- read.csv("wheat gap.csv")
tm <- read.csv("trade matrix.csv")
wi <- read.csv("wheat import countries.csv")


# UPLOADING REQUIRED PACKAGES---------------------------------------------------

library(ggplot2)
library(ggthemes)
library(dplyr)
library(scales)
library(jcolors)
library(viridis)
library(packcircles)
library(spData)
library(sf)
library(transformr)
library(gganimate)


# PREPARATION OF DATA FILES-----------------------------------------------------

names(cp) <- cp[2,]
wp <- cp[-c(1:2),] 
wp$Date <- as.Date(wp$Date, format = "%m/%d/%Y")
wp <- wp[,c(1,38)]
wp <- wp[wp$Date > "1982-03-31",]
wp$Date <- as.Date(wp$Date, format = "%Y %m")
wp$WHEAT_US_HRW <- as.numeric(wp$WHEAT_US_HRW)


names(wt)[3] <- "Code"
names(rg)[1] <- "Code"
wtrg <- merge(wt, rg, by = "Code", all.x = TRUE)
wtaf <- wtrg[ which(wtrg$UNSD_MACRO_REG == "Africa"), ]
wtafimp <- wtaf[ which(wtaf$Element == "Import Quantity"), ]
wtafimp19 <- wtafimp[ which(wtafimp$Year == "2019"), ]
wtafimp19 <- wtafimp19[,c(4,12,17)]
names(wtafimp19)[3] <- "Region"
wtafimp19 <- unique(wtafimp19)


wg <- wg[,c(4,6,10,12)]
wg <- subset(wg, subset = Element == "Import Quantity" | Element ==  "Production")


ex <- ex[, c(4,12)]
ex$region <- rep("F", nrow(ex))
ex$region <- ifelse(ex$Area == "Russian Federation" | ex$Area == "Ukraine", "T", ex$region)
ex$id[order(-ex$Value)] <- 1:nrow(ex)
ex <- ex[ex$Value > 0,]
ex$Area <- ifelse(ex$Area == "Russian Federation", "Russia", ex$Area)
ex$Area <- ifelse(ex$Area == "United States of America", "USA", ex$Area)
ex$Area <- ifelse(ex$Area == "United Kingdom of Great Britain and Northern Ireland", "UK", ex$Area)
ex$Area2 <- ifelse(ex$Value > 1000, ex$Area, "")


co <- t(co)
co <- as.data.frame(co)
names(co) <- co[1,]
co <- co[-c(1),] 
co$`Energy (cal)` <- as.numeric(co$`Energy (cal)`)
co$`Carbohydrates (g)` <- as.numeric(co$`Carbohydrates (g)`)
co$`Water (g)` <- as.numeric(co$`Water (g)`)
co$`Protein (g)` <- as.numeric(co$`Protein (g)`)
co$`Fat (g)` <- as.numeric(co$`Fat (g)`)
co$`Ash (g)` <- as.numeric(co$`Ash (g)`)
co$`Starch fiber (g)` <- as.numeric(co$`Starch fiber (g)`)
co$`Phosphorous (mg)` <- as.numeric(co$`Phosphorous (mg)`)
co$`Calcium (mg)` <- as.numeric(co$`Calcium (mg)`)
co$`Niacin (mg)` <- as.numeric(co$`Niacin (mg)`)
co$`Iron (mg)` <- as.numeric(co$`Iron (mg)`)
co$`Thiamine (mg)` <- as.numeric(co$`Thiamine (mg)`)
co$`Riboflafin (mg)` <- as.numeric(co$`Riboflafin (mg)`)
range01 <- function(x){x/max(na.omit(x))}
co$`Energy (cal)` <- range01(co$`Energy (cal)`)
co$`Carbohydrates (g)` <- range01(co$`Carbohydrates (g)`)
co$`Water (g)` <- range01(co$`Water (g)`)
co$`Protein (g)` <- range01(co$`Protein (g)`)
co$`Fat (g)` <- range01(co$`Fat (g)`)
co$`Ash (g)` <- range01(co$`Ash (g)`)
co$`Starch fiber (g)` <- range01(co$`Starch fiber (g)`)
co$`Phosphorous (mg)` <- range01(co$`Phosphorous (mg)`)
co$`Calcium (mg)` <- range01(co$`Calcium (mg)`)
co$`Niacin (mg)` <- range01(co$`Niacin (mg)`)
co$`Iron (mg)` <- range01(co$`Iron (mg)`)
co$`Thiamine (mg)` <- range01(co$`Thiamine (mg)`)
co$`Riboflafin (mg)` <- range01(co$`Riboflafin (mg)`)
co$Cereal <- rownames(co)
co <- gather(co, Contents, Value, "Energy (cal)":"Riboflafin (mg)")


# LINE CHART--------------------------------------------------------------------

ggplot(wp, aes(x = Date, y = WHEAT_US_HRW)) + 
  theme_few() +
  xlab("") + ylab("Price of US  hard red winter wheat (USD/mt)") +
  scale_x_date(breaks = date_breaks("4 years"), labels = date_format("%Y"),
               limits = c(min(wp$Date), max = max(wp$Date)), expand = c(0,0)) +
  geom_line(size = 1.2, color = "#CEFF1A") + 
  labs(title = "Monthly wheat price in the last 40 years",
       caption = "Source: World Bank Commodity Price Data")+ 
  theme(plot.margin = margin(20, 20, 20, 20),
        axis.title.y = element_text(size = 24, face = "bold", color = "grey90", margin = margin(0,20,0,0)),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 20,  face = "bold", color = "grey90"),
        plot.title = element_text(color = "grey90", size = 30, face = "bold", hjust=0,
                                  margin = margin(0,0,20,0)),
        plot.caption = element_text(color = "grey90", size = 16, face = "italic", hjust=1,
                                    margin = margin(20,0,0,0)),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey10"),
        panel.border = element_blank(),
        axis.ticks = element_line(color = "grey90"),
        axis.line.x.bottom=element_line(color = "grey90"),
        axis.line.y.left=element_line(color = "grey90")) 

# ggsave("Monthly wheat prices.jpeg", units = "cm", width = 35, height = 25, dpi = 320)


# LOLLYPOP DIAGRAM--------------------------------------------------------------

wtafimp19 %>%
  arrange(-Value) %>%
  mutate(order=factor(Area,Area)) %>%
  ggplot(aes(x = Area, y = Value/1000)) +
  geom_segment(aes(x = order, xend = Area, y = 0, yend = Value/1000, color = Region),
               size = 1.2, alpha = 0.75) +
  geom_point(aes(color = Region), size = 6, alpha = 0.75) +
  labs(caption = "Source: FAOSTAT") +
  theme_few() +
  scale_colour_jcolors(palette = "pal2") +
  theme(plot.margin = margin(20, 20, 20, 20),
        axis.ticks = element_blank(),
        plot.caption = element_text(size = 12, face = "italic", color = "white"),
        title =element_text(size=16, face = 'bold', color = "white"),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 1, vjust = 0.5, color = "white"), 
        axis.text.y = element_text(size = 12, color = "white"),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 16, margin = margin(0,10,0,0), color = "white"),
        legend.justification = c(0.65, 0.7), legend.position = c(0.65, 0.7),
        legend.title = element_blank(),
        legend.text = element_text(size = 14, color = "white"),
        legend.background = element_rect(fill = "grey10"),
        legend.key = element_rect(fill = "grey10"),
        plot.background = element_rect(fill = "grey10", color = NA),
        panel.background = element_rect(fill = "grey10"),
        panel.border = element_blank(),
        axis.line.x.bottom=element_line(color = "white"),
        axis.line.y.left=element_line(color = "white")
  ) +
  xlab("") +
  ylab("Wheat imports in 2019 (million tonnes)") 

# ggsave("Wheat import Africa.jpeg", units="cm", width = 30, height = 20, dpi = 320)


# CIRCLE PACKING PLOT-----------------------------------------------------------

packing <- circleProgressiveLayout(ex$Value, sizetype = 'area')
ex <- cbind(ex, packing)
ex.gg <- circleLayoutVertices(packing, npoints=50)
ex <- merge(ex.gg, ex, by = "id")


ggplot() + 
  labs(title = "Origins of the 231 million tonnes of wheat exported in 2019",
       subtitle = "Russia & Ukraine (highlighted) produced 23% of this volume",
       caption = "Source: FAOSTAT") +
  geom_polygon(data = ex, aes(x.x, y.x, group = id, fill = as.factor(region)), colour = NA, alpha = 1) +
  ggthemes::scale_fill_tableau() +
  geom_text(data = data, aes(x, y, size = Value, label = Area2), color = "white", fontface = "bold", show.legend = F) +
  scale_size_continuous(range = c(1,8)) +
  theme_void() + coord_equal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "grey10", color = NA),
        plot.title = element_text(color = "grey90", hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(color = "grey90", hjust = 0.5, size = 12, face = "bold", margin = margin(10,0,0,0)),
        plot.caption = element_text(color = "grey90", size= 10, hjust=1),
        plot.margin = margin(20, 20, 20, 20))  

# ggsave("Wheat exports.jpeg", units="cm", width = 20, height = 22, dpi = 320)


# HEATMAP-----------------------------------------------------------------------

ggplot(co, aes(Contents, Cereal)) + geom_tile(aes(fill = Value), colour = "white", size = 1) + 
  scale_fill_viridis(option = "F", limits = c(0,1), breaks = c(0, 0.5, 1), labels = c("low", "medium", "high")) +
  labs(fill = "Score") + theme_few() +
  xlab("") + ylab("") + 
  labs(title = "Cereal composition",
       caption = "Source: https://www.researchgate.net/figure/2-Nutritional-composition-comparison-per-100-g-maize-wheat-and-rice_tbl1_328020475") +
  theme(plot.background = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey10"),
        panel.border = element_blank(),
        axis.text.x = element_text(color = "grey90", size=14, angle = 90 , vjust=0.5, hjust=1),
        axis.text.y = element_text(color = "grey90", size=14),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x=element_blank(), axis.title.y = element_blank(),
        legend.background = element_rect(fill = "grey10", size = 0.1),
        legend.key.size = unit(1.2, "cm"),
        plot.title = element_text(color = "grey90", size = 24),
        plot.caption = element_text(color = "grey90", size = 8, face = "italic", hjust = 1.5),
        legend.title = element_text(color = "grey90", size = 20),
        legend.text = element_text(color = "grey90", size = 12),
        plot.margin = margin(20, 20, 20, 20))

# ggsave("Heatmap.jpeg", units="cm", width = 30, height = 13, dpi = 320)


# POLAR BARPLOTS----------------------------------------------------------------

ggplot(co, aes(x = Contents, y = Value, fill = factor(Contents))) +
  geom_col(width = 1, color = "white") + 
  coord_polar() + 
  labs(x = "", y = "", title = "") +
  scale_fill_tableau(palette = "Tableau 20") +
  theme_few() + facet_wrap(~ Cereal) +
  theme(
    plot.background = element_rect(fill = "grey10"),
    panel.background = element_rect(fill = "grey10"),
    panel.border = element_blank(),
    legend.position = "bottom",
    legend.background = element_rect(fill = "grey10", size = 0.1),
    legend.title = element_blank(),
    legend.text = element_text(color = "grey90", size = 10, face = "bold"),
    legend.spacing.x = unit(1.0, 'cm'),
    legend.key.height = unit(0.75, "cm"),
    legend.key.width = unit(0.75, "cm"),
    strip.background = element_rect(fill = "grey10", size = 0.1),
    strip.text = element_text(color = "grey90", size = 16, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_blank(),
    plot.margin = margin(20, 20, 20, 20))

# ggsave("Polar barplots.jpeg", units="cm", width = 30.5, height = 15, dpi = 320)


# FACET AREA PLOTS--------------------------------------------------------------

ggplot(wg, aes(Year, Value/1000)) + geom_area(aes(fill = Element), color = "white", size = 1) + theme_few() +
  xlab("") + ylab("Million tonnes") +
  scale_fill_manual(labels = c("Imports", "Production"), values = c("wheat2", "grey50")) +
  scale_x_continuous(breaks = round(seq(1961, 2019, by = 10))) +
  labs(title = "Wheat consumption in Africa & by sub-regions",
       caption = "Source: FAOSTAT") +
  facet_wrap(~ Area, scales = "free") +
  theme(plot.title = element_text(color = "grey90", hjust = 0.5, size = 18, face="bold",
                                  margin = margin(0,0,20,0)),
        strip.text = element_text(size = 16, colour = "black", face = "bold"),
        plot.caption = element_text(color = "grey90", size = 10, face = "italic", hjust=1),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey10"),
        panel.border = element_rect(color = "grey90"),
        axis.ticks = element_line(color = "grey90"),
        axis.title.y = element_text(size = 16,  face = "bold", color = "grey90"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 14,  face = "bold", color = "grey90"),
        axis.text.x = element_text(size = 10,  face = "bold", color = "grey90"),
        legend.position = c(0.025, 0.99), legend.justification = c(0.025, 0.99),
        legend.background = element_rect(fill = "grey10", size = 0.1),
        legend.title = element_blank(),
        legend.text = element_text(color = "grey90", size = 12),
        legend.key.size = unit(0.8, "cm"),
        legend.key = element_rect(fill = "grey10"),
        plot.margin = margin(20, 20, 20, 20))

# ggsave("Wheat gap africa regions.jpeg", units="cm", width = 40, height = 25, dpi = 320)


# HEATMAP-----------------------------------------------------------------------

ggplot(tm, aes(y = Value, axis1 = Exporters, axis2 = Importers)) + 
  theme_void() + ggtitle("Wheat trade (sum of 2018 & 2019)") +
  geom_alluvium(aes(fill = Exporters, weight = Value), decreasing = TRUE, alpha = 0.8, width = 0.5) + 
  geom_stratum(decreasing = TRUE, fill = "white", alpha = 0, size = 0.8, colour = "white", width = 0.5) +
  geom_text(stat = "stratum", fontface = "bold", aes(label = after_stat(stratum)), decreasing = TRUE, 
            size = 6, color = "white") + 
  scale_x_continuous(breaks = 1:2, labels = c('Exporters', 'Importers')) +
  scale_y_continuous(breaks = NULL, labels = NULL) +
  theme(plot.title = element_text(color = "white", hjust = 0.15, size = 28, face="bold"),
        axis.text = element_text(color = "white", size = 22, face="bold"),
        plot.background = element_rect(fill = "grey10"),
        legend.position = "none",
        plot.margin = margin(10, 10, 10, 10)) +
  scale_fill_manual(values = c("#4C1D4BFF","#4C1D4BFF","#4C1D4BFF","#4C1D4BFF","#4C1D4BFF","#F69C73FF","#FAEBDDFF","#4C1D4BFF"))

# ggsave("Wheat trade.jpeg", units="cm", width = 20, height = 40, dpi = 320)


# WORLD MAP---------------------------------------------------------------------

data(world)
world <- subset(world, continent != "Antarctica")

wi <- merge(world, wi, by = "name_long", all.x = TRUE)

wi <- 
  st_cast(wi, 'MULTIPOLYGON') %>%
  st_transform(crs = "+proj=moll")

ggplot(wi) +
  geom_sf(aes(fill = Value/1000), size = 0.2, color = "grey90", na.rm = TRUE) +
  scale_fill_viridis_c(name="Million tonnes", option = "F") +
  theme_void() +
  labs(title = "Annual wheat imports (means of 2018 & 2019)")+ 
  theme(
    legend.position = c(0.05,0.025), legend.justification = c(0.05,0.025),
    plot.background = element_rect(fill = "grey10", color = NA),
    plot.title = element_text(color = "white", size = 22, face = "bold", hjust=0.05, margin = margin(0,0,10,0)),
    legend.title = element_text(size = 18, color = "white", margin = margin(0,0,10,0)),
    legend.text = element_text(size = 14, color = "white", margin = margin(0,0,10,0)),
    legend.key.size = unit(1, "cm"),
    plot.margin = margin(20, 20, 20, 20))

# ggsave("Map wheat import.jpeg", units="cm", width = 40, height = 20, dpi = 320)



q <- ggplot(wp, aes(x = Date, y = WHEAT_US_HRW)) + 
  theme_few() +
  xlab("") + ylab("Price of US  hard red winter wheat (USD/mt)") +
  scale_x_date(breaks = date_breaks("4 years"), labels = date_format("%Y"),
               limits = c(min(wp$Date), max = max(wp$Date)), expand = c(0,0)) +
  geom_line(size = 1.2, color = "#CEFF1A") + 
  geom_point(size = 10, color = "#CEFF1A") + 
  labs(title = "Monthly wheat price in the last 40 years",
       caption = "Source: World Bank Commodity Price Data")+ 
  coord_cartesian(clip = 'off') + 
  transition_reveal(Date) +
  theme(plot.margin = margin(20, 20, 20, 20),
        axis.title.y = element_text(size = 24, face = "bold", color = "grey90", margin = margin(0,20,0,0)),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 20,  face = "bold", color = "grey90"),
        plot.title = element_text(color = "grey90", size = 30, face = "bold", hjust=0,
                                  margin = margin(0,0,20,0)),
        plot.caption = element_text(color = "grey90", size = 16, face = "italic", hjust=1,
                                    margin = margin(20,0,0,0)),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey10"),
        panel.border = element_blank(),
        axis.ticks = element_line(color = "grey90"),
        axis.line.x.bottom=element_line(color = "grey90"),
        axis.line.y.left=element_line(color = "grey90")) 

q

# animate(q, 200, fps = 10,  width = 1000, height = 700, 
#         renderer = gifski_renderer("Monthly wheat price animated.gif"))

