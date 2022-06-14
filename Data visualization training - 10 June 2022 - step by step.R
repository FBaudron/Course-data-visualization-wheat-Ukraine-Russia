#' ---
#' title: "Data visualization training - consequence of the Ukraine-Russia war on global wheat trade'"
#' author: "Frédéric Baudron"
#' date: "June 10th, 2022"
#' ---


# Email me at f.baudron@cgiar.org if you have questions after the training
# See also other visualizations I post regularly on Twitter (@FBaudron)


# SETTING UP THE DIRECTORY------------------------------------------------------

setwd('C:\\Users\\FBaudron\\Documents\\R\\0. Dataviz wheat\\')


# LOADING THE DATASETS----------------------------------------------------------

wp <- read.csv("wheat prices.csv")
wtafimp19 <- read.csv("wheat imp af.csv")
ex <- read.csv("export.csv")
co <- read.csv("cereal compo.csv")
wg <- read.csv("wheat gap.csv")
tm <- read.csv("trade matrix.csv")
wi <- read.csv("wheat import countries.csv")
cp <- read.csv("commodity prices.csv")


# LOADING REQUIRED PACKAGES-----------------------------------------------------

library(ggplot2)
library(ggthemes)
library(dplyr)
library(scales)
library(jcolors)
library(viridis)
library(packcircles)
library(ggalluvial)
library(spData)
library(sf)
library(transformr)
library(gganimate)
library(egg)
library(cowplot)
library(patchwork)


# LINE CHART--------------------------------------------------------------------

# format as date
wp$Date <- as.Date(wp$Date, format = "%m/%d/%Y")

ggplot(wp, aes(x = Date, y = WHEAT_US_HRW)) + 
  geom_line()

# change the size and color of the line
# also see https://r-graph-gallery.com/42-colors-names.html
# also see https://htmlcolorcodes.com/
ggplot(wp, aes(x = Date, y = WHEAT_US_HRW)) + 
  geom_line(size = 1.2, color = "red")

# change axis title
ggplot(wp, aes(x = Date, y = WHEAT_US_HRW)) + 
  geom_line(size = 1.2, color = "red") + 
  xlab("") + ylab("Price of US  hard red winter wheat (USD/mt)")

# 4 year break
ggplot(wp, aes(x = Date, y = WHEAT_US_HRW)) + 
  geom_line(size = 1.2, color = "red") + 
  xlab("") + ylab("Price of US  hard red winter wheat (USD/mt)") +
  scale_x_date(breaks = date_breaks("4 years"))

# Year labels
ggplot(wp, aes(x = Date, y = WHEAT_US_HRW)) + 
  geom_line(size = 1.2, color = "red") + 
  xlab("") + ylab("Price of US  hard red winter wheat (USD/mt)") +
  scale_x_date(breaks = date_breaks("4 years"), labels = date_format("%Y"))

# bounding the x-axis by the min and max values
ggplot(wp, aes(x = Date, y = WHEAT_US_HRW)) + 
  geom_line(size = 1.2, color = "red") + 
  xlab("") + ylab("Price of US  hard red winter wheat (USD/mt)") +
  scale_x_date(breaks = date_breaks("4 years"), labels = date_format("%Y"),
               limits = c(min(wp$Date), max = max(wp$Date)), expand = c(0,0))

# adding title and caption
ggplot(wp, aes(x = Date, y = WHEAT_US_HRW)) + 
  geom_line(size = 1.2, color = "red") + 
  xlab("") + ylab("Price of US  hard red winter wheat (USD/mt)") +
  scale_x_date(breaks = date_breaks("4 years"), labels = date_format("%Y"),
               limits = c(min(wp$Date), max = max(wp$Date)), expand = c(0,0)) +
  labs(title = "Monthly wheat price in the last 40 years",
       caption = "Source: World Bank Commodity Price Data")

# using them_few (try theme_bw, theme_classic, theme_void)
# also see https://ggplot2.tidyverse.org/reference/ggtheme.html
# also see https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/
ggplot(wp, aes(x = Date, y = WHEAT_US_HRW)) + 
  geom_line(size = 1.2, color = "red") + 
  xlab("") + ylab("Price of US  hard red winter wheat (USD/mt)") +
  scale_x_date(breaks = date_breaks("4 years"), labels = date_format("%Y"),
               limits = c(min(wp$Date), max = max(wp$Date)), expand = c(0,0)) +
  labs(title = "Monthly wheat price in the last 40 years",
       caption = "Source: World Bank Commodity Price Data") + 
  theme_few()

# change aspect of axis titles, axis text, plot title and plot caption
ggplot(wp, aes(x = Date, y = WHEAT_US_HRW)) + 
  geom_line(size = 1.2, color = "red") + 
  xlab("") + ylab("Price of US  hard red winter wheat (USD/mt)") +
  scale_x_date(breaks = date_breaks("4 years"), labels = date_format("%Y"),
               limits = c(min(wp$Date), max = max(wp$Date)), expand = c(0,0)) +
  labs(title = "Monthly wheat price in the last 40 years",
       caption = "Source: World Bank Commodity Price Data") + 
  theme_few() +
  theme(axis.title.y = element_text(size = 24, face = "bold"),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 20,  face = "bold"),
        plot.title = element_text(size = 30, face = "bold"),
        plot.caption = element_text(size = 16, face = "italic")) 

# add space below title, above caption, and around plot
ggplot(wp, aes(x = Date, y = WHEAT_US_HRW)) + 
  geom_line(size = 1.2, color = "red") + 
  xlab("") + ylab("Price of US  hard red winter wheat (USD/mt)") +
  scale_x_date(breaks = date_breaks("4 years"), labels = date_format("%Y"),
               limits = c(min(wp$Date), max = max(wp$Date)), expand = c(0,0)) +
  labs(title = "Monthly wheat price in the last 40 years",
       caption = "Source: World Bank Commodity Price Data") + 
  theme_few() +
  theme(axis.title.y = element_text(size = 24, face = "bold"),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 20,  face = "bold"),
        plot.title = element_text(size = 30, face = "bold",
                                  margin = margin(0,0,20,0)),
        plot.caption = element_text(size = 16, face = "italic",
                                    margin = margin(20,0,0,0)),
        plot.margin = margin(20, 20, 20, 20))

# dark background, light text, lines and ticks
ggplot(wp, aes(x = Date, y = WHEAT_US_HRW)) + 
  geom_line(size = 1.2, color = "red") + 
  xlab("") + ylab("Price of US  hard red winter wheat (USD/mt)") +
  scale_x_date(breaks = date_breaks("4 years"), labels = date_format("%Y"),
               limits = c(min(wp$Date), max = max(wp$Date)), expand = c(0,0)) +
  labs(title = "Monthly wheat price in the last 40 years",
       caption = "Source: World Bank Commodity Price Data") + 
  theme_few() +
  theme(axis.title.y = element_text(size = 24, face = "bold", color = "grey90", margin = margin(0,20,0,0)),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 20,  face = "bold", color = "grey90"),
        plot.title = element_text(color = "grey90", size = 30, face = "bold",
                                  margin = margin(0,0,20,0)),
        plot.caption = element_text(color = "grey90", size = 16, face = "italic",
                                    margin = margin(20,0,0,0)),
        plot.background = element_rect(fill = "grey10", color = NA),
        panel.background = element_rect(fill = "grey10"),
        panel.border = element_blank(),
        axis.ticks = element_line(color = "grey90"),
        axis.line = element_line(color = "grey90"),
        plot.margin = margin(20, 20, 20, 20))

# name the plot and use #CEFF1A as color
fig1 <- ggplot(wp, aes(x = Date, y = WHEAT_US_HRW)) + 
  geom_line(size = 1.2, color = "#CEFF1A") + 
  xlab("") + ylab("Price of US  hard red winter wheat (USD/mt)") +
  scale_x_date(breaks = date_breaks("4 years"), labels = date_format("%Y"),
               limits = c(min(wp$Date), max = max(wp$Date)), expand = c(0,0)) +
  labs(title = "Monthly wheat price in the last 40 years",
       caption = "Source: World Bank Commodity Price Data") + 
  theme_few() +
  theme(axis.title.y = element_text(size = 24, face = "bold", color = "grey90", margin = margin(0,20,0,0)),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 20,  face = "bold", color = "grey90"),
        plot.title = element_text(color = "grey90", size = 30, face = "bold",
                                  margin = margin(0,0,20,0)),
        plot.caption = element_text(color = "grey90", size = 16, face = "italic",
                                    margin = margin(20,0,0,0)),
        plot.background = element_rect(fill = "grey10", color = NA),
        panel.background = element_rect(fill = "grey10"),
        panel.border = element_blank(),
        axis.ticks = element_line(color = "grey90"),
        axis.line = element_line(color = "grey90"),
        plot.margin = margin(20, 20, 20, 20)) 

fig1

# save using ggsave
# ggsave("Fig 1 - Monthly wheat prices.jpeg", units = "cm", width = 35, height = 25, dpi = 320)


# LOLLYPOP DIAGRAM--------------------------------------------------------------

ggplot(wtafimp19, aes(x = Area, y = Value/1000)) +
  geom_point()

# add segments
ggplot(wtafimp19, aes(x = Area, y = Value/1000))  +
  geom_point() +
  geom_segment(aes(x = Area, xend = Area, y = 0, yend = Value/1000))

# color per region (add in the aesthetics)
ggplot(wtafimp19, aes(x = Area, y = Value/1000))  +
  geom_point(aes(color = Region)) +
  geom_segment(aes(x = Area, xend = Area, y = 0, yend = Value/1000, color = Region))

# order by decreasing value
# mutate() adds new variables and preserves existing ones
# remember to change origin of segment to the new variable 'order'!
wtafimp19 %>%
  arrange(-Value) %>%
  mutate(order = factor(Area, Area)) %>%
  ggplot(aes(x = order, y = Value/1000))  +
  geom_point(aes(color = Region)) +
  geom_segment(aes(x = order, xend = Area, y = 0, yend = Value/1000, color = Region))

# add 'transparency' (alpha) for the lollypop aspect
wtafimp19 %>%
  arrange(-Value) %>%
  mutate(order = factor(Area, Area)) %>%
  ggplot(aes(x = order, y = Value/1000))  +
  geom_point(aes(color = Region), size = 6, alpha = 0.75) +
  geom_segment(aes(x = order, xend = Area, y = 0, yend = Value/1000, color = Region),
               size = 1.2, alpha = 0.75)

# use the jcolors palette "pal 2"
# try scale_color_hue(), scale_color_viridis_d(), and scale_color_viridis_d(option = "F")
# also see https://cran.r-project.org/web/packages/jcolors/vignettes/using_the_jcolors_package.html
# also see https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
wtafimp19 %>%
  arrange(-Value) %>%
  mutate(order = factor(Area, Area)) %>%
  ggplot(aes(x = order, y = Value/1000))  +
  geom_point(aes(color = Region), size = 6, alpha = 0.75) +
  geom_segment(aes(x = order, xend = Area, y = 0, yend = Value/1000, color = Region),
               size = 1.2, alpha = 0.75) +
  scale_color_jcolors(palette = "pal2")

# move legend in plot area
wtafimp19 %>%
  arrange(-Value) %>%
  mutate(order = factor(Area, Area)) %>%
  ggplot(aes(x = order, y = Value/1000))  +
  geom_point(aes(color = Region), size = 6, alpha = 0.75) +
  geom_segment(aes(x = order, xend = Area, y = 0, yend = Value/1000, color = Region),
               size = 1.2, alpha = 0.75) +
  scale_color_jcolors(palette = "pal2") +
  theme(legend.position = c(0.65, 0.7))

# move the legend to the plotting area
# legend.position sets the position of the center of the legend area
# legend.justification sets the position of the bottom left corner of the legend
wtafimp19 %>%
  arrange(-Value) %>%
  mutate(order = factor(Area, Area)) %>%
  ggplot(aes(x = order, y = Value/1000))  +
  geom_point(aes(color = Region), size = 6, alpha = 0.75) +
  geom_segment(aes(x = order, xend = Area, y = 0, yend = Value/1000, color = Region),
               size = 1.2, alpha = 0.75) +
  scale_color_jcolors(palette = "pal2") +
  theme(legend.justification = c(0.65, 0.7), legend.position = c(0.65, 0.7))

# name plot, add caption, use theme_few, and dark background
fig2 <- wtafimp19 %>%
  arrange(-Value) %>%
  mutate(order = factor(Area, Area)) %>%
  ggplot(aes(x = order, y = Value/1000)) +
  geom_point(aes(color = Region), size = 6, alpha = 0.75) +
  geom_segment(aes(x = order, xend = Area, y = 0, yend = Value/1000, color = Region),
               size = 1.2, alpha = 0.75) +
  scale_color_jcolors(palette = "pal2") +
  labs(caption = "Source: FAOSTAT") +
  theme_few() +
  theme(legend.position = c(0.65, 0.7), legend.justification = c(0.65, 0.7), 
        plot.margin = margin(20, 20, 20, 20),
        axis.ticks = element_blank(),
        plot.caption = element_text(size = 12, face = "italic", color = "white"),
        title =element_text(size=16, face = 'bold', color = "white"),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 1, vjust = 0.5, color = "white"), 
        axis.text.y = element_text(size = 12, color = "white"),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 16, margin = margin(0,10,0,0), color = "white"),
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

fig2

# save using ggsave
# ggsave("Fig 2 - Wheat import Africa.jpeg", units="cm", width = 30, height = 20, dpi = 320)


# CIRCLE PACKING PLOT-----------------------------------------------------------

#create a dataset of coordinates and radii (radii based on values provided)
packing <- circleProgressiveLayout(ex$Value, sizetype = 'area')
# bind with original dataset
ex <- cbind(ex, packing)
# get the layout of the circles
ex.gg <- circleLayoutVertices(packing, npoints=50)

ggplot() + 
  geom_polygon(data = ex.gg, aes(x, y, group = id))

# change colour of circles and add labels
ggplot() + 
  geom_polygon(data = ex.gg, aes(x, y, group = id), fill = "red") +
  geom_text(data = ex, aes(x, y, size = Value, label = Area), color = "white", fontface = "bold")

# remove legend for text size
ggplot() + 
  geom_polygon(data = ex.gg, aes(x, y, group = id), fill = "red") +
  geom_text(data = ex, aes(x, y, size = Value, label = Area), color = "white", fontface = "bold", show.legend = F)

# show only main labels (replace Area by Area2, see dataset)
ggplot() + 
  geom_polygon(data = ex.gg, aes(x, y, group = id), fill = "red") +
  geom_text(data = ex, aes(x, y, size = Value, label = Area2), color = "white", fontface = "bold", show.legend = F)

# play a bit with the relative size of the labels
ggplot() + 
  geom_polygon(data = ex.gg, aes(x, y, group = id), fill = "red") +
  geom_text(data = ex, aes(x, y, size = Value, label = Area2), color = "white", fontface = "bold", show.legend = F)+
  scale_size_continuous(range = c(1,8)) 

# use theme_void and coor_equal
ggplot() + 
  geom_polygon(data = ex.gg, aes(x, y, group = id), fill = "red") +
  geom_text(data = ex, aes(x, y, size = Value, label = Area2), color = "white", fontface = "bold", show.legend = F)+
  scale_size_continuous(range = c(1,8)) +
  theme_void() + coord_equal()

# colour differently Ukrain and Russia ('region' = true in the dataset)
reg <- ex[,c(3,4)]
ex.gg <- merge(ex.gg, reg, by = "id")
ggplot() + 
  geom_polygon(data = ex.gg, aes(x, y, group = id, fill = as.factor(region))) +
  geom_text(data = ex, aes(x, y, size = Value, label = Area2), color = "white", fontface = "bold", show.legend = F)+
  scale_size_continuous(range = c(1,8)) +
  theme_void() + coord_equal() +
  ggthemes::scale_fill_tableau()
  
# add title and caption, remove legend, and dark background
fig3 <- ggplot() + 
  geom_polygon(data = ex.gg, aes(x, y, group = id, fill = as.factor(region))) +
  geom_text(data = ex, aes(x, y, size = Value, label = Area2), color = "white", fontface = "bold", show.legend = F) +
  scale_size_continuous(range = c(1,8)) +
  theme_void() + coord_equal() +
  ggthemes::scale_fill_tableau() +
  labs(title = "Origins of the 231 million tonnes of wheat exported in 2019",
       subtitle = "Russia & Ukraine (highlighted) produced 23% of this volume",
       caption = "Source: FAOSTAT") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "grey10", color = NA),
        plot.title = element_text(color = "grey90", hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(color = "grey90", hjust = 0.5, size = 12, face = "bold", margin = margin(10,0,0,0)),
        plot.caption = element_text(color = "grey90", size= 10, hjust=1),
        plot.margin = margin(20, 20, 20, 20))  

fig3

# save using ggsave
# ggsave("Fig 3 - Wheat exports.jpeg", units="cm", width = 20, height = 22, dpi = 320)


# HEATMAP-----------------------------------------------------------------------

ggplot(co, aes(Contents, Cereal)) + 
  geom_tile(aes(fill = Value))

# add white lines of size 1
ggplot(co, aes(Contents, Cereal)) + 
  geom_tile(aes(fill = Value), colour = "white", size = 1)

# use the viridris palette with option F (try other palettes as well!)
ggplot(co, aes(Contents, Cereal)) + 
  geom_tile(aes(fill = Value), colour = "white", size = 1) + 
  scale_fill_viridis(option = "F")

# change the scale to low, meadium, high
ggplot(co, aes(Contents, Cereal)) + 
  geom_tile(aes(fill = Value), colour = "white", size = 1) + 
  scale_fill_viridis(option = "F",
                     limits = c(0,1), breaks = c(0, 0.5, 1), labels = c("low", "medium", "high"))

# Change the legend title
ggplot(co, aes(Contents, Cereal)) + 
  geom_tile(aes(fill = Value), colour = "white", size = 1) + 
  scale_fill_viridis(option = "F",
                     limits = c(0,1), breaks = c(0, 0.5, 1), labels = c("low", "medium", "high")) +
  labs(fill = "Score")

# add plot title and caption
ggplot(co, aes(Contents, Cereal)) + 
  geom_tile(aes(fill = Value), colour = "white", size = 1) + 
  scale_fill_viridis(option = "F",
                     limits = c(0,1), breaks = c(0, 0.5, 1), labels = c("low", "medium", "high")) +
  labs(fill = "Score") + 
  xlab("") + ylab("") + 
  labs(title = "Cereal composition",
       caption = "Source: https://www.researchgate.net/figure/2-Nutritional-composition-comparison-per-100-g-maize-wheat-and-rice_tbl1_328020475")

# use theme_few (try other themes as well!)
ggplot(co, aes(Contents, Cereal)) + 
  geom_tile(aes(fill = Value), colour = "white", size = 1) + 
  scale_fill_viridis(option = "F",
                     limits = c(0,1), breaks = c(0, 0.5, 1), labels = c("low", "medium", "high")) +
  labs(fill = "Score") + 
  xlab("") + ylab("") + 
  labs(title = "Cereal composition",
       caption = "Source: https://www.researchgate.net/figure/2-Nutritional-composition-comparison-per-100-g-maize-wheat-and-rice_tbl1_328020475") +
  theme_few()

# name plot, remove panel border, and use dark background
# also note changes of horizontal justification for plot title and caption (hjust)
fig4 <- ggplot(co, aes(Contents, Cereal)) + 
  geom_tile(aes(fill = Value), colour = "white", size = 1) + 
  scale_fill_viridis(option = "F",
                     limits = c(0,1), breaks = c(0, 0.5, 1), labels = c("low", "medium", "high")) +
  labs(fill = "Score") + 
  xlab("") + ylab("") + 
  labs(title = "Cereal composition",
       caption = "Source: https://www.researchgate.net/figure/2-Nutritional-composition-comparison-per-100-g-maize-wheat-and-rice_tbl1_328020475") +
  theme_few() +
  theme(plot.background = element_rect(fill = "grey10", color = NA),
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

fig4

# save using ggsave
# ggsave("Fig 4 - Heatmap.jpeg", units="cm", width = 30, height = 13, dpi = 320)


# POLAR BARPLOTS----------------------------------------------------------------

# first draw a simple bar plot
ggplot(co, aes(x = Contents, y = Value, fill = factor(Contents))) +
  geom_col()

# wrap the plot around with 
ggplot(co, aes(x = Contents, y = Value, fill = factor(Contents))) +
  geom_col() +
  coord_polar()

# add white lines of size 1, use palette tableau with 20 values,
# remove all labels and use theme_few
ggplot(co, aes(x = Contents, y = Value, fill = factor(Contents))) +
  geom_col(width = 1, color = "white") + 
  coord_polar() + 
  scale_fill_tableau(palette = "Tableau 20") +
  labs(x = "", y = "", title = "") +
  theme_few()

# use facet_wrap to separate the plot per ceral and create a panel
ggplot(co, aes(x = Contents, y = Value, fill = factor(Contents))) +
  geom_col(width = 1, color = "white") + 
  coord_polar() + 
  scale_fill_tableau(palette = "Tableau 20") +
  labs(x = "", y = "", title = "") +
  theme_few() + facet_wrap(~ Cereal)

# name plot, move legend to the bottom, remove panel border, and use dark background
# also note changes applied to the background and text of the strip
fig5 <- ggplot(co, aes(x = Contents, y = Value, fill = factor(Contents))) +
  geom_col(width = 1, color = "white") + 
  coord_polar() + 
  scale_fill_tableau(palette = "Tableau 20") +
  labs(x = "", y = "", title = "") +
  theme_few() + facet_wrap(~ Cereal) +
  theme(
    plot.background = element_rect(fill = "grey10", color = NA),
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

fig5

# save using ggsave
# ggsave("Fig 5 - Polar barplots.jpeg", units="cm", width = 30.5, height = 15, dpi = 320)


# FACET AREA PLOTS--------------------------------------------------------------

# simple facet area plot
ggplot(wg, aes(Year, Value/1000, fill = Element)) + 
  geom_area() +
  facet_wrap(~ Area)

# free scale of the different graphs in the panel
ggplot(wg, aes(Year, Value/1000, fill = Element)) + 
  geom_area() +
  facet_wrap(~ Area, scales = "free")

# change the x-axis scale to have breaks of 10 years
ggplot(wg, aes(Year, Value/1000, fill = Element)) + 
  geom_area(color = "white", size = 1) +
  facet_wrap(~ Area, scales = "free") + 
  scale_x_continuous(breaks = seq(1961, 2019, by = 10))

# use a manual color scale to fill areas
ggplot(wg, aes(Year, Value/1000, fill = Element)) + 
  geom_area(color = "white", size = 1) +
  facet_wrap(~ Area, scales = "free") + 
  scale_x_continuous(breaks = seq(1961, 2019, by = 10)) +
  scale_fill_manual(labels = c("Imports", "Production"), values = c("wheat2", "grey50"))

# change axis titles, use theme_few and add plot title and caption
ggplot(wg, aes(Year, Value/1000, fill = Element)) + 
  geom_area(color = "white", size = 1) +
  facet_wrap(~ Area, scales = "free") + 
  scale_x_continuous(breaks = seq(1961, 2019, by = 10)) +
  scale_fill_manual(labels = c("Imports", "Production"), values = c("wheat2", "grey50")) +
  xlab("") + ylab("Million tonnes") +
  theme_few() +
  labs(title = "Wheat consumption in Africa & by sub-regions",
       caption = "Source: FAOSTAT")

# move the legend to the top left corner of the panel, and remove legend title
ggplot(wg, aes(Year, Value/1000, fill = Element)) + 
  geom_area(color = "white", size = 1) +
  facet_wrap(~ Area, scales = "free") + 
  scale_x_continuous(breaks = seq(1961, 2019, by = 10)) +
  scale_fill_manual(labels = c("Imports", "Production"), values = c("wheat2", "grey50")) +
  xlab("") + ylab("Million tonnes") +
  theme_few() +
  labs(title = "Wheat consumption in Africa & by sub-regions",
       caption = "Source: FAOSTAT")  +
  theme(legend.position = c(0.025, 0.99), legend.justification = c(0.025, 0.99),
        legend.title = element_blank())
 
# increase legend key
ggplot(wg, aes(Year, Value/1000, fill = Element)) + 
  geom_area(color = "white", size = 1) +
  facet_wrap(~ Area, scales = "free") + 
  scale_x_continuous(breaks = seq(1961, 2019, by = 10)) +
  scale_fill_manual(labels = c("Imports", "Production"), values = c("wheat2", "grey50")) +
  xlab("") + ylab("Million tonnes") +
  theme_few() +
  labs(title = "Wheat consumption in Africa & by sub-regions",
       caption = "Source: FAOSTAT")  +
  theme(legend.position = c(0.025, 0.99), legend.justification = c(0.025, 0.99),
        legend.title = element_blank(),
        legend.key.size = unit(0.8, "cm"))

# name the plot, increase space below the plot title, and use dark background
fig6 <- ggplot(wg, aes(Year, Value/1000, fill = Element)) + 
  geom_area(color = "white", size = 1) +
  facet_wrap(~ Area, scales = "free") + 
  scale_x_continuous(breaks = seq(1961, 2019, by = 10)) +
  scale_fill_manual(labels = c("Imports", "Production"), values = c("wheat2", "grey50")) +
  xlab("") + ylab("Million tonnes") +
  theme_few() +
  labs(title = "Wheat consumption in Africa & by sub-regions",
       caption = "Source: FAOSTAT")  +
  theme(legend.position = c(0.025, 0.99), legend.justification = c(0.025, 0.99),
        legend.title = element_blank(),
        legend.key.size = unit(0.8, "cm"),
        plot.title = element_text(color = "grey90", hjust = 0.5, size = 18, face="bold",
                                  margin = margin(0,0,20,0)),
        strip.text = element_text(size = 16, colour = "black", face = "bold"),
        plot.caption = element_text(color = "grey90", size = 10, face = "italic", hjust=1),
        plot.background = element_rect(fill = "grey10", color = NA),
        panel.background = element_rect(fill = "grey10"),
        panel.border = element_rect(color = "grey90"),
        axis.ticks = element_line(color = "grey90"),
        axis.title.y = element_text(size = 16,  face = "bold", color = "grey90"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 14,  face = "bold", color = "grey90"),
        axis.text.x = element_text(size = 10,  face = "bold", color = "grey90"),
        legend.background = element_rect(fill = "grey10", size = 0.1),
        legend.text = element_text(color = "grey90", size = 12),
        legend.key = element_rect(fill = "grey10"),
        plot.margin = margin(20, 20, 20, 20))

fig6

# save using ggsave
# ggsave("Fig 6 - Wheat gap africa regions.jpeg", units="cm", width = 40, height = 25, dpi = 320)


# ALLUVIAL DIAGRAM--------------------------------------------------------------

# draw strata
ggplot(tm, aes(y = Value, axis1 = Exporters, axis2 = Importers)) + 
  geom_stratum()

# add steams
ggplot(tm, aes(y = Value, axis1 = Exporters, axis2 = Importers)) + 
  geom_stratum() +
  geom_alluvium()

# color streams by exporter (axis1 values)
ggplot(tm, aes(y = Value, axis1 = Exporters, axis2 = Importers)) + 
  geom_stratum() +
  geom_alluvium(aes(fill = Exporters))

# add text to strata
ggplot(tm, aes(y = Value, axis1 = Exporters, axis2 = Importers)) + 
  geom_stratum() +
  geom_alluvium(aes(fill = Exporters)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) 

# add 2 breaks to the x-axis and relabel thease breaks, remove breaks and labels to the y-axis
ggplot(tm, aes(y = Value, axis1 = Exporters, axis2 = Importers)) + 
  geom_stratum() +
  geom_alluvium(aes(fill = Exporters)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_continuous(breaks = 1:2, labels = c('Exporters', 'Importers')) +
  scale_y_continuous(breaks = NULL, labels = NULL)

# add plot title and use theme_void
ggplot(tm, aes(y = Value, axis1 = Exporters, axis2 = Importers)) + 
  geom_stratum() +
  geom_alluvium(aes(fill = Exporters)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_continuous(breaks = 1:2, labels = c('Exporters', 'Importers')) +
  scale_y_continuous(breaks = NULL, labels = NULL) +
  ggtitle("Wheat trade (sum of 2018 & 2019)") +
  theme_void()

# reorder strata, streams and text by decreasing order
ggplot(tm, aes(y = Value, axis1 = Exporters, axis2 = Importers)) + 
  geom_stratum(decreasing = TRUE) +
  geom_alluvium(aes(fill = Exporters), decreasing = TRUE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), decreasing = TRUE) +
  scale_x_continuous(breaks = 1:2, labels = c('Exporters', 'Importers')) +
  scale_y_continuous(breaks = NULL, labels = NULL) +
  ggtitle("Wheat trade (sum of 2018 & 2019)") +
  theme_void()

# dark background, increased text size (to 6), bold text, and size od strate frame 0.8
# no fill for strat (total transparency)
# manual color palette with 1 color for Russia, 1 for Ukraine, and one for all other Exporters
# (alphabetic order still prevails here)
ggplot(tm, aes(y = Value, axis1 = Exporters, axis2 = Importers)) + 
  geom_stratum(decreasing = TRUE, fill = NA, colour = "white", size = 0.8) +
  geom_alluvium(aes(fill = Exporters), decreasing = TRUE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), decreasing = TRUE,
            color = "white", size = 6, fontface = "bold") +
  scale_x_continuous(breaks = 1:2, labels = c('Exporters', 'Importers')) +
  scale_y_continuous(breaks = NULL, labels = NULL) +
  ggtitle("Wheat trade (sum of 2018 & 2019)") +
  theme_void() + 
  theme(plot.title = element_text(color = "white", hjust = 0.15, size = 28, face="bold"),
        axis.text = element_text(color = "white", size = 22, face="bold"),
        plot.background = element_rect(fill = "grey10", color = NA),
        legend.position = "none",
        plot.margin = margin(10, 10, 10, 10)) +
  scale_fill_manual(values = c("#4C1D4BFF","#4C1D4BFF","#4C1D4BFF","#4C1D4BFF","#4C1D4BFF","#F69C73FF","#FAEBDDFF","#4C1D4BFF"))

# name the plot and make the strata half of the plot (0.5) and the stream half of the plot (0.5)
fig7 <- ggplot(tm, aes(y = Value, axis1 = Exporters, axis2 = Importers)) + 
  geom_stratum(decreasing = TRUE, fill = NA, colour = "white", size = 0.8, width = 0.5) +
  geom_alluvium(aes(fill = Exporters), decreasing = TRUE, width = 0.5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), decreasing = TRUE,
            color = "white", size = 6, fontface = "bold") +
  scale_x_continuous(breaks = 1:2, labels = c('Exporters', 'Importers')) +
  scale_y_continuous(breaks = NULL, labels = NULL) +
  ggtitle("Wheat trade (sum of 2018 & 2019)") +
  theme_void() + 
  theme(plot.title = element_text(color = "white", hjust = 0.15, size = 28, face="bold"),
        axis.text = element_text(color = "white", size = 22, face="bold"),
        plot.background = element_rect(fill = "grey10", color = NA),
        legend.position = "none",
        plot.margin = margin(10, 10, 10, 10)) +
  scale_fill_manual(values = c("#4C1D4BFF","#4C1D4BFF","#4C1D4BFF","#4C1D4BFF","#4C1D4BFF","#F69C73FF","#FAEBDDFF","#4C1D4BFF"))

fig7

# save using ggsave
# ggsave("Fig 7 - Wheat trade.jpeg", units="cm", width = 25, height = 40, dpi = 320)


# WORLD MAP---------------------------------------------------------------------

# download world country polygons from spData
data(world)

# remove Antartica
world <- subset(world, continent != "Antarctica")

# merge country polygons with your own dataset
wi <- merge(world, wi, by = "name_long", all.x = TRUE)

# choose a projection (I like Mollweide)
# also see https://proj.org/operations/projections/index.html for various projections
wi <- 
  st_cast(wi, 'MULTIPOLYGON') %>%
  st_transform(crs = "+proj=moll")

# simple map
ggplot(wi) +
  geom_sf(aes(fill = Value/1000))

# use viridis palette with option F and change legend title
ggplot(wi) +
  geom_sf(aes(fill = Value/1000)) +
  scale_fill_viridis_c(name="Million tonnes", option = "F")

# change color and size of the frame of polygons
ggplot(wi) +
  geom_sf(aes(fill = Value/1000), size = 0.2, color = "grey90") +
  scale_fill_viridis_c(name="Million tonnes", option = "F")

# add plot title, use theme_void, and move legend to bottom left
ggplot(wi) +
  geom_sf(aes(fill = Value/1000), size = 0.2, color = "grey90") +
  scale_fill_viridis_c(name="Million tonnes", option = "F") +
  labs(title = "Annual wheat imports (means of 2018 & 2019)")+ 
  theme_void() +
  theme(
    legend.position = c(0.05,0.025), legend.justification = c(0.05,0.025))
  
# name the plot, dark background and increased key size
fig8 <- ggplot(wi) +
  geom_sf(aes(fill = Value/1000), size = 0.2, color = "grey90") +
  scale_fill_viridis_c(name="Million tonnes", option = "F") +
  labs(title = "Annual wheat imports (means of 2018 & 2019)") + 
  theme_void() +
  theme(
    legend.position = c(0.05,0.025), legend.justification = c(0.05,0.025),
    plot.background = element_rect(fill = "grey10", color = NA),
    plot.title = element_text(color = "white", size = 22, face = "bold", hjust=0.05, margin = margin(0,0,10,0)),
    legend.title = element_text(size = 18, color = "white", margin = margin(0,0,10,0)),
    legend.text = element_text(size = 14, color = "white", margin = margin(0,0,10,0)),
    legend.key.size = unit(1, "cm"),
    plot.margin = margin(20, 20, 20, 20))

fig8

# save using ggsave
# ggsave("Fig 8 - Map wheat import.jpeg", units="cm", width = 40, height = 20, dpi = 320)


# SIMPLE PANEL WITH EGG---------------------------------------------------------

pan1_draft <- ggarrange(fig1, fig2, 
          ncol=1, nrow=2, widths=c(1), heights=c(1,1))

# use ggdraw to capture a snapshot of the plot (or else, the last single plot will be saved, not the panel)
# also ensure that the background of the snapshot is the same as the panel (dark backgrouund in this case)
ggdraw(pan1_draft) + theme(plot.background = element_rect(fill = "grey10"))

# add titles (A and B) to the plot before capturing a snapshot
pan1_draft <- ggarrange(fig1 + ggtitle("A"),
          fig2 + ggtitle("B"), 
          ncol=1, nrow=2, widths=c(1), heights=c(1,1))

ggdraw(pan1_draft) + theme(plot.background = element_rect(fill = "grey10"))

# adjust the theme of the plots (for them to be similar) before taking a snapshot and saving
pan1 <- ggarrange(fig1 + ggtitle("A") + 
                  theme(axis.title.y = element_text(size = 14, face = "bold", color = "grey90"),
                        axis.text.y = element_text(size = 14, face = "bold", color = "grey90"),
                        plot.title = element_text(size = 22, face = "bold", color = "grey90"),
                        plot.caption = element_text(size = 12, face = "italic", color = "grey90")),
                  fig2 + ggtitle("B") +
                    theme(plot.title = element_text(size = 22, face = "bold", color = "grey90")), 
                  ncol=1, nrow=2, widths=c(1), heights=c(1,1))

ggdraw(pan1) + 
  theme(plot.background = element_rect(fill = "grey10"),
        plot.margin = margin(10, 10, 10, 10))

# save the panel (the snapshot captured by ggdraw) using ggsave
# ggsave("Panel 1.jpeg", units="cm", width = 30, height = 40, dpi = 320)


# MORE COMPLEX PANEL WITH PATCHWORK---------------------------------------------

# create a layout where plots will be placed by alphabetic order (A, B, etc) in the plot order provided
layout <- "
AAAADDDDDDFFFF
AAAADDDDDDFFFF
AAAADDDDDDFFFF
AAAADDDDDDFFFF
BBBBDDDDDDFFFF
BBBBEEEEEEFFFF
CCCCEEEEEEFFFF
CCCCGGGGGGGGGG
CCCCGGGGGGGGGG
CCCCGGGGGGGGGG
CCCCGGGGGGGGGG
"
pan2_draft <- fig1 + fig2 + fig3 + fig6 + fig8 + fig7 + fig5 + 
  plot_layout(design = layout)

# use ggdraw to capture a snapshot of the plot
ggdraw(pan2_draft) + 
  theme(plot.background = element_rect(fill = "grey10"))

# add plot titles to plots (A, B, etc), and adjust the theme of the plots
pan2 <- fig1 + ggtitle("A") + theme(plot.title = element_text(size = 30, face = "bold", color = "grey90"),
                                    plot.caption = element_blank(),
                                    axis.title.y = element_text(size = 14, face = "bold", color = "grey90"),
                                    axis.text.y = element_text(size = 14, face = "bold", color = "grey90")) +
  fig2 + ggtitle("B") + theme(plot.title = element_text(size = 30, face = "bold", color = "grey90"),
                              plot.caption = element_blank()) +
  fig3 + ggtitle("C") + theme(plot.title = element_text(size = 30, face = "bold", color = "grey90", hjust = 0),
                              plot.subtitle = element_blank(),
                              plot.caption = element_blank()) +
  fig6 + ggtitle("D") + theme(plot.title = element_text(size = 30, face = "bold", color = "grey90", hjust = 0),
                              plot.caption = element_blank()) +
  fig8 + ggtitle("E") + theme(plot.title = element_text(size = 30, face = "bold", color = "grey90")) +
  fig7 + ggtitle("F") + theme(plot.title = element_text(size = 30, face = "bold", color = "grey90")) +
  fig5 + ggtitle("G") + theme(plot.title = element_text(size = 30, face = "bold", color = "grey90", hjust = 0)) +
  plot_layout(design = layout) + 
  theme(plot.background = element_rect(fill = "grey10"),
        plot.margin = margin(20, 20, 20, 20))

# use ggdraw to capture a snapshot of the plot
ggdraw(pan2) + 
  theme(plot.background = element_rect(fill = "grey10"))

# save the panel (the snapshot captured by ggdraw) using ggsave
# ggsave("Panel 2.jpeg", units="cm", width = 80, height = 60, dpi = 320)


# SIMPLE ANIMATED GRAPH---------------------------------------------------------
 
# let's animate fig1, by simply specifying what the transition should be (Date)
fig1 + transition_reveal(Date)

# add a moving dot (geom_point). Carreful: all animated object needs to be mentioned before transition_reveal!
fig1 + geom_point(size = 10, color = "#CEFF1A") +
  transition_reveal(Date)
  
# name the animated plot
ani1 <- fig1 + geom_point(size = 10, color = "#CEFF1A") +
  transition_reveal(Date)

# save as gif
# animate(ani1, 200, fps = 10,  width = 1000, height = 700, 
#       renderer = gifski_renderer("Ani 1 - Monthly wheat price animated.gif"))

# you can also save as 
# if(!require("av")) install.packages("av") 
# animate(ani1, 200, fps = 10,  width = 1000, height = 700, 
#      renderer = av_renderer("Ani 1 - Monthly wheat price animated.mp4"))


# MULTI LINE ANIMATED GRAPH-----------------------------------------------------

cp$Date <- as.Date(cp$Date, format = "%m/%d/%Y")
cp$Commodity <- as.factor(cp$Commodity)
cp$Index <- as.numeric(cp$Index)

# create a multi-line static plot
fig9 <- ggplot(cp, aes(x = Date, y = Index, colour = Commodity)) + 
  geom_line(size = 1.5) + 
  xlab("") + ylab("FAO Food Price Index (2014-2016 = 100)") +
  scale_x_date(breaks = date_breaks("3 months"), labels = date_format("%b %y")) +
  labs(subtitle = "Source: https://www.fao.org/worldfoodsituation/FoodPricesIndex/en/") + 
  scale_colour_jcolors(palette = "pal2") +
  scale_y_continuous(labels = comma)  +
  theme_few() +
  theme(legend.position = "none", 
        axis.title.y = element_text(size = 24, face = "bold", color = "grey90"),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 20,  face = "bold", color = "grey90"),
        plot.subtitle = element_text(color = "grey90", size = 16, face = "italic", hjust=1),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey10"),
        panel.border = element_blank(),
        axis.ticks = element_line(color = "grey90"),
        axis.line.x.bottom=element_line(color = "grey90"),
        axis.line.y.left=element_line(color = "grey90")) 

fig9

# add dots and animate
fig9 +
  geom_point(size = 8) + 
  transition_reveal(Date)

# name the animated plot, add segment and text
# note the change of plot margin to allow space for the text
# clip = "off" in coord_cartesian allows drawing in the plot margins.
ani2 <- fig9 +
  geom_point(size = 8) + 
  geom_segment(aes(xend = max(Date) + 1, yend = Index), linetype = 2, size = 1) + 
  geom_text(aes(x = max(Date) + 5, label = Commodity), hjust = 0, size = 10) + 
  theme(plot.margin = margin(20, 80, 20, 20)) + 
  coord_cartesian(clip = 'off') + 
  transition_reveal(Date)

ani2

# save as gif
# animate(ani2, 200, fps = 15,  width = 1000, height = 700, 
#       renderer = gifski_renderer("Ani 2 - FAO price index animated.gif")) 


# ANIMATED PANEL----------------------------------------------------------------

# create a facet plot from fig9
fig10 <- fig9 + facet_wrap(~ Commodity, scales = "free")

# modify strips
fig10 <- fig9 + 
  facet_wrap(~ Commodity, scales = "free") +
  theme(strip.background = element_rect(fill = "grey10", size = 0.1),
        strip.text = element_text(color = "grey90", size = 18, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold", color = "grey90", margin = margin(0,10,0,0)),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 12,  face = "bold", color = "grey90"),
        plot.caption = element_text(color = "grey90", size = 10, face = "italic", hjust=1, margin = margin(10,0,0,0)))

# name and animate
ani3 <- fig10 +
  transition_reveal(Date)

ani3

# save as gif
# animate(ani3, 200, fps = 15,  width = 1300, height = 600, 
#        renderer = gifski_renderer("Ani 3 - FAO price index panel animated.gif")) 


# THE END-----------------------------------------------------------------------
# I HOPE YOU ENJOYED!



