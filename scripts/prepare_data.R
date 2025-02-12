#  clean and manipulate data issue #6 ####

# clear environment ####
rm(list = ls())

# load libraries ####
library(ggplot2)


# load data ####

trees <- read.csv("data/PointDendrometerTrees.csv")
installs <- read.csv("data/PointDendrometerInstalls.csv")


all_data <- NULL

for(band in trees$dendrometerID) {
  
  idx <- trees$dendrometerID %in% band
  
  files <-  list.files("data/master_data_records/", paste("data", as.character(band), sep = "_"), full.names = T)
  if(length(files) > 0) {  
    x <-  do.call(rbind, lapply(files, read.csv, sep = ";", h = F, row.names = 1, dec = ","))
    
    
    # rename columns we know about
    
    head(x)
    colnames(x)[1] <- "timestamp"
    colnames(x)[3] <- "temperature"
    colnames(x)[6] <- "value" # micrometers ?
    head(x)
    
    # remove the duplicated timestamp
    x <- x[!duplicated(x$timestamp,fromLast = TRUE), ] 
    
    # convert dates and units
    x$timestamp <- as.POSIXct(x$timestamp, format = "%Y.%m.%d %H:%S", tz = "UTC")
    # all_data$mm <- all_data$mum * 0.001
    # all_data$dendrometerID <- factor(all_data$dendrometerID)
    
    str(x)
    
    # remove data prior to instalation
    instalation_day <- as.Date(paste(trees$dendro.start.year[idx], trees$dendro.start.month[idx], trees$dendro.start.day[idx], sep = "-"))
    
    x <- x[as.Date(x$timestamp) > instalation_day, ]
    
    # calculate mm from measurement value
    
    x$mm <- x$value * 0.001  #(x$value - x$value[1])*(8890/(34000-1279)) * 0.001 # see LolluHandbppl.pdf page 14: The full range of digital numbers is from 1 279 up to 34 000. The curve intersects with the y axis at point 1 279; here the measurement is 0 µm. From this we may deduce the proportionality constant: 8 890/(34 000 – 1 279) And finally the formula for converting the digital number to micrometers:   µm = (Value – 12 79)*{8 890/(34 000-1 279)}
    
    if(nrow(x)>0 ) all_data <- rbind(all_data, 
                      data.frame(dendrometerID = band, x))
    
  }
  
}
# 
# # rename columns we know about
# 
# unique(all_data$dendrometerID)
# head(all_data)
# colnames(all_data)[2] <- "timestamp"
# colnames(all_data)[4] <- "temperature"
# colnames(all_data)[7] <- "mum" # micrometers
# head(all_data)
# 
# 
# # convert dates and units
# all_data$timestamp <- as.POSIXct(all_data$timestamp, format = "%Y.%m.%d %H:%S")
# all_data$mm <- all_data$mum * 0.001
all_data$dendrometerID <- factor(all_data$dendrometerID)

str(all_data)

# plot

# coeff = max(all_data[all_data$V3 %in% 0,]$temperature)/max(all_data[all_data$V3 %in% 0,]$mm)

all_data$sp <- trees$sp[match(all_data$dendrometerID, trees$dendrometerID)]

all_data$ID <- paste(trees$dbh.2018.mm[match(all_data$dendrometerID, trees$dendrometerID)]/10, "cm -", all_data$dendrometerID)

ggplot(data = all_data[all_data$V3 %in% 0,],) +
  geom_line(aes(x = timestamp, y = mm, color = ID)) +
  # geom_line(aes(x = timestamp, y = temperature/coeff)) +
  facet_wrap(~sp) #, scales = "free_y") 
  # scale_y_continuous(minor_breaks = seq())
  # scale_y_continuous(sec.axis = sec_axis(~.*coeff, name = "Temperature [C]"))
ggsave("results/full_record_dendroband_plots.png", width = 10, height = 10, units = "in", dpi = 300)


ggplot(data = all_data[all_data$V3 %in% 0 & format(all_data$timestam, "%m-%d") %in% paste0("07-", c(18:24)),
],) +
  geom_line(aes(x = timestamp, y = mm, color = ID)) +
  # geom_line(aes(x = timestamp, y = temperature/coeff)) +
  facet_wrap(~sp, scales = "free_y") 
# scale_y_continuous(minor_breaks = seq())
# scale_y_continuous(sec.axis = sec_axis(~.*coeff, name = "Temperature [C]"))

ggsave("results/dendroband_plots_July18-24.png", width = 10, height = 10, units = "in", dpi = 300)

