library("spTimer")
library("akima")
#library("coda")
library("spacetime")
library("fields")
#library("forecast")
library("MASS")
library("colorspace")
library("maps")
library("MBA")

surfplot <- function(val, ...) {
  surf <- val
  surf <- mba.surf(surf, 200, 200)$xyz
  image.plot(surf, xlab = "Longitude", ylab = "Latitude", axes = F, ...)
  contour(surf, nlevels = 10, lty = 3, add = TRUE)
  axis(1)
  axis(2)
}
s2011 = read.csv("Data/s2011.csv")
f2011 = read.csv("Data/f2011.csv")
s2012 = read.csv("Data/s2012.csv")
f2012 = read.csv("Data/s2012.csv")
s2013 = read.csv("Data/s2013.csv")
f2013 = read.csv("Data/s2013.csv")
s2014 = read.csv("Data/s2014.csv")
f2014 = read.csv("Data/s2014.csv")
s2015 = read.csv("Data/s2015.csv")
f2015 = read.csv("Data/s2015.csv")
s2016 = read.csv("Data/s2016.csv")
f2016 = read.csv("Data/s2016.csv")
data_list = list(s2011,f2011,s2012,f2012,s2013,f2013,s2014,f2014,s2015,f2015,s2016,f2016)

names = unique(s2011$name)
names = as.character(names)

plot_contour = function(season.year, sub_basin){
  data_plot = data_list[[season.year]]
  data_plot = data_plot[data_plot[,5] == names[sub_basin],]
  data_plot_new = data.frame(data_plot[,4],data_plot[,3],data_plot[,2])
  surfplot(val = data_plot_new, main = "Contour ")
}

