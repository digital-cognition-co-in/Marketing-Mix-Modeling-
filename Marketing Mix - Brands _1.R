# Marketing Mix - Brands 
# Customer ratings for - perceptual adjectives and 4 brands.
# Within marketing analytics - perceptual adjectives to be linked to perceptual maps.
d <- read.csv("C:/Users/Rohit/Desktop/Marketing Mix - Brands/d.csv")
head(d)
summary(d)
# scaling the raw data  - creating a scaled matrix
dsc<-scale(d[,1:20])
# creating a Data Frame of same name - "dsc" from scaled matrix 
dsc<-as.data.frame(dsc)
summary(dsc)
# as seen in summary - "mean" for all - dimensions - is now "0.00" 

# Now we create a merged data frame "dsc1" - adding the "brand.name" variable to the 
# scaled data frame created earlier 
dsc1<-cbind(dsc,d$brand.name)
# change the - "d$brand.name" to just "brand.name"
names(dsc1) [21] <- "brand.name"

library(corrplot)

# varied permutations of the bi variable correlation plot....
# too many variables thus creating seperate Correlation Plots 
c1<-cor(dsc1[, 1:5])
c2<-cor(dsc1[, 6:10])
c3<-cor(dsc1[, 10:15])
c4<-cor(dsc1[, 16:20])

layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
corrplot.mixed(c1,lower="number", upper="circle")
corrplot.mixed(c2,lower="number", upper="circle")
corrplot.mixed(c3,lower="number", upper="circle")
corrplot.mixed(c4,lower="number", upper="circle")

corrplot(c3,type="upper", method="number",order="AOE")
# "AOE" for the angular order of the eigenvectors
corrplot(c4,type="upper", method="shade",order="hclust")
# "hclust" for the hierarchical clustering order.
corrplot(c3,type="upper", method="shade",order="FPC")
# "FPC" for the first principal component order.
corrplot(c4,type="upper", method="ellipse",order="FPC")
# "FPC" for the first principal component order.
#  method="ellipse" 
corrplot(c4,type="upper", method="color",order="hclust")
# "FPC" for the first principal component order.
#  method="color" 
corrplot(c3,type="upper", method="shade",order="hclust")
# "FPC" for the first principal component order.
#  method="shade"  - not very different from "color"

###
# Whats the average or the "mean" rating for each brand 
# for all given - 20 - perceptual adjectives ? 
###

avg.ratings <- aggregate(.~ brand.name , data=dsc1 , mean)
View(avg.ratings)
library(gplots)
library("RColorBrewer", lib.loc="~/R/win-library/3.1")
heatmap.2(as.matrix(c3),col=brewer.pal(9, "GnBu"),
          xlab=" Perceptual Adjectives ", ylab= "Coded Brand Names",trace="none", key=T, dend="none",main="\n\n\n Heat Map \n\n Perceptual Adj. Avg Ratings")

heatmap.2(as.matrix(c4),col=brewer.pal(7,"Greens"),
          xlab=" Perceptual Adjectives ", ylab= "Coded Brand Names",trace="none", key=T, dend="none",main="\n\n\n Heat Map \n\n Perceptual Adj. Avg Ratings")
# 
heatmap.2(as.matrix(c4),col=brewer.pal(9,"BrBG"),
          xlab=" Perceptual Adjectives ", ylab= "Coded Brand Names",trace="none", key=T, dend="none",main="\n\n\n Heat Map \n\n Perceptual Adj. Avg Ratings")
# 
heatmap.2(as.matrix(avg.ratings),col=brewer.pal(9,"BrBG"),
          xlab=" Perceptual Adjectives ", ylab= "Coded Brand Names",trace="none", key=T, dend="none",main="\n\n\n Heat Map \n\n Perceptual Adj. Avg Ratings")
#
# As the average ratings are mostly in the "0.000" range we need to 
# multiply with "1000" across to get a decent data viz with the HeatMap . 
getwd()
write.table(avg.ratings,"C:/Users/Rohit/Desktop/Marketing Mix - Brands/avg.csv",sep=",")
# Excel calc - external to R - now import avg1.csv 
avg1 <- read.csv("C:/Users/Rohit/Desktop/Marketing Mix - Brands/avg1.csv")
View(avg1)
#
heatmap.2(as.matrix(avg1),col=brewer.pal(9,"BrBG"),
          xlab=" Perceptual Adjectives ", ylab= "Coded Brand Names",trace="none", key=T, dend="none",main="\n\n\n Heat Map \n\n Perceptual Adj. Avg Ratings")
#
heatmap.2(as.matrix(avg1),col=brewer.pal(9, "GnBu"),
          xlab=" Perceptual Adjectives ", ylab= "Coded Brand Names",trace="none", key=T, dend="none",main="\n\n\n Heat Map \n\n Perceptual Adj. Avg Ratings")
#

