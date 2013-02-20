#AUTHOR: Robert Clements
#DATE: February 18, 2013
#SUMMARY: Fit an ordinal regression model, support vector machine, and a gam with a spatial term,
#to the cleaned up DBMI database. 
require(mgcv)
require(ordinal)
require(e1071)
require(ggplot2) #for some very nice plotting
require(ggmap) #for including maps
require(RColorBrewer)


#############
#load data and explore it

ipe.data <- read.table("Raw data/all_clean_ordinal_data.txt", header = TRUE)
head(ipe.data)

p <- ggplot(ipe.data, aes(factor(MCS), dist))
p + geom_boxplot()

p <- ggplot(ipe.data, aes(factor(MCS), Mw))
p + geom_boxplot()

p <- ggplot(ipe.data, aes(dist, MCS, alpha = .1))
p + geom_point() + theme(legend.position="none")

brewer.div <- colorRampPalette(brewer.pal(9, "RdBu"), interpolate = "spline")
cols <- (brewer.div(10))

p <- ggplot(ipe.data, aes(log(dist), Mw, color = MCS)) 
p + geom_point() + 
  scale_colour_gradientn(colours = cols, 
                         breaks = 2:11, 
                         guide = "legend")


################
#visualize the locations

locations <- unique(ipe.data[,c("lon", "lat")])
#ity = get_map(location = "italy", zoom = 5, source = "osm", color = "bw")
#open street map's server is often unavailable, so I'd suggest saving this map
#save(ity, file = "ity")
load("ity")
locs <- ggmap(ity)
locs <- locs + 
  geom_point(data = locations, mapping = aes(x = lon, y = lat))
locs


################
#start modeling
#ordered logistic model first

#make MCS a factor, which it is
ipe.data$MCS <- as.factor(ipe.data$MCS)

#start with ordinal regression with cumulative link 
formula1 <- MCS ~ Mw + dist

model.ord <- clm(formula1, data = ipe.data, link = "logit", threshold = "flexible")
summary(model.ord)


################
#svm next

#support vector machine with defaults 
formula1 <- MCS ~ Mw + dist

model.svm <- svm(formula1, data = ipe.data, type = "C-classification", kernel = "radial")

#circles are support vectors, triangles are the rest of the data
#terrain colors are our different classes, rainbow colors are the classes that
#each point belongs to
plot(model.svm, ipe.data, Mw~dist, dataSymbol = 2, svSymbol = 1, symbolPalette = rainbow(10),
     color.palette = terrain.colors)


################
#gam last

#gam with smooth terms and spatial term 
gam.data <- ipe.data
gam.data$MCSgam <- as.numeric(gam.data$MCS)

formula.gam <- MCSgam ~ te(dist, Mw) + s(dist) + s(Mw) + s(lon, lat, bs="tp", k=10)

model.gam <- gam(formula.gam, data = gam.data)

summary(model.gam)


################
#load testing data and get our IPE model predictions for comparison

#our testing data and IPE predictions
test.data <- read.table("ipe_test_data1.txt", h=TRUE)
IPE1 <- test.data$Ipre
temp.data <- read.table("ipe_test_data2.txt", h=TRUE)
IPE2 <- temp.data$Ipre

test.data.internet <- read.table("ipe_test_data_internet1.txt", h=TRUE)
IPE.internet1 <- test.data.internet$Ipre
temp.data <- read.table("ipe_test_data_internet2.txt", h=TRUE)
IPE.internet2 <- temp.data$Ipre

rm(temp.data)

pred.ord <- predict(model.ord, newdata=test.data, type="class")[[1]]
pred.svm <- predict(model.svm, newdata=test.data)
pred.gam <- round(predict(model.gam, newdata=test.data))

pred.ord.internet <- predict(model.ord, newdata=test.data.internet, type="class")[[1]]
pred.svm.internet <- predict(model.svm, newdata=test.data.internet)
pred.gam.internet <- round(predict(model.gam, newdata=test.data.internet))


################
#compare using mae

#use mae, which may not make a lot of sense
mae.ord <- mean(abs(test.data$MCSgam-as.numeric(pred.ord)))
mae.svm <- mean(abs(test.data$MCSgam-as.numeric(pred.svm)))
mae.gam <- mean(abs(test.data$MCSgam-(pred.gam)))

mae.ord.internet <- mean(abs(test.data.internet$MCSgam-as.numeric(pred.ord.internet)))
mae.svm.internet <- mean(abs(test.data.internet$MCSgam-as.numeric(pred.svm.internet)))
mae.gam.internet <- mean(abs(test.data.internet$MCSgam-(pred.gam.internet)))

mae.all <- c(mae.ord, mae.svm, mae.gam)
mae.all.internet <- c(mae.ord.internet, mae.svm.internet, mae.gam.internet)

#compare with our two Italy IPEs
IPE1.mcs <- round(IPE1)-1
IPE2.mcs <- round(IPE2)-1
IPE.internet1.mcs <- round(IPE.internet1)-1
IPE.internet2.mcs <- round(IPE.internet2)-1

mae.IPE1 <- mean(abs(test.data$MCSgam-IPE1.mcs))
mae.IPE2 <- mean(abs(test.data$MCSgam-IPE2.mcs))
mae.IPE.internet1 <- mean(abs(test.data.internet$MCSgam-IPE.internet1.mcs))
mae.IPE.internet2 <- mean(abs(test.data.internet$MCSgam-IPE.internet2.mcs))

mae.IPE <- c(mae.IPE1, mae.IPE2)
mae.IPE.internet <- c(mae.IPE.internet1, mae.IPE.internet2)

mae.all
mae.IPE

mae.all.internet
mae.IPE.internet