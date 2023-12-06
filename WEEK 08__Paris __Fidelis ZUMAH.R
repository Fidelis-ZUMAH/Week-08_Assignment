#loading the data
clim <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv", sep = ";")
#viewing the first 5 rows of the datasrt
head(clim)

#checking the data structure
str(clim)

#uncoding data structure of altitude and p_mean from characters to numbers
clim$altitude <- as.numeric(gsub(",", "", clim$altitude))
clim$p_mean <- as.numeric(gsub(",", "", clim$p_mean))
str(clim)

#packages for maps
install.packages("raster")
library(raster)
install.packages("sp")
install.packages("maps")

library(raster)
library(sp)
library(maps)
library(ggplot2)
G1 <- raster::getData(country = "France", 
                      level = 1)

G1 <- raster::getData(country = "France", level = 1)

ggplot() +
  geom_polygon(
    data = G1,
    aes(x = long, y = lat, group = group),
    colour = "grey10", fill = "#fff7bc"
  ) +
  geom_point(
    data = clim,
    aes(x = lon, y = lat),
    alpha = .5,
    size = 2
  ) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_map()

#Exluding the high mountains extremes
climfrar<-clim[1:34,]

#model for annual temperature 
Tem_Model <- lm(t_mean ~ altitude + lat + lon, data = climfrar)
summary (Tem_Model) 


# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 37.2650364  2.6220099  14.212 7.29e-15 ***
#   altitude    -0.0064139  0.0008688  -7.383 3.17e-08 ***
#   lat         -0.5339603  0.0557546  -9.577 1.24e-10 ***
#   lon          0.0321010  0.0395728   0.811    0.424  

Mean_Annual_Temperature = 37.265 - 0.006414*altitude - 0.534*latitude + 0.0321*longitude

#Interpretation
#The Intercept (37.265036): This is the estimated value of the annual temperature when all predictor variables
#(altitude, latitude, and longitude) are zero.  It  means the annual temperature of Paris depends on altitude and latitude
#(a= 37.265, SE = 2.622, p<0.001)


# for every increase in altitude by one unit the average mean annual temperature 
# decreases by 0.0064 units(SE = 0.00087, p<0.001)

# Lfor every increase in latitude by one unit the average mean average temperature 
# decreases by 0.534 units(SE = 0.0557, p<0.001)


# for every increase in longitude by one unit the average mean average temperature 
# increases by 0.0321 units(SE = 0.03957, p>0,001)


model2 <- lm(t_mean~ altitude + lat, data = climfrar)
summary(model2)


# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 37.9147567  2.4828724   15.27 5.68e-16 ***
#   altitude    -0.0062643  0.0008443   -7.42 2.34e-08 ***
#   lat         -0.5465325  0.0532610  -10.26 1.72e-11 ***


Mean_Annual_Temperature = 37.9147 - 0.00626*altitude - 0.547*latitude

#prediction for mont ventoux
prediction_Mont_ventoux <- model2$coefficients[1] + model2$coefficients[2]*(1212) + model2$coefficients[3]*(44.16)
prediction_Mont_ventoux

#prediction for pic duc midi
prediction_pic_duc_midi <- 37.9147 + model2$coefficients[2]*(2860) + model2$coefficients[3]*(42.93)
prediction_pic_duc_midi

#predicted mean
predicted_t_mean <- predict(model2, newdata = list("altitude" = c(1212, 2860), "lat" = c(44.2, 42.9)), interval = "p", level = 0.95)
predicted_t_mean  

# The prediction for the mean annual temperature of Mont-Ventoux is 6.17 
#with  that of the predicted mean annual temperature falling being within the predicted range from 3.79 to 8.54.


# The predicttion for the mean annual temperature of Pic-du-midi is -3.45 
#with that of the predicted mean annual temperature falling with the predicted range from -8.35 to 1.45.

#3d scatter plot
library(ggplot2)
install.packages("scatterplot3d")
library(scatterplot3d)

scatter_3d <- with(climfrar, scatterplot3d(altitude, lat, t_mean,
                                           pch = 16, highlight.3d = TRUE, angle = 45))
scatter_3d$plane3d(model2)
summary(scatter_3d)

# The intercept shows the mean annual temperature of 37.915 for a altitude, 
# latitude and longitude with zero values (a= 37.915, SE = 2.483, p<0.001)

# for every increase in altitude by one unit the average mean annual temperature 
# decreases by 0.00636 units(b= 0.00364,SE = 0.000844, p<0.001)

# for every increase in latitude by one unit the average mean annual temperature 
# decreases by 0.547 units(c= 0.547,SE = 0.0533, p<0.001)

# The r squared value of 83% indicated  that the proportion of variance in the mean annual
# temperature that is explained by altitude and latitude.

# The  p value of 1.268e-12 shows that altitude and latitude are significantly related to mean annual temperature.





