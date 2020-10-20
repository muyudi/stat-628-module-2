setwd("/Users/yudimu/Desktop/628/Module2/main")
bodyfat = read.csv("BodyFat.csv")
#There are 17 variables
head(bodyfat)
attach(bodyfat)
#Summary of data
summary(bodyfat)

#bodydat has potential outliers, height has impossible minimum
library(ggplot2)
measure = data.frame(IDNO, WEIGHT, BODYFAT, HEIGHT)

weight.out = which(WEIGHT == max(WEIGHT))
ggplot(measure, aes(x = IDNO, y = WEIGHT)) +
  geom_point(col = "navy") +
  geom_point(aes(x = weight.out, y = WEIGHT[weight.out]), 
             shape = 1, cex = 7, col = "red") +
  theme(legend.position="none") +
  labs(title="Weight") +
  theme(plot.title = element_text(hjust = 0.5)) 

bodyfat.out = which(BODYFAT == 0)
ggplot(measure, aes(x = IDNO, y = BODYFAT)) +
  geom_point(col = "navy") +
  geom_line(aes(y = 0), col = "red") +
  geom_point(aes(x = bodyfat.out, y = BODYFAT[bodyfat.out]), 
             shape = 1, cex = 7, col = "red") +
  theme(legend.position="none") +
  labs(title="Bodyfat") +
  theme(plot.title = element_text(hjust = 0.5)) 

height.out = which(HEIGHT == min(HEIGHT))
ggplot(measure, aes(x = IDNO, y = HEIGHT)) +
  geom_point(col = "navy") +
  geom_point(aes(x = height.out, y = HEIGHT[height.out]), 
             shape = 1, cex = 7, col = "red") +
  theme(legend.position="none") +
  labs(title="Height") +
  theme(plot.title = element_text(hjust = 0.5)) 


#change weight.outlier & height.outlier
bodyfat[c(weight.out, height.out), ]
weight = 48.9/703*(72.25)^2 #this is not an outlier
height = sqrt(703*205/29.9)#the min height is data is error
bodyfat[height.out,]$HEIGHT = height

#bodyfat.outlier
#495/bodyfat[c(bodyfat.out), 3] - 450
#bodyfat1 = bodyfat[-bodyfat.out,]
#nrow(bodyfat); nrow(bodyfat1)
detach(bodyfat)
attach(bodyfat)

#Test Bodyfat (checked "Siri's equation")
cal.bodyfat = 495/DENSITY-450
plot(BODYFAT, cal.bodyfat, pch = 16, col = "blue", cex = 0.6, 
     main = "Check Bodyfat")
out.bodyfat = unique(c(which(abs(cal.bodyfat - BODYFAT) > 3), which(BODYFAT<2)))
points(BODYFAT[out.bodyfat], cal.bodyfat[out.bodyfat], cex = 2)
text(BODYFAT[out.bodyfat], cal.bodyfat[out.bodyfat], 
     labels = out.bodyfat, pos=4)


#Test BMI (checked BMI equation)
bmi = 730*WEIGHT/(HEIGHT)^2
plot(ADIPOSITY, bmi, pch = 16, col = "blue", cex = 0.6,
     main = "Check Adiposity")
out.bmi = which(abs(bmi - ADIPOSITY) > 1.9)
points(ADIPOSITY[out.bmi], bmi[out.bmi], cex = 2)
text(ADIPOSITY[out.bmi], bmi[out.bmi], labels = out.bmi, pos=c(3,4))


#Change to inch
coef = 0.393700787 
bodyfat.new = bodyfat[-c(out.bmi, out.bodyfat), c(2, 4:6, 8:17)]
bodyfat.new[, 5:14] = round(bodyfat[-c(out.bmi, out.bodyfat), 8:17]*coef, 2)


#Other variables
#Age
age = ggplot(bodyfat, aes(AGE)) +
  geom_histogram(binwidth = 4, fill = "cyan", colour = "blue")
# Abdomen
abdomen = ggplot(bodyfat, aes(ABDOMEN)) +
  geom_histogram(binwidth = 4, fill = "cyan", colour = "blue")
#Neck
neck = ggplot(bodyfat, aes(NECK)) +
  geom_histogram(binwidth = 1, fill = "cyan", colour = "blue")
# Chest
chest = ggplot(bodyfat, aes(CHEST)) +
  geom_histogram(binwidth = 4, fill = "cyan", colour = "blue")
# Thigh
thigh = ggplot(bodyfat, aes(CHEST)) +
  geom_histogram(binwidth = 4, fill = "cyan", colour = "blue")
# Hip
hip = ggplot(bodyfat, aes(HIP)) +
  geom_histogram(binwidth = 4, fill = "cyan", colour = "blue")
# Knee
knee = ggplot(bodyfat, aes(KNEE)) +
  geom_histogram(binwidth = 1, fill = "cyan", colour = "blue")
# Wrist
wrist = ggplot(bodyfat, aes(WRIST)) +
  geom_histogram(binwidth = 1, fill = "cyan", colour = "blue")



library(gridExtra)
empty = ggplot(data.frame(x=1:10,y=1:10),aes(x,y))+
  annotation_raster(age, -Inf, Inf, -Inf, Inf)+
  opts(axis.title.x=theme_blank(), 
       axis.title.y=theme_blank(),
       axis.text.x=theme_blank(),
       axis.text.y=theme_blank(),
       axis.ticks=theme_blank())
grid.arrange(age, abdomen, neck, chest,  
             ncol=2, nrow=2)
grid.arrange( thigh, hip, knee, wrist,
              ncol=2, nrow=2)


#write out cleaned data
detach(bodyfat)
attach(bodyfat.new)
write.csv(bodyfat.new, file = "cleaned data.csv")

