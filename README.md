# stat-628-module-2
Group members: Yudi Mu, Wenyi Wang, Yuechuan Chen.  
This repository is for STAT 628 Module 2, which is a project to build up a model for body fat percentage of men.  
There are four folders in the repository: data, code, image and summary.  
## data folder
The raw data is Bodyfat.csv, which includes age, weight, height, bmi, and various body circumference measurements. The variables are listed below:
ID number of individual,   
Percent body fat from Siri's(1956) equation,  
Density determined from underwater weighing,  
Age(years),  
Weight(lbs),  
Height(inches),  
Adioposity(bmi),  
Neck circumference (cm),  
Chest circumference (cm),  
Abdomen 2 circumference (cm),  
Hip circumference (cm),  
Thigh circumference (cm),  
Knee circumference (cm),  
Ankle circumference (cm),  
Biceps (extended) circumference (cm),  
Forearm circumference (cm),  
Wrist circumference (cm).  
The cleaned data is the data after deleting the outliers, revising the calculation errors, removing unused variables, and changing "cm" to "inch". 
## code folder
In cleandata.R, it has a first look into the data and find the potential wrong calculation and outliers in the raw data, and the result is saved in the cleaned data file.  
The code.R includes all other parts where we biuld up the model and do hypothesis testing and draw conclusion on the rule of thumb.
## image folder
The plots included in the folder are:


## summary folder
This folder contains the PDF file of our summary of Module2.

# Shiny app
Besides, our Shiny App can be found here: 

