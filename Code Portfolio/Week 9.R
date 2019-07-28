#Week 9 Practice
#Plotting Systems

data(cars)
with(cars, plot(speed, dist)) #Create the plot / draw canvas
title("Speed vs. Stopping distance") #add chart title


library(lattice)
#a lattice plot of the relationship between life expectancy and income 
#and how that relationship varies by region in the United States
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4, 1))

install.packages("ggplot2")
library(ggplot2)

data(mpg)
qplot(displ, hwy, data = mpg) #basic ggplot

#Graphics Devices

with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful Geyser data")

#save plot in PDF file
pdf(file = "myplot.pdf")
with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful Geyser data")
dev.off() 

#copy plotss to a image file
library(datasets)
with(faithful, plot(eruptions, waiting))  
title(main = "Old Faithful Geyser data")  
dev.copy(png, file = "geyserplot.png")  
dev.off()  

