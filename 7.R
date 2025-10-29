
library(ggplot2)
ggplot(mtcars, aes(x = hp, y = mpg)) +
 geom_point(color = "red", size = 3) +
 labs(title = "Scatterplot: MPG vs Horsepower", x = "Horsepower", y = "Miles per Gallon") +
 theme_minimal()
ggplot(mtcars, aes(x = mpg)) +
 geom_histogram(binwidth = 2, fill = "blue", color = "black") +
 labs(title = "Histogram: MPG Distribution", x = "Miles per Gallon", y = "Frequency") +
 theme_minimal()
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
 geom_boxplot(fill = "red", color = "darkgreen") +
 labs(title = "Boxplot: MPG by Cylinders", x = "Number of Cylinders", y = "Miles per Gallon")+
 theme_minimal()

ggplot(mtcars, aes(x = mpg)) +
 geom_dotplot(binwidth = 1, fill = "lightblue") +
 labs(title = "Dotplot: MPG Distribution", x = "Miles per Gallon", y = "Count") +

 theme_minimal()
