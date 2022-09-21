data(iris)
install.packages("dplyr")
library(dplyr)
data(iris)
head(iris)
iris$Petal.Length
selected_Column<-dplyr::select(iris, Petal.Length)
iris[,3]
petal_length_mean<-mean(iris[,3])
hist(iris[,3], main="Petal Length Histogram",xlab="Petal Length",col="green")

