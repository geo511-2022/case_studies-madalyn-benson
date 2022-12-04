library(tidyverse)
dataurl="https://data.giss.nasa.gov/tmp/gistemp/STATIONS/tmp_USW00014733_14_0_1/station.txt"
httr::GET("https://data.giss.nasa.gov/cgi-bin/gistemp/stdata_show_v4.cgi?id=USW00014733&ds=14&dt=1")
read_table(dataurl)
temp=read_table(dataurl,
                skip=3, #skip the first line which has column names
                na="999.90", # tell R that 999.90 means missing in this dataset
                col_names = c("YEAR","JAN","FEB","MAR", # define column names 
                              "APR","MAY","JUN","JUL",  
                              "AUG","SEP","OCT","NOV",  
                              "DEC","DJF","MAM","JJA",  
                              "SON","metANN"))
p1<-ggplot(temp,aes(YEAR,JJA))+geom_line()+geom_smooth()+ylab("Mean Summer Temperatures (C)")+xlab("Year")+ggtitle("Mean Summer Temperatures in Buffalo, NY",subtitle="Summer includes June, July, and August 
Data from the Global Historical Climate Network
Blue line is a LOESS Smooth")
png(file="Week 02 Case Study PNG", width=480, height=300)
p1
dev.off()
