## 2019_02_19_R_for_datacheince_ch4
## reference: https://r4ds.had.co.nz/workflow-basics.html


getwd()
setwd("C:/rstudio/0_2019_spring/statistical learning/r_data_science")
seq(1, 10)

(x<- "hello world") # () print the results


(my_variable <- 10)
my_variable

library(tidyverse)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y =hwy))

filter(mpg, cyl== 8 )
filter(diamonds, carat >3)

