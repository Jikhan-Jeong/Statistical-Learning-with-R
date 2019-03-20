## 2019_03_20_r_woring_group_excel
## reference: r_working_group so data is not mine, this is the reason I am not upload it.

library(ISLR)
library(ElemStatLearn)
library(e1071)

getwd()
setwd('C:/rstudio/0_2019_spring/')

library(readxl)

excel_sheets(path="fed_prison_pop.xlsx") # [1] "Table 1"

read_excel(path="fed_prison_pop.xlsx",
           sheet ="Table 1",
           skip =1,
           col_names= TRUE)
install.packages('openxlsx')
library(openxlsx)

datalist <- list(iris = iris, PlantGrowth = PlantGrowth)
str(datalist)  
write.xlsx(x = datalist, file ="plant_data.xlsx")

read.xlsx(xlsxFile = "plant_data.xlsx", 
          sheet = "iris",
          colNames = TRUE,
          startRow = 1
          )

library(data.table)

rodents <- fread(file = "rodents.csv")
fread(file = "rodents.csv",
      integer64 = "character")


fwrite(x = rodents, file = "rodents_2.csv")

str(read.csv(file ="rodents_2.csv"))

saveRDS(object = rodents, file ="rodents.rds")

rodents2 <- readRDS(file ="rodents.rds")
str(rodents2)
