

library (tidyverse)
library(readxl)

Netflix_Merge_final <- read_excel("DataSet/Netflix Title Merge final.xls")
View(Netflix_Title_Merge_final)


names(Netflix_Merge_final)
glimpse(Netflix_Merge_final)
summary(Netflix_Merge_final)



## convertir a formato fecha - campo "date_added"

Netflix_Merge <- Netflix_Merge_final %>% 
  mutate(date_added = as.Date(Netflix_Merge_final$date_added, format = "%B%d,%Y"))


glimpse(Netflix_Merge)
summary(Netflix_Merge)
