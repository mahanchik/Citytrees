install.packages("xlsx", dep = T)
install.packages("readr")
install.packages("ggplot2")
install.packages("tidyverse")
library (xlsx)
library (readr)
library (ggplot2)
library (tidyverse)


######## Первая домашка ########

Citytrees <- read_delim("Desktop/Citytrees.csv", 
                        ";", escape_double = FALSE, col_types = cols(Tno = col_double()), 
                        locale = locale(decimal_mark = ","), 
                        trim_ws = TRUE)
View(Citytrees)

plot(Citytrees$`Ht (m)`, Citytrees$`Crown Diameter (m)`)

ggplot(Citytrees, aes(x = `Ht (m)`, y = `Crown Diameter (m)`, color = Species)) + geom_point()


######## Вторая домашка ########

# Все переменные имеют корректный тип данных
# Повторяющиеся переменные убраны
# Из имен переменных убраны размерности
# Всем переменам заданы их реальные размерности
# Если какая-то переменная является ошибкой другой переменной, она должна быть убрана и добавлена в виде ошибки к основной переменной
# Категориальные переменные должны быть факторами
# Категории переменной из имени должны быть убраны
# Коды категориальных переменных заменены их категориями
# Должны быть созданы переменные координат(lat,lon) в британской системе координат(с учетом кодов квадратов) и в WGS84
# Виды должны быть переименованы на латыне



# Все переменные имеют корректный тип данных

Все переменные имеют корректный тип данных, тк мы через Import Dataset в графе Decimal mark изменили (.) на (,)



# Повторяющиеся переменные убраны

Citytrees = Citytrees %>% select(-`dbh (mm)`, -HR)

Из переменных убрали dbh (mm) и HR-пустую колонку



# Из имен переменных убраны размерности

Citytrees = Citytrees %>% rename(dbh = `dbh (m)`)
Citytrees = Citytrees %>% rename(Ht = `Ht (m)`)
Citytrees = Citytrees %>% rename(Clearance_Ht = `Clearance Ht (m)`)
Citytrees = Citytrees %>% rename(Crown_Depth = `Crown Depth (m)`)
Citytrees = Citytrees %>% rename(Average_Radial_Crown_spread = `Average Radial Crown spread (m)`)
Citytrees = Citytrees %>% rename(Total_Mean_Radial_Crown_Spread = `Total Mean Radial Crown Spread (m)`)
Citytrees = Citytrees %>% rename(Crown_Diameter = `Crown Diameter (m)`)
Citytrees = Citytrees %>% rename(Stem_diameter_Jan_2017 = `Stem diameter Jan 2017 (mm)`)
Citytrees = Citytrees %>% rename(Annual_Girth_Increment = `Annual Girth Increment (mm)`)
Citytrees = Citytrees %>% rename(Two_yr_dia_gain = `2yr dia gain (mm)`)
Citytrees = Citytrees %>% rename(Total_NSEW_Radial_Crown_Spread = `Total N,S,E,W Radial Crown Spread (m)`)



# Всем переменам заданы их реальные размерности

library(units)

units(Citytrees$dbh) = as_units("m")
units(Citytrees$Ht) = as_units("m")
units(Citytrees$Clearance_Ht) = as_units("m")
units(Citytrees$Crown_Depth) = as_units("m")
units(Citytrees$Average_Radial_Crown_spread) = as_units("m")
units(Citytrees$Total_NSEW_Radial_Crown_Spread) = as_units("m")
units(Citytrees$Total_Mean_Radial_Crown_Spread) = as_units("m")
units(Citytrees$Crown_Diameter) = as_units("m")
units(Citytrees$Stem_diameter_Jan_2017) = as_units("mm")
units(Citytrees$Two_yr_dia_gain) = as_units("mm")
units(Citytrees$Annual_Girth_Increment) = as_units("mm")
units(Citytrees$`Predicted crown diamet using combined formulla`) = as_units("m")
units(Citytrees$`Predicted Crown Diameter`) = as_units("m")

Citytrees %>% as.data.frame()



# Если какая-то переменная является ошибкой другой переменной, она должна быть убрана и добавлена в виде ошибки к основной переменной

Citytrees = Citytrees %>% mutate(error = `Predicted crown diamet using combined formulla` - Crown_Diameter)
Citytrees = Citytrees %>% select(-Difference)

Citytrees = Citytrees %>% mutate(error2 = `Predicted Crown Diameter` - Crown_Diameter)
Citytrees = Citytrees %>% select(-Diference)



# Категориальные переменные должны быть факторами

library(forcats)
library(sf)

names(Citytrees)

Citytrees$`Age Index 1=Y 2=SM 3=EM 4=M`
Citytrees = Citytrees %>% mutate(`Age Index 1=Y 2=SM 3=EM 4=M` = as.numeric(`Age Index 1=Y 2=SM 3=EM 4=M`))

Citytrees = Citytrees %>%
  mutate(AgeIndex = as_factor(`Age Index 1=Y 2=SM 3=EM 4=M`)) %>%
  mutate(AgeIndex = fct_recode(AgeIndex,Y = "1", SM = "2",EM = "3", M = "4"))

Citytrees$AgeIndex[Citytrees$AgeIndex == "<NA>"]

Citytrees$AgeIndex

Citytrees$`Data Set      1=Norwich                0= Peterborough`

Citytrees = Citytrees %>% 
  mutate(DataSet = as_factor(`Data Set      1=Norwich                0= Peterborough`)) %>%
  mutate(DataSet = fct_recode(DataSet, Norwich = "1", Peterborough = "0"))

Citytrees$DataSet

Citytrees$`Pruning Index 5 = pruned within 5yrs  10 pruned between 5 and 10yrs`
Citytrees = Citytrees %>%
  mutate(PruningIndex = as_factor(`Pruning Index 5 = pruned within 5yrs  10 pruned between 5 and 10yrs`)) %>%
  mutate(PruningIndex = fct_recode(PruningIndex,`pruned within 5yrs` = "5", `pruned between 5 and 10yrs` = "10"))

Citytrees$PruningIndex
Citytrees$`Type of Prunning None= 0 CR= 1 Other = 2 Both = 3`
Citytrees = Citytrees %>%
  mutate(TypeOfPruning = as_factor(`Type of Prunning None= 0 CR= 1 Other = 2 Both = 3`)) %>%
  mutate(TypeOfPruning = fct_recode(TypeOfPruning,None = "0", CR = "1", Other = "2", Both = "3"))
Citytrees$TypeOfPruning

Citytrees$`Soil Code 1=sand and gravel 2= Clay 3=silt`
Citytrees = Citytrees %>%
  mutate(SoilCode = as_factor(`Soil Code 1=sand and gravel 2= Clay 3=silt`)) %>%
  mutate(SoilCode = fct_recode(SoilCode,`Sand and Gravel` = "1", Clay = "2", Slit = "3"))
Citytrees$SoilCode



# Виды должны быть переименованы на латыне

# Transform all to latin 
# maple - Acer platanoides, 
# Oak - Quercus robur,
# Silver birch - Betula pendula, 
# Sycamore - Platanus occidentalis


Citytrees$Species
Citytrees$Species[Citytrees$Species == "Oak"] = "Quercus robur"
Citytrees$Species[Citytrees$Species == "Norway maple"] = "Acer platanoides"
Citytrees$Species[Citytrees$Species == "Norway Maple"] = "Acer platanoides"
Citytrees$Species[Citytrees$Species == "Silver Birch"] = "Betula pendula"
Citytrees$Species[Citytrees$Species == "Sycamore"] = "Platanus occidentalis"



# Должны быть созданы переменные координат(lat,lon) в британской системе координат(с учетом кодов квадратов) и в WGS84

library(stringr)

Citytrees$`Grid Reference`
coord = str_replace_all(Citytrees$`Grid Reference`,' ','')
coord_N = str_trunc(coord, 12, "right", ellipsis = "") %>% str_trunc(5,"left", ellipsis = "")
coord_E = str_trunc(coord, 7, "right", ellipsis = "") %>% str_trunc( 5, "left", ellipsis = "")
quadr = str_trunc(coord, 2, "right", ellipsis = "")
table_c = data.frame(as.integer(coord_E), as.integer(coord_N), quadr)

names(table_c)=c("E", "N", "quadr")
head(table_c)
table_c = na.exclude(table_c)

table_c = table_c %>% mutate("Easting_BC" = case_when(
  quadr == "TF" ~ E +600000,
  quadr == "TG" ~ E +700000,
  quadr == "TL" ~ E +600000,
))
table_c = table_c %>% mutate("Northing_BC" = case_when(
  quadr == "TF" ~ N +300000,
  quadr == "TG" ~ N +300000,
  quadr == "TL" ~ N +200000,
))

table_c = na.exclude(table_c)

table_WGS = 
  table_c %>%
  st_as_sf(coords = c("Easting_BC", "Northing_BC"), crs = 27700) %>%
  st_transform(4326) %>%
  st_coordinates() %>% as.data.frame()

table_WGS = data.frame(Lat = table_WGS$Y, Lon = table_WGS$X)
table_WGS %>% head

Citytrees$`Grid Reference`[1]
table_c[1,]
table_WGS[1,]

coord = cbind(table_c,table_WGS)
head(coord)
