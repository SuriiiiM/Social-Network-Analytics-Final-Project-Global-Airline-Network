library(data.table)
library(dplyr)
library(igraph)

library(writexl)
library(tibble)
library(readxl)


setwd("C:/Users/mengc/Desktop/Fall/Social Network Analytics/Final Project/2014")
airport2014 <- read_excel("airport.xlsx")
city2014 <- read_excel("city.xlsx")
country2014 <- read_excel("country.xlsx")
international2014 <- read_excel("international.xlsx")

airport_ah2014 <- read_excel("airport_ah.xlsx")
city_ah2014 <- read_excel("city_ah.xlsx")
country_ah2014 <- read_excel("country_ah.xlsx")
international_ah2014 <- read_excel("international_ah.xlsx")

setwd("C:/Users/mengc/Desktop/Fall/Social Network Analytics/Final Project/2019")
airport2019 <- read_excel("airport.xlsx")
city2019 <- read_excel("city.xlsx")
country2019 <- read_excel("country.xlsx")
international2019 <- read_excel("international.xlsx")

setwd("C:/Users/mengc/Desktop/Fall/Social Network Analytics/Final Project")
airportw <- merge(airport2014,airport_ah2014,by="Node") 
airportw <- merge(airportw,airport2019,by="Node",suffixes = c(".2014", ".2019"))

cityw <- merge(city2014,city_ah2014,by="Node") 
cityw <- merge(cityw,city2019,by="Node",suffixes = c(".2014", ".2019"))

countryw <- merge(country2014,country_ah2014,by="Node") 
countryw <- merge(countryw,country2019,by="Node",suffixes = c(".2014", ".2019"))

internationalw <- merge(international2014,international_ah2014,by="Node") 
internationalw <- merge(internationalw,international2019,by="Node",suffixes = c(".2014", ".2019"))

write_xlsx(airportw, path = "airportw.xlsx", col_names = TRUE)
write_xlsx(cityw, path = "cityw.xlsx", col_names = TRUE)
write_xlsx(countryw, path = "countryw.xlsx", col_names = TRUE)
write_xlsx(internationalw, path = "internationalw.xlsx", col_names = TRUE)


othdata2014 <- fread("reggdata.csv")[,c(1,3,5)]
othdata2019 <- fread("reggdata.csv")[,c(1,3,6)]
library(tidyr)
colnames(othdata2014) <- c("var","country","yr2014")
colnames(othdata2019) <- c("var","country","yr2018")
library(reshape2)
othdata2014$yr2014 <- as.numeric(othdata2014$yr2014)
othdata2019$yr2018 <- as.numeric(othdata2019$yr2018)

othdata2014 <- dcast(othdata2014, country ~ var,value.var="yr2014",fun=mean)
othdata2019 <- dcast(othdata2019, country ~ var,value.var="yr2018",fun=mean)
write_xlsx(countrywhole2014, path = "countrywhole2014.xlsx", col_names = TRUE)

othdataw <- merge(othdata2014,othdata2019,by="country",suffixes = c(".2014", ".2019"))
othdataw <- othdataw[,c(1,4:10,12:16,19:25,27:31)]

colnames(othdataw) <- c("country","2014-1","2014-2","2014-3","2014-4","2014-5","2014-6","2014-7","2014-8","2014-9",
                        "2014-10","2014-11","2014-12","2019-1","2019-2","2019-3","2019-4","2019-5","2019-6",
                        "2019-7","2019-8","2019-9","2019-10","2019-11","2019-12")
countrywhole <- merge(countryw,othdataw,by.x ="Node",by.y = "country")
write_xlsx(countrywhole, path = "countrywhole.xlsx", col_names = TRUE)

countrywhole2014 <- countrywhole[,c(1:9,16:27)]
countrywhole2019 <- countrywhole[,c(1,10:15,28:39)]
cor(countrywhole2014[,c(2:21)],use="complete.obs")
colnames(countrywhole2014)

summary(glm(strength.2014~.,data=countrywhole2014[,c(2,13,15:21)]))
summary(glm(coreness.2014~.,data=countrywhole2014[,c(3,13,15:21)]))
summary(glm(page.rank.2014~.,data=countrywhole2014[,c(4,13,15:21)]))
summary(glm(betweenness.2014~.,data=countrywhole2014[,c(5,13,15:21)]))
summary(glm(closeness.2014~.,data=countrywhole2014[,c(6,13,15:21)]))
summary(glm(eigen_centrality.2014~.,data=countrywhole2014[,c(7,13,15:21)]))
summary(glm(authority~.,data=countrywhole2014[,c(8,13,15:21)]))
summary(glm(hub_score~.,data=countrywhole2014[,c(9,13,15:21)]))



summary(glm(strength.2019~.,data=countrywhole2019[,c(2,13,15:17,19)]))
summary(glm(coreness.2019~.,data=countrywhole2019[,c(3,13,15:17,19)]))
summary(glm(page.rank.2019~.,data=countrywhole2019[,c(4,13,15:17,19)]))
summary(glm(betweenness.2019~.,data=countrywhole2019[,c(5,13,15:17,19)]))
summary(glm(closeness.2019~.,data=countrywhole2019[,c(6,13,15:17,19)]))
summary(glm(eigen_centrality.2019~.,data=countrywhole2019[,c(7,13,15:17,19)]))



