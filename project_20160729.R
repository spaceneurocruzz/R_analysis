install.packages(c("readr", "xlsx", "dplyr", "ggplot2", "jsonlite", "devtools", "gridExtra"))
devtools::install_github("hrbrmstr/omdbapi")

install.packages("data.table")
install.packages("magrittr")
install.packages("dplyr")

getwd()
setwd("D:/R")

(myDefaultDirectory <- getwd())
myNewDirectory <- "D:/R"
setwd(myNewDirectory)

library("dplyr")
library("data.table")
library("ggplot2")

cancer <- read.csv("cancer.csv")
View(cancer)
#obesebyage <- read.csv("obesebyage.csv")
#obesebycountry <- read.csv("obesebycounty.csv")
obesebyyears <- read.csv("obesebyyears.csv")
View(obesebyyears)

head(cancer)
tail(cancer)
str(cancer)

(cancer <- tbl_df(cancer))
glimpse(cancer)

cancer <-cancer[,-5]
cancer <-cancer[,-5]
cancer <-cancer[,-6]

cancer <- rename(cancer, year=癌症診斷年)
cancer <- rename(cancer, gender=性別)
cancer <- rename(cancer, place=縣市別)
cancer <- rename(cancer, disease=癌症別)
cancer <- rename(cancer, age=平均年齡)
cancer <- rename(cancer, perten=粗率..每10萬人口.)

(myCSVPath <- paste(myNewDirectory, "cancerdata.csv", sep = "/"))
write.csv(cancer, file = myCSVPath, row.names = FALSE)


#diseases ranking
cancer %>%
  group_by(disease) %>%
  summarise(pertenMean= mean(perten))%>%
  arrange(pertenMean %>% desc())


select(year,gender,place,disease,age,perten) %>%
  filter(sarea == '??�' & hour == 8) %>%
  group_by(hour, sarea, sna) %>%
  summarise(avg = mean(avg.sbi) %>% round()) %>%
  arrange(avg %>% desc())

png(file = "topfive.png")
cancer %>%
  filter(cancer$disease %in% c( "女性乳房", "肝及肝內膽管","結直腸","肺、支氣管及氣管","子宮頸")) %>%
  group_by(disease) %>%
  summarise(pertenMean= mean(perten))%>%
  ggplot(aes(x = disease, y = pertenMean)) +
  labs(x = "癌症種類", y = "每十萬人口粗率平均值") +
  geom_bar(stat="identity", fill = "#2196F3", colour = "#616161")
  dev.off()
  
  
png(file = "topfiveFM.png")
cancer %>%
  filter(cancer$disease %in% c( "女性乳房", "肝及肝內膽管","結直腸","肺、支氣管及氣管","子宮頸")) %>%
  group_by(disease, gender) %>%
  summarise(pertenMean= mean(perten))%>%
  ggplot(aes(x = disease, y = pertenMean, fill = gender)) +
  labs(x = "癌症種類", y = "每十萬人口粗率平均值") +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#F44336", "#2196F3","aquamarine3" ))
dev.off()


#diseases ranking
cancer %>%
  group_by(year) %>%
  summarise(pertenyearMean= mean(perten))%>%
  arrange(pertenyearMean %>% desc())

png(file = "yeartrend.png")
cancer %>%
  filter(cancer$disease %in% c( "女性乳房", "肝及肝內膽管","結直腸","肺、支氣管及氣管","子宮頸")) %>%
  group_by(year, gender) %>%
  summarise(pertenyearMean= mean(perten))%>%
  ggplot(aes(x = year, y = pertenyearMean, colour = gender)) +
  labs(title="1979-2013年癌症發生率趨勢圖",x = "Year", y = "每十萬人口粗率平均值") +
  geom_line(stat="identity") 
dev.off()

#diseases ranking
cancer %>%
  filter(disease == '女性乳房') %>%
  group_by(year) %>%
  summarise(fbreastMean= mean(age))%>%
  arrange(fbreastMean %>% desc())

png(file = "yearbreastcancer.png")
cancer %>%
  filter(disease == '女性乳房') %>%
  group_by(year) %>%
  summarise(fbreastMean= mean(age))%>%
  ggplot(aes(x = year, y = fbreastMean)) +
  labs(title="女性乳癌年齡趨勢圖", x = "Year", y = "Age") +
  geom_line(stat="identity", colour = "red3") 
dev.off()

png(file = "2008-2013breastcancer.png")
cancer %>%
  filter(disease == '女性乳房'& cancer$year %in% c(2008,2009,2010,2011,2012,2013)) %>%
  group_by(year) %>%
  summarise(pertenyearMean= mean(perten))%>%
  ggplot(aes(x = year, y = pertenyearMean)) +
  labs(title="2008-2013年女性乳癌發生率趨勢圖", x = "Year", y = "每十萬人口粗率平均值") +
  geom_line(stat="identity", colour = "red3") 
dev.off()


png(file = "2008-2013obesity_F.png")
obesebyyears %>%
  group_by(byyear) %>%
  ggplot(aes(x = byyear, y = female)) +
  labs(title="2008-2013年女性肥胖比率趨勢圖", x = "Year", y = "占總樣本數") +
  geom_line(stat="identity", colour = "red3") 
dev.off()


png(file = "1979-2013diseasetrend.png")
cancer %>%
  filter(cancer$disease %in% c( "女性乳房", "肝及肝內膽管","結直腸","肺、支氣管及氣管","子宮頸")) %>%
  group_by(year, disease) %>%
  summarise(pertenyearMean= mean(perten))%>%
  ggplot(aes(x = year, y = pertenyearMean, colour = disease)) +
  labs(title="1979-2013年癌症發生率趨勢圖",x = "Year", y = "每十萬人口粗率平均值") +
  geom_line(stat="identity") 
dev.off()

png(file = "2008-2013diseasetrend.png")
cancer %>%
  filter(cancer$disease %in% c( "女性乳房", "肝及肝內膽管","結直腸","肺、支氣管及氣管","子宮頸") & cancer$year %in% c(2008,2009,2010,2011,2012,2013)) %>%
  group_by(year, disease) %>%
  summarise(pertenyearMean= mean(perten))%>%
  ggplot(aes(x = year, y = pertenyearMean, colour = disease)) +
  labs(title="2008-2013年癌症發生率趨勢圖",x = "Year", y = "每十萬人口粗率平均值") +
  geom_line(stat="identity") 
dev.off()

png(file = "1979-2013diseasetrend_F.png")
cancer %>%
  filter(cancer$gender=="女" & cancer$disease %in% c( "女性乳房", "肝及肝內膽管","結直腸","肺、支氣管及氣管","子宮頸")) %>%
  group_by(year, disease) %>%
  summarise(pertenyearMean= mean(perten))%>%
  ggplot(aes(x = year, y = pertenyearMean, colour = disease)) +
  labs(title="1979-2013年女性癌症發生率趨勢圖",x = "Year", y = "每十萬人口粗率平均值") +
  geom_line(stat="identity") 
dev.off()

png(file = "2008-2013diseasetrend_F.png")
cancer %>%
  filter(cancer$gender=="女" & cancer$disease %in% c( "女性乳房", "肝及肝內膽管","結直腸","肺、支氣管及氣管","子宮頸") & cancer$year %in% c(2008,2009,2010,2011,2012,2013)) %>%
  group_by(year, disease) %>%
  summarise(pertenyearMean= mean(perten))%>%
  ggplot(aes(x = year, y = pertenyearMean, colour = disease)) +
  labs(title="2008-2013年女性癌症發生率趨勢圖",x = "Year", y = "每十萬人口粗率平均值") +
  geom_line(stat="identity") 
dev.off()


png(file = "2013breastplace.png")
cancerCount <- cancer %>%
  filter(disease == '女性乳房' & year == 2013) %>%
  group_by(place) %>%
  summarise(count = n())
  pie(cancerCount$count, labels = cancerCount$place, main = "2013年各地區女性乳癌發生率")
dev.off()








