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

cancer <- rename(cancer, year=���g�E�_�~)
cancer <- rename(cancer, gender=�ʧO)
cancer <- rename(cancer, place=�����O)
cancer <- rename(cancer, disease=���g�O)
cancer <- rename(cancer, age=�����~��)
cancer <- rename(cancer, perten=�ʲv..�C10�U�H�f.)

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
  filter(cancer$disease %in% c( "�k�ʨũ�", "�x�Ψx���x��","�����z","�͡B���ޤή��","�l�c�V")) %>%
  group_by(disease) %>%
  summarise(pertenMean= mean(perten))%>%
  ggplot(aes(x = disease, y = pertenMean)) +
  labs(x = "���g����", y = "�C�Q�U�H�f�ʲv������") +
  geom_bar(stat="identity", fill = "#2196F3", colour = "#616161")
  dev.off()
  
  
png(file = "topfiveFM.png")
cancer %>%
  filter(cancer$disease %in% c( "�k�ʨũ�", "�x�Ψx���x��","�����z","�͡B���ޤή��","�l�c�V")) %>%
  group_by(disease, gender) %>%
  summarise(pertenMean= mean(perten))%>%
  ggplot(aes(x = disease, y = pertenMean, fill = gender)) +
  labs(x = "���g����", y = "�C�Q�U�H�f�ʲv������") +
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
  filter(cancer$disease %in% c( "�k�ʨũ�", "�x�Ψx���x��","�����z","�͡B���ޤή��","�l�c�V")) %>%
  group_by(year, gender) %>%
  summarise(pertenyearMean= mean(perten))%>%
  ggplot(aes(x = year, y = pertenyearMean, colour = gender)) +
  labs(title="1979-2013�~���g�o�Ͳv�Ͷչ�",x = "Year", y = "�C�Q�U�H�f�ʲv������") +
  geom_line(stat="identity") 
dev.off()

#diseases ranking
cancer %>%
  filter(disease == '�k�ʨũ�') %>%
  group_by(year) %>%
  summarise(fbreastMean= mean(age))%>%
  arrange(fbreastMean %>% desc())

png(file = "yearbreastcancer.png")
cancer %>%
  filter(disease == '�k�ʨũ�') %>%
  group_by(year) %>%
  summarise(fbreastMean= mean(age))%>%
  ggplot(aes(x = year, y = fbreastMean)) +
  labs(title="�k�ʨ����~���Ͷչ�", x = "Year", y = "Age") +
  geom_line(stat="identity", colour = "red3") 
dev.off()

png(file = "2008-2013breastcancer.png")
cancer %>%
  filter(disease == '�k�ʨũ�'& cancer$year %in% c(2008,2009,2010,2011,2012,2013)) %>%
  group_by(year) %>%
  summarise(pertenyearMean= mean(perten))%>%
  ggplot(aes(x = year, y = pertenyearMean)) +
  labs(title="2008-2013�~�k�ʨ����o�Ͳv�Ͷչ�", x = "Year", y = "�C�Q�U�H�f�ʲv������") +
  geom_line(stat="identity", colour = "red3") 
dev.off()


png(file = "2008-2013obesity_F.png")
obesebyyears %>%
  group_by(byyear) %>%
  ggplot(aes(x = byyear, y = female)) +
  labs(title="2008-2013�~�k�ʪέD��v�Ͷչ�", x = "Year", y = "�e�`�˥���") +
  geom_line(stat="identity", colour = "red3") 
dev.off()


png(file = "1979-2013diseasetrend.png")
cancer %>%
  filter(cancer$disease %in% c( "�k�ʨũ�", "�x�Ψx���x��","�����z","�͡B���ޤή��","�l�c�V")) %>%
  group_by(year, disease) %>%
  summarise(pertenyearMean= mean(perten))%>%
  ggplot(aes(x = year, y = pertenyearMean, colour = disease)) +
  labs(title="1979-2013�~���g�o�Ͳv�Ͷչ�",x = "Year", y = "�C�Q�U�H�f�ʲv������") +
  geom_line(stat="identity") 
dev.off()

png(file = "2008-2013diseasetrend.png")
cancer %>%
  filter(cancer$disease %in% c( "�k�ʨũ�", "�x�Ψx���x��","�����z","�͡B���ޤή��","�l�c�V") & cancer$year %in% c(2008,2009,2010,2011,2012,2013)) %>%
  group_by(year, disease) %>%
  summarise(pertenyearMean= mean(perten))%>%
  ggplot(aes(x = year, y = pertenyearMean, colour = disease)) +
  labs(title="2008-2013�~���g�o�Ͳv�Ͷչ�",x = "Year", y = "�C�Q�U�H�f�ʲv������") +
  geom_line(stat="identity") 
dev.off()

png(file = "1979-2013diseasetrend_F.png")
cancer %>%
  filter(cancer$gender=="�k" & cancer$disease %in% c( "�k�ʨũ�", "�x�Ψx���x��","�����z","�͡B���ޤή��","�l�c�V")) %>%
  group_by(year, disease) %>%
  summarise(pertenyearMean= mean(perten))%>%
  ggplot(aes(x = year, y = pertenyearMean, colour = disease)) +
  labs(title="1979-2013�~�k�����g�o�Ͳv�Ͷչ�",x = "Year", y = "�C�Q�U�H�f�ʲv������") +
  geom_line(stat="identity") 
dev.off()

png(file = "2008-2013diseasetrend_F.png")
cancer %>%
  filter(cancer$gender=="�k" & cancer$disease %in% c( "�k�ʨũ�", "�x�Ψx���x��","�����z","�͡B���ޤή��","�l�c�V") & cancer$year %in% c(2008,2009,2010,2011,2012,2013)) %>%
  group_by(year, disease) %>%
  summarise(pertenyearMean= mean(perten))%>%
  ggplot(aes(x = year, y = pertenyearMean, colour = disease)) +
  labs(title="2008-2013�~�k�����g�o�Ͳv�Ͷչ�",x = "Year", y = "�C�Q�U�H�f�ʲv������") +
  geom_line(stat="identity") 
dev.off()


png(file = "2013breastplace.png")
cancerCount <- cancer %>%
  filter(disease == '�k�ʨũ�' & year == 2013) %>%
  group_by(place) %>%
  summarise(count = n())
  pie(cancerCount$count, labels = cancerCount$place, main = "2013�~�U�a�Ϥk�ʨ����o�Ͳv")
dev.off()







