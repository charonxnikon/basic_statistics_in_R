library("multcomp") #glht
library("plotly")
library("fitdistrplus") #fitdist
library("sasLM") #Diffogram
library("emmeans")
library("ggplot2")
library("lavaan")
library("sjPlot")
library("sjmisc")

getwd() 

dir()

data <- read.csv('CARS.csv')

tail(data)

summary(data)

data_tmp <- data[c("MPG_Highway", "Type")]

head(data_tmp)

unique(data_tmp$Type)

print(shapiro.test(data_tmp[data_tmp$Type == 'SUV',]$MPG_Highway))
print(shapiro.test(data_tmp[data_tmp$Type == 'Sedan',]$MPG_Highway))
print(shapiro.test(data_tmp[data_tmp$Type == 'Sports',]$MPG_Highway))
print(shapiro.test(data_tmp[data_tmp$Type == 'Wagon',]$MPG_Highway))
print(shapiro.test(data_tmp[data_tmp$Type == 'Truck',]$MPG_Highway))
print(shapiro.test(data_tmp[data_tmp$Type == 'Hybrid',]$MPG_Highway))

print(kruskal.test(MPG_Highway ~ Type, data_tmp))

x <- 6
par(mar = c(4, x, 2, 1)) #Спасибо Хутороному Дмитрию, почему это работает не знаю,
#но так решаются траблы с обрезанием лэйблов на оси Oy

factor_tmp <- factor(data$Type)
aov_m <- aov(MPG_Highway ~ factor_tmp, data)
glht_test <- glht(aov_m, linfct = mcp(factor_tmp = "Tukey"))

summary(glht_test)
par(bg = 'grey')
par(mar = c(4, 9, 2, 1))
plot(confint(glht_test, level = 0.99))

summary(aov_m)

data$TruckSuv = data$Type
for (i in 1:length(data$TruckSuv))
{
    if ((data$TruckSuv[i] == 'SUV') || (data$TruckSuv[i] == 'Truck'))
    {
      data$TruckSuv[i] = paste('Truck_and_Suv')
    }
}

head(data)

unique(data$TruckSuv)

factor_tmp <- factor(data$TruckSuv)
aov_m <- aov(MPG_Highway ~ factor_tmp, data)
glht_test <- glht(aov_m, linfct = mcp(factor_tmp = "Tukey"))

summary(glht_test)
par(bg = 'grey')
par(mar = c(4, 11, 2, 1))
plot(confint(glht_test, level = 0.99))

summary(aov_m)

data_tmp <- data
for (i in 1:length(data_tmp$TruckSuv))
{
    if ((data_tmp$TruckSuv[i] == 'Sedan') || (data_tmp$TruckSuv[i] == 'Wagon'))
    {
      data_tmp$TruckSuv[i] = paste('Wagon_and_Sedan')
    }
}

head(data_tmp)

unique(data_tmp$TruckSuv)

factor_tmp <- factor(data_tmp$TruckSuv)
aov_m <- aov(MPG_Highway ~ factor_tmp, data_tmp)
glht_test <- glht(aov_m, linfct = mcp(factor_tmp = "Tukey"))

summary(glht_test)
par(bg = 'grey')
par(mar = c(4, 13, 2, 1))
plot(confint(glht_test, level = 0.99))

summary(aov_m)

data_tmp_2 <- data
for (i in 1:length(data_tmp_2$TruckSuv))
{
    if ((data_tmp_2$TruckSuv[i] == 'Sports') || (data_tmp_2$TruckSuv[i] == 'Wagon'))
    {
      data_tmp_2$TruckSuv[i] = paste('Wagon_and_Sports')
    }
}

head(data_tmp_2)

unique(data_tmp_2$TruckSuv)

factor_tmp <- factor(data_tmp_2$TruckSuv)
aov_m <- aov(MPG_Highway ~ factor_tmp, data_tmp_2)
glht_test <- glht(aov_m, linfct = mcp(factor_tmp = "Tukey"))

summary(glht_test)
par(bg = 'grey')
par(mar = c(4, 13, 2, 1))
plot(confint(glht_test, level = 0.99))

summary(aov_m)

data <- data_tmp

unique(data$TruckSuv)

model_1 <- lm(MPG_Highway ~ TruckSuv, data)
model_2 <- lm(MPG_Highway ~ TruckSuv + Origin, data)
model_3 <- lm(MPG_Highway ~ TruckSuv * Origin, data)

summary(model_1)
print('------------------------------------------')
summary(model_2)
print('------------------------------------------')
summary(model_3)

factor_type <- factor(data$TruckSuv)
factor_origin <- factor(data$Origin)

aov_m_1 <- aov(MPG_Highway ~ factor_type * factor_origin, data)
aov_m_2 <- aov(MPG_Highway ~ factor_type + factor_origin, data)

summary(aov_m_1)
print('------------------------------------------')
summary(aov_m_2)

par(bg = 'grey')
interaction.plot(data$Origin, data$TruckSuv, data$MPG_Highway)

par(bg = 'grey')
interaction.plot(data$TruckSuv, data$Origin, data$MPG_Highway)

#fit <- lm(MPG_Highway ~ TruckSuv * Origin, data)

#plot_model(fit, type = "pred",terms = c("TruckSuv", "Origin"))

par(mar = c(3, 4, 2, 1))
par(bg = 'grey')
Diffogram(MPG_Highway ~ factor_type + factor_origin, data, Term = "factor_type", conf.level = 0.99, adj = "tukey")
Diffogram(MPG_Highway ~ factor_type + factor_origin, data, Term = "factor_origin", conf.level = 0.99, adj = "tukey")

unique(data$Type)
unique(data$Origin)

US_tucks <- subset(data, (Type == 'Truck' & Origin == 'USA'))

head(US_tucks)

AEU_sedans <- subset(data, (Type == 'Sedan' & (Origin == 'Asia' | Origin == 'Europe')))

head(AEU_sedans)

AEU_sedans <- data[data$Origin %in% c('Europe', 'Asia') & data$Type == 'Sedan', 'MPG_Highway']
US_tucks <- data[data$Origin == 'USA' & data$Type == 'Truck', 'MPG_Highway']

aov_m <- aov(MPG_Highway ~ Origin + Type - 1, data)

summary(aov_m)

matrix_tmp <- matrix(c(0.5, 0.5, -1, 1, 0, 0, -1, 0), 1)

glht_test <- glht(aov_m, linfct = matrix_tmp)

confint(glht_test, level = 0.99)

residuals <- aov_m$residuals

fd <- fitdist(residuals, 'norm')
fd

ks.test(residuals, 'pnorm', mean = fd$estimate[1], sd = fd$estimate[2])

bartlett.test(list(AEU_sedans,US_tucks))

kruskal.test(list(AEU_sedans,US_tucks))


