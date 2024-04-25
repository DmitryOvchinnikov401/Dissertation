## Овчинников Дмитрий



###################################################################
## Если проблемы с R
# install.packages("installr")
# library(installr)
# updateR()

# remove.packages("pillar")
# install.packages("pillar", type = "binary")



## Список используемых пакетов
install.packages("readxl") #Для чтения Excel
install.packages("GGally") #Для матрицы корреляции
install.packages("car") #Для расчета VIF и тестирования гипотез с произвольными линейными ограничениями
install.packages("stargazer") #Для оформления таблиц с регрессиями
install.packages("clubSandwich") #Тест Вольда
install.packages("sandwich") #Для робастных стандартных ошибок


## Список используемых библиотек
library("readxl")
library("GGally") 
library("car") 
library("stargazer") 
library("clubSandwich") 
library("sandwich") 



###################################################################
## Загружаем и редактируем данные
OrigData <- read_excel("C:\\Users\\Дмитрий\\Desktop\\Risk_R_d.xlsx")
Data1 <- subset(OrigData, Date >= '2001-01-16') # Формируем выборку исходя из доступности всех данных

# Выбор директории
getwd()
setwd("C:\\Users\\Дмитрий\\Desktop")

## Выделяем периоды экономических шоков
Data21 <- subset(OrigData, Date >= '2000-03-27' & Date <= '2002-10-09') # Пузырь доткомов
Data22 <- subset(OrigData, Date >= '2007-10-31' & Date <= '2009-03-09') # Мировой экономический кризис
Data23 <- subset(OrigData, Date >= '2020-01-17' & Date <= '2020-04-03') # Пандемия коронавируса
Data24 <- subset(OrigData, Date >= '2022-01-04' & Date <= '2022-10-12') # СВО



## Считаем корреляции
# 1. Корреляционная матрица для доходностей акций и облигаций
Cor_data_1 <- Data1[,c(6:16, 20:21)]
cor(Cor_data_1, use = "complete.obs")
ggcorr(Cor_data_1, label = TRUE)


# 2. Корреляционная матрица для биржевых товаров
Cor_data_2 <- Data1[,c(25:36)]
cor(Cor_data_2, use = "complete.obs")
ggcorr(Cor_data_2, label = TRUE)


# 3. Корреляционная матрица для всего
Cor_data_3 <- Data1 [c(6:16, 20:21, 25:36)]
cor(Cor_data_3, use = "complete.obs")
ggcorr(Cor_data_3, label = TRUE, hjust = 0.75, size = 3)


## На основании VIF-значений убираем переменную Ind (сильно коррелирует с Mat, ConDis и Fin)


# Вводим робастные стандартные ошибки

# Функция для обычного МНК 
cse = function(reg) {
        rob = sqrt(diag(vcovHC(reg, type = "HC1")))
        return(rob)
}


###################################################################
## A. Регрессии - на всем периоде
# 1. Нефть (CL)
CLa = lm(data = Data1, CrudeOil ~ Ener + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

vif(CLa) # Проверяем VIF значения - только один раз
CL_Ha <- linearHypothesis(CLa, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(CLa))
CL_Ha <- CL_Ha[2,4]

# 2. Мазут
NYa = lm(data = Data1, HeatingOil ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

NY_Ha <- linearHypothesis(NYa, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(NYa))
NY_Ha <- NY_Ha[2,4]

# 3. Природный газ
NGa = lm(data = Data1, Gas ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

NG_Ha <- linearHypothesis(NGa, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(NGa))

NG_Ha <- NG_Ha[2,4]

# 4. Золото
GCa = lm(data = Data1, Gold ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

GC_Ha <- linearHypothesis(GCa, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(GCa))
GC_Ha <- GC_Ha[2,4]

# 5. Серебро
SIa = lm(data = Data1, Silver ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

SI_Ha <- linearHypothesis(SIa, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(SIa))
SI_Ha <- SI_Ha[2,4]

# 6. Платина
PLa = lm(data = Data1, Platinum ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

PL_Ha <- linearHypothesis(PLa, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(PLa))
PL_Ha <- PL_Ha[2,4]

# 7. Медь
HGa = lm(data = Data1, Copper ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

HG_Ha <- linearHypothesis(HGa, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(HGa))
HG_Ha <- HG_Ha[2,4]

# 8. Какао
CCa = lm(data = Data1, Cocoa ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

CC_Ha <- linearHypothesis(CCa, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(CCa))
CC_Ha <- CC_Ha[2,4]

# 9. Кофе
KCa = lm(data = Data1, Coffee ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

KC_Ha <- linearHypothesis(KCa, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(KCa))
KC_Ha <- KC_Ha[2,4]

# 10. Сахар
SBa = lm(data = Data1, Sugar ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

SB_Ha <- linearHypothesis(SBa, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(SBa))
SB_Ha <- SB_Ha[2,4]

# 11. Кукуруза
ZCa = lm(data = Data1, Corn ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

ZC_Ha <- linearHypothesis(ZCa, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(ZCa))
ZC_Ha <- ZC_Ha[2,4]

# 12. Пшеница
ZWa = lm(data = Data1, Wheat ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

ZW_Ha <- linearHypothesis(ZWa, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(ZWa))
ZW_Ha <- ZW_Ha[2,4]

# Агрегирование результатов
stargazer(CLa, NYa, NGa, GCa, SIa, PLa, HGa,
          CCa, KCa, SBa, ZCa, ZWa,
          se=list(cse(CLa),cse(NYa), cse(NGa), cse(GCa), cse(SIa), cse(PLa), cse(HGa),
                  cse(CCa),cse(KCa), cse(SBa), cse(ZCa), cse(ZWa)),
          title="Регрессии на всем исследуемом временном промежутке", type="text", 
          df=FALSE, digits=2, out = "All_Time.html", summary = FALSE)

hyp_1 <- c(CL_Ha, NY_Ha, NG_Ha, GC_Ha, SI_Ha, PL_Ha, HG_Ha,
           CC_Ha, KC_Ha, SB_Ha, ZC_Ha, ZW_Ha)
round (hyp_1, digits = 3)



###################################################################
## B. Регрессии - Пузырь доткомов
# 1. Нефть (CL)
CLb = lm(data = Data21, CrudeOil ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

CL_Hb <- linearHypothesis(CLb, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(CLb))
CL_Hb <- CL_Hb[2,4]

# 2. Мазут
NYb = lm(data = Data21, HeatingOil ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

NY_Hb <- linearHypothesis(NYb, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(NYb))
NY_Hb <- NY_Hb[2,4]

# 3. Природный газ
NGb = lm(data = Data21, Gas ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

NG_Hb <- linearHypothesis(NGb, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(NGb))
NG_Hb <- NG_Hb[2,4]

# 4. Золото
GCb = lm(data = Data21, Gold ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

GC_Hb <- linearHypothesis(GCb, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(GCb))
GC_Hb <- GC_Hb[2,4]

# 5. Серебро
SIb = lm(data = Data21, Silver ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

SI_Hb <- linearHypothesis(SIb, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(SIb))
SI_Hb <- SI_Hb[2,4]

# 6. Платина
PLb = lm(data = Data21, Platinum ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

PL_Hb <- linearHypothesis(PLb, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(PLb))
PL_Hb <- PL_Hb[2,4]

# 7. Медь
HGb = lm(data = Data21, Copper ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

HG_Hb <- linearHypothesis(HGb, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(HGb))
HG_Hb <- HG_Hb[2,4]

# 8. Какао
CCb = lm(data = Data21, Cocoa ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

CC_Hb <- linearHypothesis(CCb, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(CCb))
CC_Hb <- CC_Hb[2,4]

# 9. Кофе
KCb = lm(data = Data21, Coffee ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

KC_Hb <- linearHypothesis(KCb, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(KCb))
KC_Hb <- KC_Hb[2,4]

# 10. Сахар
SBb = lm(data = Data21, Sugar ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

SB_Hb <- linearHypothesis(SBb, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(SBb))
SB_Hb <- SB_Hb[2,4]

# 11. Кукуруза
ZCb = lm(data = Data21, Corn ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

ZC_Hb <- linearHypothesis(ZCb, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(ZCb))
ZC_Hb <- ZC_Hb[2,4]

# 12. Пшеница
ZWb = lm(data = Data21, Wheat ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

ZW_Hb <- linearHypothesis(ZWb, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(ZWb))
ZW_Hb <- ZW_Hb[2,4]

# Агрегирование результатов
stargazer(CLb, NYb, NGb, GCb, SIb, PLb, HGb,
          CCb, KCb, SBb, ZCb, ZWb,
          se=list(cse(CLb),cse(NYb), cse(NGb), cse(GCb), cse(SIb), cse(PLb), cse(HGb),
                  cse(CCb),cse(KCb), cse(SBb), cse(ZCb), cse(ZWb)),
          title="Регрессии во время Пузыря Доткомов", type="text", 
          df=FALSE, digits=2, out = "Dot_Com.html", summary = FALSE)

hyp_2 <- c(CL_Hb, NY_Hb, NG_Hb, GC_Hb, SI_Hb, PL_Hb, HG_Hb,
         CC_Hb, KC_Hb, SB_Hb, ZC_Hb, ZW_Hb)
round (hyp_2, digits = 3)


###################################################################
## C. Регрессии - Великая рецессия
# 1. Нефть (CL)
CLc = lm(data = Data22, CrudeOil ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

CL_Hc <- linearHypothesis(CLc, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(CLc))
CL_Hc <- CL_Hc[2,4]

# 2. Мазут
NYc = lm(data = Data22, HeatingOil ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

NY_Hc <- linearHypothesis(NYc, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(NYc))
NY_Hc <- NY_Hc[2,4]

# 3. Природный газ
NGc = lm(data = Data22, Gas ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

NG_Hc <- linearHypothesis(NGc, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(NGc))
NG_Hc <- NG_Hc[2,4]

# 4. Золото
GCc = lm(data = Data22, Gold ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

GC_Hc <- linearHypothesis(GCc, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(GCc))

GC_Hc <- GC_Hc[2,4]

# 5. Серебро
SIc = lm(data = Data22, Silver ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

SI_Hc <- linearHypothesis(SIc, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(SIc))
SI_Hc <- SI_Hc[2,4]

# 6. Платина
PLc = lm(data = Data22, Platinum ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

PL_Hc <- linearHypothesis(PLc, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(PLc))
PL_Hc <- PL_Hc[2,4]

# 7. Медь
HGc = lm(data = Data22, Copper ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

HG_Hc <- linearHypothesis(HGc, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(HGc))
HG_Hc <- HG_Hc[2,4]

# 8. Какао
CCc = lm(data = Data22, Cocoa ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

CC_Hc <- linearHypothesis(CCc, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(CCc))
CC_Hc <- CC_Hc[2,4]

# 9. Кофе
KCc = lm(data = Data22, Coffee ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

KC_Hc <- linearHypothesis(KCc, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(KCc))
KC_Hc <- KC_Hc[2,4]

# 10. Сахар
SBc = lm(data = Data22, Sugar ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

SB_Hc <- linearHypothesis(SBc, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(SBc))
SB_Hc <- SB_Hc[2,4]

# 11. Кукуруза
ZCc = lm(data = Data22, Corn ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

ZC_Hc <- linearHypothesis(ZCc, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(ZCc))
ZC_Hc <- ZC_Hc[2,4]

# 12. Пшеница
ZWc = lm(data = Data22, Wheat ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

ZW_Hc <- linearHypothesis(ZWc, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(ZWc))
ZW_Hc <- ZW_Hc[2,4]

# Агрегирование результатов
stargazer(CLc, NYc, NGc, GCc, SIc, PLc, HGc,
          CCc, KCc, SBc, ZCc, ZWc,
          se=list(cse(CLc),cse(NYc), cse(NGc), cse(GCc), cse(SIc), cse(PLc), cse(HGc),
                  cse(CCc),cse(KCc), cse(SBc), cse(ZCc), cse(ZWc)),
          title="Регрессии во время мирового экономического кризиса", type="text", 
          df=FALSE, digits=2, out = "Great_Rec.html", summary = FALSE)

hyp_3 <- c(CL_Hc, NY_Hc, NG_Hc, GC_Hc, SI_Hc, PL_Hc, HG_Hc,
         CC_Hc, KC_Hc, SB_Hc, ZC_Hc, ZW_Hc)
round (hyp_3, digits = 3)



###################################################################
## D. Регрессии - Ковид
# 1. Нефть (CL)
CLd = lm(data = Data23, CrudeOil ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

CL_Hd <- linearHypothesis(CLd, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(CLd))
CL_Hd <- CL_Hd[2,4]

# 2. Мазут
NYd = lm(data = Data23, HeatingOil ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

NY_Hd <- linearHypothesis(NYd, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(NYd))
NY_Hd <- NY_Hd[2,4]

# 3. Природный газ
NGd = lm(data = Data23, Gas ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

NG_Hd <- linearHypothesis(NGd, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(NGd))
NG_Hd <- NG_Hd[2,4]

# 4. Золото
GCd = lm(data = Data23, Gold ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

GC_Hd <- linearHypothesis(GCd, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(GCd))
GC_Hd <- GC_Hd[2,4]

# 5. Серебро
SId = lm(data = Data23, Silver ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

SI_Hd <- linearHypothesis(SId, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(SId))
SI_Hd <- SI_Hd[2,4]

# 6. Платина
PLd = lm(data = Data23, Platinum ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

PL_Hd <- linearHypothesis(PLd, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(PLd))
PL_Hd <- PL_Hd[2,4]

# 7. Медь
HGd = lm(data = Data23, Copper ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

HG_Hd <- linearHypothesis(HGd, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(HGd))
HG_Hd <- HG_Hd[2,4]

# 8. Какао
CCd = lm(data = Data23, Cocoa ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

CC_Hd <- linearHypothesis(CCd, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(CCd))
CC_Hd <- CC_Hd[2,4]

# 9. Кофе
KCd = lm(data = Data23, Coffee ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

KC_Hd <- linearHypothesis(KCd, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(KCd))
KC_Hd <- KC_Hd[2,4]

# 10. Сахар
SBd = lm(data = Data23, Sugar ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

SB_Hd <- linearHypothesis(SBd, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(SBd))
SB_Hd <- SB_Hd[2,4]

# 11. Кукуруза
ZCd = lm(data = Data23, Corn ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

ZC_Hd <- linearHypothesis(ZCd, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(ZCd))
ZC_Hd <- ZC_Hd[2,4]

# 12. Пшеница
ZWd = lm(data = Data23, Wheat ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

ZW_Hd <- linearHypothesis(ZWd, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(ZWd))
ZW_Hd <- ZW_Hd[2,4]

# Агрегирование результатов
stargazer(CLd, NYd, NGd, GCd, SId, PLd, HGd,
          CCd, KCd, SBd, ZCd, ZWd,
          se=list(cse(CLd),cse(NYd), cse(NGd), cse(GCd), cse(SId), cse(PLd), cse(HGd),
                  cse(CCd),cse(KCd), cse(SBd), cse(ZCd), cse(ZWd)),
          title="Регрессии во время пандемии коронавируса", type="text", 
          df=FALSE, digits=2, out = "Covid.html", summary = FALSE)

hyp_4 <- c(CL_Hd, NY_Hd, NG_Hd, GC_Hd, SI_Hd, PL_Hd, HG_Hd,
         CC_Hd, KC_Hd, SB_Hd, ZC_Hd, ZW_Hd)
round (hyp_4, digits = 3)



###################################################################
## E. Регрессии - СВО
# 1. Нефть (CL)
CLe = lm(data = Data24, CrudeOil ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

CL_He <- linearHypothesis(CLe, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(CLe))
CL_He <- CL_He[2,4]

# 2. Мазут
NYe = lm(data = Data24, HeatingOil ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

NY_He <- linearHypothesis(NYe, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(NYe))
NY_He <- NY_He[2,4]

# 3. Природный газ
NGe = lm(data = Data24, Gas ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

NG_He <- linearHypothesis(NGe, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(NGe))
NG_He <- NG_He[2,4]

# 4. Золото
GCe = lm(data = Data24, Gold ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

GC_He <- linearHypothesis(GCe, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(GCe))
GC_He <- GC_He[2,4]

# 5. Серебро
SIe = lm(data = Data24, Silver ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

SI_He <- linearHypothesis(SIe, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(SIe))
SI_He <- SI_He[2,4]

# 6. Платина
PLe = lm(data = Data24, Platinum ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

PL_He <- linearHypothesis(PLe, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(PLe))
PL_He <- PL_He[2,4]

# 7. Медь
HGe = lm(data = Data24, Copper ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

HG_He <- linearHypothesis(HGe, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(HGe))
HG_He <- HG_He[2,4]

# 8. Какао
CCe = lm(data = Data24, Cocoa ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

CC_He <- linearHypothesis(CCe, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(CCe))
CC_He <- CC_He[2,4]

# 9. Кофе
KCe = lm(data = Data24, Coffee ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

KC_He <- linearHypothesis(KCe, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(KCe))
KC_He <- KC_He[2,4]

# 10. Сахар
SBe = lm(data = Data24, Sugar ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

SB_He <- linearHypothesis(SBe, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(SBe))
SB_He <- SB_He[2,4]

# 11. Кукуруза
ZCe = lm(data = Data24, Corn ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

ZC_He <- linearHypothesis(ZCe, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(ZCe))
ZC_He <- ZC_He[2,4]

# 12. Пшеница
ZWe = lm(data = Data24, Wheat ~ Ener + Mat + ConDis + ConSt	+ Heal
        + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC)

ZW_He <- linearHypothesis(ZWe, c("(Intercept) = 0", "Ener + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs + BondT + BondC = 1"),
                         test = "F", vcov. = vcovHC(ZWe))
ZW_He <- ZW_He[2,4]

# Агрегирование результатов
stargazer(CLe, NYe, NGe, GCe, SIe, PLe, HGe,
          CCe, KCe, SBe, ZCe, ZWe,
          se=list(cse(CLe),cse(NYe), cse(NGe), cse(GCe), cse(SIe), cse(PLe), cse(HGe),
                  cse(CCe),cse(KCe), cse(SBe), cse(ZCe), cse(ZWe)),
          title="Регрессии во время специальной военной операции", type="text", 
          df=FALSE, digits=2, out = "SVO.html", summary = FALSE)

hyp_5 <- c(CL_He, NY_He, NG_He, GC_He, SI_He, PL_He, HG_He,
         CC_He, KC_He, SB_He, ZC_He, ZW_He)
round (hyp_5, digits = 3)


### Summary
## P-values
p_values <- cbind(hyp_1,hyp_2,hyp_3, hyp_4, hyp_5)
stargazer(p_values, title="P-value для гипотезы H0",
          column.labels = c("CrudeOil", "HeatingOil",  "Gas", "Gold",  "Silver",  "Platinum",
          "Copper",  "Cocoa",  "Coffee",  "Sugar",  "Corn",  "Wheat"),
          df=FALSE, digits.extra=2, type="text", out = "p_value.html", summary = FALSE)
          
## Coefficients
# Все время наблюдений
All_Time <- cbind(coef(CLa), coef(NYa),coef(NGa) ,coef(GCa) ,coef(SIa) ,coef(PLa) ,coef(HGa), 
             coef(CCa) ,coef(KCa) ,coef(SBa) ,coef(ZCa) ,coef(ZWa))
All_Time <- round(All_Time, digits = 3)
colnames(All_Time) <- c("CrudeOil", "HeatingOil",  "Gas", "Gold",  "Silver",  "Platinum",
                   "Copper",  "Cocoa",  "Coffee",  "Sugar",  "Corn",  "Wheat")
write.csv(All_Time, file = "C:\\Users\\Дмитрий\\Desktop\\All_Time.csv")


# Пузырь доткомов
Dot_Com <- cbind(coef(CLb), coef(NYb),coef(NGb) ,coef(GCb) ,coef(SIb) ,coef(PLb) ,coef(HGb), 
             coef(CCb) ,coef(KCb) ,coef(SBb) ,coef(ZCb) ,coef(ZWb))
Dot_Com <- round(Dot_Com, digits = 3)
colnames(Dot_Com) <- c("CrudeOil", "HeatingOil",  "Gas", "Gold",  "Silver",  "Platinum",
                   "Copper",  "Cocoa",  "Coffee",  "Sugar",  "Corn",  "Wheat")
write.csv(Dot_Com, file = "C:\\Users\\Дмитрий\\Desktop\\Dot_Com.csv")


# Кризис 2008
Great_Rec <- cbind(coef(CLc), coef(NYc),coef(NGc) ,coef(GCc) ,coef(SIc) ,coef(PLc) ,coef(HGc), 
                 coef(CCc) ,coef(KCc) ,coef(SBc) ,coef(ZCc) ,coef(ZWc))
Great_Rec <- round(Great_Rec, digits = 3)
colnames(Great_Rec) <- c("CrudeOil", "HeatingOil",  "Gas", "Gold",  "Silver",  "Platinum",
                       "Copper",  "Cocoa",  "Coffee",  "Sugar",  "Corn",  "Wheat")
write.csv(Great_Rec, file = "C:\\Users\\Дмитрий\\Desktop\\Great_Rec.csv")


# Ковид
Covid <- cbind(coef(CLd), coef(NYd),coef(NGd) ,coef(GCd) ,coef(SId) ,coef(PLd) ,coef(HGd), 
                   coef(CCd) ,coef(KCd) ,coef(SBd) ,coef(ZCd) ,coef(ZWd))
Covid <- round(Covid, digits = 3)
colnames(Covid) <- c("CrudeOil", "HeatingOil",  "Gas", "Gold",  "Silver",  "Platinum",
                         "Copper",  "Cocoa",  "Coffee",  "Sugar",  "Corn",  "Wheat")
write.csv(Covid, file = "C:\\Users\\Дмитрий\\Desktop\\Covid.csv")

# СВО
SVO <- cbind(coef(CLe), coef(NYe),coef(NGe) ,coef(GCe) ,coef(SIe) ,coef(PLe) ,coef(HGe), 
               coef(CCe) ,coef(KCe) ,coef(SBe) ,coef(ZCe) ,coef(ZWe))
SVO <- round(SVO, digits = 3)
colnames(SVO) <- c("CrudeOil", "HeatingOil",  "Gas", "Gold",  "Silver",  "Platinum",
                     "Copper",  "Cocoa",  "Coffee",  "Sugar",  "Corn",  "Wheat")
write.csv(SVO, file = "C:\\Users\\Дмитрий\\Desktop\\SVO.csv")



###################################################################
# Тестируем для энергетических commodities - нефти и мазута (убираем Ener + BondsT + BondsC)
## B. Регрессии - Пузырь доткомов
# 1. Нефть (CL)
CLb = lm(data = Data21, CrudeOil ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

CL_Hb <- linearHypothesis(CLb, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(CLb))
CL_Hb <- CL_Hb[2,4]

# 2. Мазут
NYb = lm(data = Data21, HeatingOil ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

NY_Hb <- linearHypothesis(NYb, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(NYb))
NY_Hb <- NY_Hb[2,4]

# 3. Природный газ
NGb = lm(data = Data21, Gas ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

NG_Hb <- linearHypothesis(NGb, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(NGb))
NG_Hb <- NG_Hb[2,4]

# 4. Золото
GCb = lm(data = Data21, Gold ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

GC_Hb <- linearHypothesis(GCb, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(GCb))
GC_Hb <- GC_Hb[2,4]

# 5. Серебро
SIb = lm(data = Data21, Silver ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

SI_Hb <- linearHypothesis(SIb, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(SIb))
SI_Hb <- SI_Hb[2,4]

# 6. Платина
PLb = lm(data = Data21, Platinum ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

PL_Hb <- linearHypothesis(PLb, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(PLb))
PL_Hb <- PL_Hb[2,4]

# 7. Медь
HGb = lm(data = Data21, Copper ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

HG_Hb <- linearHypothesis(HGb, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(HGb))
HG_Hb <- HG_Hb[2,4]

# 8. Какао
CCb = lm(data = Data21, Cocoa ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

CC_Hb <- linearHypothesis(CCb, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(CCb))
CC_Hb <- CC_Hb[2,4]

# 9. Кофе
KCb = lm(data = Data21, Coffee ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

KC_Hb <- linearHypothesis(KCb, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(KCb))
KC_Hb <- KC_Hb[2,4]

# 10. Сахар
SBb = lm(data = Data21, Sugar ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

SB_Hb <- linearHypothesis(SBb, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(SBb))
SB_Hb <- SB_Hb[2,4]

# 11. Кукуруза
ZCb = lm(data = Data21, Corn ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

ZC_Hb <- linearHypothesis(ZCb, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(ZCb))
ZC_Hb <- ZC_Hb[2,4]

# 12. Пшеница
ZWb = lm(data = Data21, Wheat ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

ZW_Hb <- linearHypothesis(ZWb, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(ZWb))
ZW_Hb <- ZW_Hb[2,4]

# Агрегирование результатов
stargazer(CLb, NYb, NGb, GCb, SIb, PLb, HGb,
          CCb, KCb, SBb, ZCb, ZWb,
          se=list(cse(CLb),cse(NYb), cse(NGb), cse(GCb), cse(SIb), cse(PLb), cse(HGb),
                  cse(CCb),cse(KCb), cse(SBb), cse(ZCb), cse(ZWb)),
          title="Регрессии во время Пузыря Доткомов", type="text", 
          df=FALSE, digits=2, out = "Dot_Com.html", summary = FALSE)

hyp_2 <- c(CL_Hb, NY_Hb, NG_Hb, GC_Hb, SI_Hb, PL_Hb, HG_Hb,
           CC_Hb, KC_Hb, SB_Hb, ZC_Hb, ZW_Hb)
round (hyp_2, digits = 3)



## C. Регрессии - Великая рецессия
# 1. Нефть (CL)
CLc = lm(data = Data22, CrudeOil ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

CL_Hc <- linearHypothesis(CLc, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(CLc))
CL_Hc <- CL_Hc[2,4]

# 2. Мазут
NYc = lm(data = Data22, HeatingOil ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

NY_Hc <- linearHypothesis(NYc, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(NYc))
NY_Hc <- NY_Hc[2,4]

# 3. Природный газ
NGc = lm(data = Data22, Gas ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

NG_Hc <- linearHypothesis(NGc, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(NGc))
NG_Hc <- NG_Hc[2,4]

# 4. Золото
GCc = lm(data = Data22, Gold ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

GC_Hc <- linearHypothesis(GCc, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(GCc))

GC_Hc <- GC_Hc[2,4]

# 5. Серебро
SIc = lm(data = Data22, Silver ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

SI_Hc <- linearHypothesis(SIc, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(SIc))
SI_Hc <- SI_Hc[2,4]

# 6. Платина
PLc = lm(data = Data22, Platinum ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

PL_Hc <- linearHypothesis(PLc, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(PLc))
PL_Hc <- PL_Hc[2,4]

# 7. Медь
HGc = lm(data = Data22, Copper ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

HG_Hc <- linearHypothesis(HGc, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(HGc))
HG_Hc <- HG_Hc[2,4]

# 8. Какао
CCc = lm(data = Data22, Cocoa ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

CC_Hc <- linearHypothesis(CCc, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(CCc))
CC_Hc <- CC_Hc[2,4]

# 9. Кофе
KCc = lm(data = Data22, Coffee ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

KC_Hc <- linearHypothesis(KCc, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(KCc))
KC_Hc <- KC_Hc[2,4]

# 10. Сахар
SBc = lm(data = Data22, Sugar ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

SB_Hc <- linearHypothesis(SBc, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(SBc))
SB_Hc <- SB_Hc[2,4]

# 11. Кукуруза
ZCc = lm(data = Data22, Corn ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

ZC_Hc <- linearHypothesis(ZCc, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(ZCc))
ZC_Hc <- ZC_Hc[2,4]

# 12. Пшеница
ZWc = lm(data = Data22, Wheat ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

ZW_Hc <- linearHypothesis(ZWc, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(ZWc))
ZW_Hc <- ZW_Hc[2,4]

# Агрегирование результатов
stargazer(CLc, NYc, NGc, GCc, SIc, PLc, HGc,
          CCc, KCc, SBc, ZCc, ZWc,
          se=list(cse(CLc),cse(NYc), cse(NGc), cse(GCc), cse(SIc), cse(PLc), cse(HGc),
                  cse(CCc),cse(KCc), cse(SBc), cse(ZCc), cse(ZWc)),
          title="Регрессии во время мирового экономического кризиса", type="text", 
          df=FALSE, digits=2, out = "Great_Rec.html", summary = FALSE)

hyp_3 <- c(CL_Hc, NY_Hc, NG_Hc, GC_Hc, SI_Hc, PL_Hc, HG_Hc,
           CC_Hc, KC_Hc, SB_Hc, ZC_Hc, ZW_Hc)
round (hyp_3, digits = 3)




## D. Регрессии - Ковид
# 1. Нефть (CL)
CLd = lm(data = Data23, CrudeOil ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

CL_Hd <- linearHypothesis(CLd, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(CLd))
CL_Hd <- CL_Hd[2,4]

# 2. Мазут
NYd = lm(data = Data23, HeatingOil ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

NY_Hd <- linearHypothesis(NYd, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(NYd))
NY_Hd <- NY_Hd[2,4]

# 3. Природный газ
NGd = lm(data = Data23, Gas ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

NG_Hd <- linearHypothesis(NGd, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(NGd))
NG_Hd <- NG_Hd[2,4]

# 4. Золото
GCd = lm(data = Data23, Gold ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

GC_Hd <- linearHypothesis(GCd, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(GCd))
GC_Hd <- GC_Hd[2,4]

# 5. Серебро
SId = lm(data = Data23, Silver ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

SI_Hd <- linearHypothesis(SId, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(SId))
SI_Hd <- SI_Hd[2,4]

# 6. Платина
PLd = lm(data = Data23, Platinum ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

PL_Hd <- linearHypothesis(PLd, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(PLd))
PL_Hd <- PL_Hd[2,4]

# 7. Медь
HGd = lm(data = Data23, Copper ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

HG_Hd <- linearHypothesis(HGd, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(HGd))
HG_Hd <- HG_Hd[2,4]

# 8. Какао
CCd = lm(data = Data23, Cocoa ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

CC_Hd <- linearHypothesis(CCd, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(CCd))
CC_Hd <- CC_Hd[2,4]

# 9. Кофе
KCd = lm(data = Data23, Coffee ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

KC_Hd <- linearHypothesis(KCd, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(KCd))
KC_Hd <- KC_Hd[2,4]

# 10. Сахар
SBd = lm(data = Data23, Sugar ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

SB_Hd <- linearHypothesis(SBd, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(SBd))
SB_Hd <- SB_Hd[2,4]

# 11. Кукуруза
ZCd = lm(data = Data23, Corn ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

ZC_Hd <- linearHypothesis(ZCd, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(ZCd))
ZC_Hd <- ZC_Hd[2,4]

# 12. Пшеница
ZWd = lm(data = Data23, Wheat ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

ZW_Hd <- linearHypothesis(ZWd, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(ZWd))
ZW_Hd <- ZW_Hd[2,4]

# Агрегирование результатов
stargazer(CLd, NYd, NGd, GCd, SId, PLd, HGd,
          CCd, KCd, SBd, ZCd, ZWd,
          se=list(cse(CLd),cse(NYd), cse(NGd), cse(GCd), cse(SId), cse(PLd), cse(HGd),
                  cse(CCd),cse(KCd), cse(SBd), cse(ZCd), cse(ZWd)),
          title="Регрессии во время пандемии коронавируса", type="text", 
          df=FALSE, digits=2, out = "Covid.html", summary = FALSE)

hyp_4 <- c(CL_Hd, NY_Hd, NG_Hd, GC_Hd, SI_Hd, PL_Hd, HG_Hd,
           CC_Hd, KC_Hd, SB_Hd, ZC_Hd, ZW_Hd)
round (hyp_4, digits = 3)




## E. Регрессии - СВО
# 1. Нефть (CL)
CLe = lm(data = Data24, CrudeOil ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

CL_He <- linearHypothesis(CLe, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(CLe))
CL_He <- CL_He[2,4]

# 2. Мазут
NYe = lm(data = Data24, HeatingOil ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

NY_He <- linearHypothesis(NYe, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(NYe))
NY_He <- NY_He[2,4]

# 3. Природный газ
NGe = lm(data = Data24, Gas ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

NG_He <- linearHypothesis(NGe, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(NGe))
NG_He <- NG_He[2,4]

# 4. Золото
GCe = lm(data = Data24, Gold ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

GC_He <- linearHypothesis(GCe, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(GCe))
GC_He <- GC_He[2,4]

# 5. Серебро
SIe = lm(data = Data24, Silver ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

SI_He <- linearHypothesis(SIe, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(SIe))
SI_He <- SI_He[2,4]

# 6. Платина
PLe = lm(data = Data24, Platinum ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

PL_He <- linearHypothesis(PLe, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(PLe))
PL_He <- PL_He[2,4]

# 7. Медь
HGe = lm(data = Data24, Copper ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

HG_He <- linearHypothesis(HGe, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(HGe))
HG_He <- HG_He[2,4]

# 8. Какао
CCe = lm(data = Data24, Cocoa ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

CC_He <- linearHypothesis(CCe, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(CCe))
CC_He <- CC_He[2,4]

# 9. Кофе
KCe = lm(data = Data24, Coffee ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

KC_He <- linearHypothesis(KCe, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(KCe))
KC_He <- KC_He[2,4]

# 10. Сахар
SBe = lm(data = Data24, Sugar ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

SB_He <- linearHypothesis(SBe, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(SBe))
SB_He <- SB_He[2,4]

# 11. Кукуруза
ZCe = lm(data = Data24, Corn ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

ZC_He <- linearHypothesis(ZCe, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(ZCe))
ZC_He <- ZC_He[2,4]

# 12. Пшеница
ZWe = lm(data = Data24, Wheat ~  + Mat + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

ZW_He <- linearHypothesis(ZWe, c("(Intercept) = 0", " + Mat + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(ZWe))
ZW_He <- ZW_He[2,4]

# Агрегирование результатов
stargazer(CLe, NYe, NGe, GCe, SIe, PLe, HGe,
          CCe, KCe, SBe, ZCe, ZWe,
          se=list(cse(CLe),cse(NYe), cse(NGe), cse(GCe), cse(SIe), cse(PLe), cse(HGe),
                  cse(CCe),cse(KCe), cse(SBe), cse(ZCe), cse(ZWe)),
          title="Регрессии во время специальной военной операции", type="text", 
          df=FALSE, digits=2, out = "SVO.html", summary = FALSE)

hyp_5 <- c(CL_He, NY_He, NG_He, GC_He, SI_He, PL_He, HG_He,
           CC_He, KC_He, SB_He, ZC_He, ZW_He)
round (hyp_5, digits = 3)


### Summary
## P-values
p_values <- cbind(hyp_1,hyp_2,hyp_3, hyp_4, hyp_5)
stargazer(p_values, title="P-value для гипотезы H0",
          column.labels = c("CrudeOil", "HeatingOil",  "Gas", "Gold",  "Silver",  "Platinum",
                            "Copper",  "Cocoa",  "Coffee",  "Sugar",  "Corn",  "Wheat"),
          df=FALSE, digits.extra=2, type="text", out = "p_value.html", summary = FALSE)












###################################################################
# Тестируем для золота и меди (убираем Mat + BondsT + BondsC)
## B. Регрессии - Пузырь доткомов
# 1. Нефть (CL)
CLb = lm(data = Data21, CrudeOil ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

CL_Hb <- linearHypothesis(CLb, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(CLb))
CL_Hb <- CL_Hb[2,4]

# 2. Мазут
NYb = lm(data = Data21, HeatingOil ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

NY_Hb <- linearHypothesis(NYb, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(NYb))
NY_Hb <- NY_Hb[2,4]

# 3. Природный газ
NGb = lm(data = Data21, Gas ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

NG_Hb <- linearHypothesis(NGb, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(NGb))
NG_Hb <- NG_Hb[2,4]

# 4. Золото
GCb = lm(data = Data21, Gold ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

GC_Hb <- linearHypothesis(GCb, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(GCb))
GC_Hb <- GC_Hb[2,4]

# 5. Серебро
SIb = lm(data = Data21, Silver ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

SI_Hb <- linearHypothesis(SIb, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(SIb))
SI_Hb <- SI_Hb[2,4]

# 6. Платина
PLb = lm(data = Data21, Platinum ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

PL_Hb <- linearHypothesis(PLb, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(PLb))
PL_Hb <- PL_Hb[2,4]

# 7. Медь
HGb = lm(data = Data21, Copper ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

HG_Hb <- linearHypothesis(HGb, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(HGb))
HG_Hb <- HG_Hb[2,4]

# 8. Какао
CCb = lm(data = Data21, Cocoa ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

CC_Hb <- linearHypothesis(CCb, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(CCb))
CC_Hb <- CC_Hb[2,4]

# 9. Кофе
KCb = lm(data = Data21, Coffee ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

KC_Hb <- linearHypothesis(KCb, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(KCb))
KC_Hb <- KC_Hb[2,4]

# 10. Сахар
SBb = lm(data = Data21, Sugar ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

SB_Hb <- linearHypothesis(SBb, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(SBb))
SB_Hb <- SB_Hb[2,4]

# 11. Кукуруза
ZCb = lm(data = Data21, Corn ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

ZC_Hb <- linearHypothesis(ZCb, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(ZCb))
ZC_Hb <- ZC_Hb[2,4]

# 12. Пшеница
ZWb = lm(data = Data21, Wheat ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

ZW_Hb <- linearHypothesis(ZWb, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(ZWb))
ZW_Hb <- ZW_Hb[2,4]

# Агрегирование результатов
stargazer(CLb, NYb, NGb, GCb, SIb, PLb, HGb,
          CCb, KCb, SBb, ZCb, ZWb,
          se=list(cse(CLb),cse(NYb), cse(NGb), cse(GCb), cse(SIb), cse(PLb), cse(HGb),
                  cse(CCb),cse(KCb), cse(SBb), cse(ZCb), cse(ZWb)),
          title="Регрессии во время Пузыря Доткомов", type="text", 
          df=FALSE, digits=2, out = "Dot_Com.html", summary = FALSE)

hyp_2 <- c(CL_Hb, NY_Hb, NG_Hb, GC_Hb, SI_Hb, PL_Hb, HG_Hb,
           CC_Hb, KC_Hb, SB_Hb, ZC_Hb, ZW_Hb)
round (hyp_2, digits = 3)



## C. Регрессии - Великая рецессия
# 1. Нефть (CL)
CLc = lm(data = Data22, CrudeOil ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC)

CL_Hc <- linearHypothesis(CLc, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC = 1"),
                          test = "F", vcov. = vcovHC(CLc))
CL_Hc <- CL_Hc[2,4]

# 2. Мазут
NYc = lm(data = Data22, HeatingOil ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC)

NY_Hc <- linearHypothesis(NYc, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC = 1"),
                          test = "F", vcov. = vcovHC(NYc))
NY_Hc <- NY_Hc[2,4]

# 3. Природный газ
NGc = lm(data = Data22, Gas ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC)

NG_Hc <- linearHypothesis(NGc, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC = 1"),
                          test = "F", vcov. = vcovHC(NGc))
NG_Hc <- NG_Hc[2,4]

# 4. Золото
GCc = lm(data = Data22, Gold ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC)

GC_Hc <- linearHypothesis(GCc, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC = 1"),
                          test = "F", vcov. = vcovHC(GCc))

GC_Hc <- GC_Hc[2,4]

# 5. Серебро
SIc = lm(data = Data22, Silver ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC)

SI_Hc <- linearHypothesis(SIc, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC = 1"),
                          test = "F", vcov. = vcovHC(SIc))
SI_Hc <- SI_Hc[2,4]

# 6. Платина
PLc = lm(data = Data22, Platinum ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC)

PL_Hc <- linearHypothesis(PLc, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC = 1"),
                          test = "F", vcov. = vcovHC(PLc))
PL_Hc <- PL_Hc[2,4]

# 7. Медь
HGc = lm(data = Data22, Copper ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC)

HG_Hc <- linearHypothesis(HGc, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC = 1"),
                          test = "F", vcov. = vcovHC(HGc))
HG_Hc <- HG_Hc[2,4]

# 8. Какао
CCc = lm(data = Data22, Cocoa ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC)

CC_Hc <- linearHypothesis(CCc, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC = 1"),
                          test = "F", vcov. = vcovHC(CCc))
CC_Hc <- CC_Hc[2,4]

# 9. Кофе
KCc = lm(data = Data22, Coffee ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC)

KC_Hc <- linearHypothesis(KCc, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC = 1"),
                          test = "F", vcov. = vcovHC(KCc))
KC_Hc <- KC_Hc[2,4]

# 10. Сахар
SBc = lm(data = Data22, Sugar ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC)

SB_Hc <- linearHypothesis(SBc, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC = 1"),
                          test = "F", vcov. = vcovHC(SBc))
SB_Hc <- SB_Hc[2,4]

# 11. Кукуруза
ZCc = lm(data = Data22, Corn ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC)

ZC_Hc <- linearHypothesis(ZCc, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC = 1"),
                          test = "F", vcov. = vcovHC(ZCc))
ZC_Hc <- ZC_Hc[2,4]

# 12. Пшеница
ZWc = lm(data = Data22, Wheat ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC)

ZW_Hc <- linearHypothesis(ZWc, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs  + BondC = 1"),
                          test = "F", vcov. = vcovHC(ZWc))
ZW_Hc <- ZW_Hc[2,4]

# Агрегирование результатов
stargazer(CLc, NYc, NGc, GCc, SIc, PLc, HGc,
          CCc, KCc, SBc, ZCc, ZWc,
          se=list(cse(CLc),cse(NYc), cse(NGc), cse(GCc), cse(SIc), cse(PLc), cse(HGc),
                  cse(CCc),cse(KCc), cse(SBc), cse(ZCc), cse(ZWc)),
          title="Регрессии во время мирового экономического кризиса", type="text", 
          df=FALSE, digits=2, out = "Great_Rec.html", summary = FALSE)

hyp_3 <- c(CL_Hc, NY_Hc, NG_Hc, GC_Hc, SI_Hc, PL_Hc, HG_Hc,
           CC_Hc, KC_Hc, SB_Hc, ZC_Hc, ZW_Hc)
round (hyp_3, digits = 3)



## D. Регрессии - Ковид
# 1. Нефть (CL)
CLd = lm(data = Data23, CrudeOil ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

CL_Hd <- linearHypothesis(CLd, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(CLd))
CL_Hd <- CL_Hd[2,4]

# 2. Мазут
NYd = lm(data = Data23, HeatingOil ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

NY_Hd <- linearHypothesis(NYd, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(NYd))
NY_Hd <- NY_Hd[2,4]

# 3. Природный газ
NGd = lm(data = Data23, Gas ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

NG_Hd <- linearHypothesis(NGd, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(NGd))
NG_Hd <- NG_Hd[2,4]

# 4. Золото
GCd = lm(data = Data23, Gold ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

GC_Hd <- linearHypothesis(GCd, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(GCd))
GC_Hd <- GC_Hd[2,4]

# 5. Серебро
SId = lm(data = Data23, Silver ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

SI_Hd <- linearHypothesis(SId, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(SId))
SI_Hd <- SI_Hd[2,4]

# 6. Платина
PLd = lm(data = Data23, Platinum ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

PL_Hd <- linearHypothesis(PLd, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(PLd))
PL_Hd <- PL_Hd[2,4]

# 7. Медь
HGd = lm(data = Data23, Copper ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

HG_Hd <- linearHypothesis(HGd, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(HGd))
HG_Hd <- HG_Hd[2,4]

# 8. Какао
CCd = lm(data = Data23, Cocoa ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

CC_Hd <- linearHypothesis(CCd, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(CCd))
CC_Hd <- CC_Hd[2,4]

# 9. Кофе
KCd = lm(data = Data23, Coffee ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

KC_Hd <- linearHypothesis(KCd, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(KCd))
KC_Hd <- KC_Hd[2,4]

# 10. Сахар
SBd = lm(data = Data23, Sugar ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

SB_Hd <- linearHypothesis(SBd, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(SBd))
SB_Hd <- SB_Hd[2,4]

# 11. Кукуруза
ZCd = lm(data = Data23, Corn ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

ZC_Hd <- linearHypothesis(ZCd, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(ZCd))
ZC_Hd <- ZC_Hd[2,4]

# 12. Пшеница
ZWd = lm(data = Data23, Wheat ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

ZW_Hd <- linearHypothesis(ZWd, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(ZWd))
ZW_Hd <- ZW_Hd[2,4]

# Агрегирование результатов
stargazer(CLd, NYd, NGd, GCd, SId, PLd, HGd,
          CCd, KCd, SBd, ZCd, ZWd,
          se=list(cse(CLd),cse(NYd), cse(NGd), cse(GCd), cse(SId), cse(PLd), cse(HGd),
                  cse(CCd),cse(KCd), cse(SBd), cse(ZCd), cse(ZWd)),
          title="Регрессии во время пандемии коронавируса", type="text", 
          df=FALSE, digits=2, out = "Covid.html", summary = FALSE)

hyp_4 <- c(CL_Hd, NY_Hd, NG_Hd, GC_Hd, SI_Hd, PL_Hd, HG_Hd,
           CC_Hd, KC_Hd, SB_Hd, ZC_Hd, ZW_Hd)
round (hyp_4, digits = 3)


## E. Регрессии - СВО
# 1. Нефть (CL)
CLe = lm(data = Data24, CrudeOil ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

CL_He <- linearHypothesis(CLe, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(CLe))
CL_He <- CL_He[2,4]

# 2. Мазут
NYe = lm(data = Data24, HeatingOil ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

NY_He <- linearHypothesis(NYe, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(NYe))
NY_He <- NY_He[2,4]

# 3. Природный газ
NGe = lm(data = Data24, Gas ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

NG_He <- linearHypothesis(NGe, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(NGe))
NG_He <- NG_He[2,4]

# 4. Золото
GCe = lm(data = Data24, Gold ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

GC_He <- linearHypothesis(GCe, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(GCe))
GC_He <- GC_He[2,4]

# 5. Серебро
SIe = lm(data = Data24, Silver ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

SI_He <- linearHypothesis(SIe, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(SIe))
SI_He <- SI_He[2,4]

# 6. Платина
PLe = lm(data = Data24, Platinum ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

PL_He <- linearHypothesis(PLe, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(PLe))
PL_He <- PL_He[2,4]

# 7. Медь
HGe = lm(data = Data24, Copper ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

HG_He <- linearHypothesis(HGe, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(HGe))
HG_He <- HG_He[2,4]

# 8. Какао
CCe = lm(data = Data24, Cocoa ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

CC_He <- linearHypothesis(CCe, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(CCe))
CC_He <- CC_He[2,4]

# 9. Кофе
KCe = lm(data = Data24, Coffee ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

KC_He <- linearHypothesis(KCe, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(KCe))
KC_He <- KC_He[2,4]

# 10. Сахар
SBe = lm(data = Data24, Sugar ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

SB_He <- linearHypothesis(SBe, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(SBe))
SB_He <- SB_He[2,4]

# 11. Кукуруза
ZCe = lm(data = Data24, Corn ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

ZC_He <- linearHypothesis(ZCe, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(ZCe))
ZC_He <- ZC_He[2,4]

# 12. Пшеница
ZWe = lm(data = Data24, Wheat ~ Ener  + ConDis + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

ZW_He <- linearHypothesis(ZWe, c("(Intercept) = 0", "Ener  + ConDis + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(ZWe))
ZW_He <- ZW_He[2,4]

# Агрегирование результатов
stargazer(CLe, NYe, NGe, GCe, SIe, PLe, HGe,
          CCe, KCe, SBe, ZCe, ZWe,
          se=list(cse(CLe),cse(NYe), cse(NGe), cse(GCe), cse(SIe), cse(PLe), cse(HGe),
                  cse(CCe),cse(KCe), cse(SBe), cse(ZCe), cse(ZWe)),
          title="Регрессии во время специальной военной операции", type="text", 
          df=FALSE, digits=2, out = "SVO.html", summary = FALSE)

hyp_5 <- c(CL_He, NY_He, NG_He, GC_He, SI_He, PL_He, HG_He,
           CC_He, KC_He, SB_He, ZC_He, ZW_He)
round (hyp_5, digits = 3)


### Summary
## P-values
p_values <- cbind(hyp_1,hyp_2,hyp_3, hyp_4, hyp_5)
stargazer(p_values, title="P-value для гипотезы H0",
          column.labels = c("CrudeOil", "HeatingOil",  "Gas", "Gold",  "Silver",  "Platinum",
                            "Copper",  "Cocoa",  "Coffee",  "Sugar",  "Corn",  "Wheat"),
          df=FALSE, digits.extra=2, type="text", out = "p_value.html", summary = FALSE)




###################################################################
# Тестируем для серебра (убираем Ener, Mat, ConDis, RealEs, BondT, BondC)
## B. Регрессии - Пузырь доткомов
# 1. Нефть (CL)
CLb = lm(data = Data21, CrudeOil ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

CL_Hb <- linearHypothesis(CLb, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(CLb))
CL_Hb <- CL_Hb[2,4]

# 2. Мазут
NYb = lm(data = Data21, HeatingOil ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

NY_Hb <- linearHypothesis(NYb, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(NYb))
NY_Hb <- NY_Hb[2,4]

# 3. Природный газ
NGb = lm(data = Data21, Gas ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

NG_Hb <- linearHypothesis(NGb, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(NGb))
NG_Hb <- NG_Hb[2,4]

# 4. Золото
GCb = lm(data = Data21, Gold ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

GC_Hb <- linearHypothesis(GCb, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(GCb))
GC_Hb <- GC_Hb[2,4]

# 5. Серебро
SIb = lm(data = Data21, Silver ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

SI_Hb <- linearHypothesis(SIb, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(SIb))
SI_Hb <- SI_Hb[2,4]

# 6. Платина
PLb = lm(data = Data21, Platinum ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

PL_Hb <- linearHypothesis(PLb, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(PLb))
PL_Hb <- PL_Hb[2,4]

# 7. Медь
HGb = lm(data = Data21, Copper ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

HG_Hb <- linearHypothesis(HGb, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(HGb))
HG_Hb <- HG_Hb[2,4]

# 8. Какао
CCb = lm(data = Data21, Cocoa ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

CC_Hb <- linearHypothesis(CCb, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(CCb))
CC_Hb <- CC_Hb[2,4]

# 9. Кофе
KCb = lm(data = Data21, Coffee ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

KC_Hb <- linearHypothesis(KCb, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(KCb))
KC_Hb <- KC_Hb[2,4]

# 10. Сахар
SBb = lm(data = Data21, Sugar ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

SB_Hb <- linearHypothesis(SBb, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(SBb))
SB_Hb <- SB_Hb[2,4]

# 11. Кукуруза
ZCb = lm(data = Data21, Corn ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

ZC_Hb <- linearHypothesis(ZCb, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(ZCb))
ZC_Hb <- ZC_Hb[2,4]

# 12. Пшеница
ZWb = lm(data = Data21, Wheat ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

ZW_Hb <- linearHypothesis(ZWb, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(ZWb))
ZW_Hb <- ZW_Hb[2,4]

# Агрегирование результатов
stargazer(CLb, NYb, NGb, GCb, SIb, PLb, HGb,
          CCb, KCb, SBb, ZCb, ZWb,
          se=list(cse(CLb),cse(NYb), cse(NGb), cse(GCb), cse(SIb), cse(PLb), cse(HGb),
                  cse(CCb),cse(KCb), cse(SBb), cse(ZCb), cse(ZWb)),
          title="Регрессии во время Пузыря Доткомов", type="text", 
          df=FALSE, digits=2, out = "Dot_Com.html", summary = FALSE)

hyp_2 <- c(CL_Hb, NY_Hb, NG_Hb, GC_Hb, SI_Hb, PL_Hb, HG_Hb,
           CC_Hb, KC_Hb, SB_Hb, ZC_Hb, ZW_Hb)
round (hyp_2, digits = 3)


## C. Регрессии - Великая рецессия
# 1. Нефть (CL)
CLc = lm(data = Data22, CrudeOil ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

CL_Hc <- linearHypothesis(CLc, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(CLc))
CL_Hc <- CL_Hc[2,4]

# 2. Мазут
NYc = lm(data = Data22, HeatingOil ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

NY_Hc <- linearHypothesis(NYc, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(NYc))
NY_Hc <- NY_Hc[2,4]

# 3. Природный газ
NGc = lm(data = Data22, Gas ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

NG_Hc <- linearHypothesis(NGc, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(NGc))
NG_Hc <- NG_Hc[2,4]

# 4. Золото
GCc = lm(data = Data22, Gold ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

GC_Hc <- linearHypothesis(GCc, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(GCc))

GC_Hc <- GC_Hc[2,4]

# 5. Серебро
SIc = lm(data = Data22, Silver ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

SI_Hc <- linearHypothesis(SIc, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(SIc))
SI_Hc <- SI_Hc[2,4]

# 6. Платина
PLc = lm(data = Data22, Platinum ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

PL_Hc <- linearHypothesis(PLc, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(PLc))
PL_Hc <- PL_Hc[2,4]

# 7. Медь
HGc = lm(data = Data22, Copper ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

HG_Hc <- linearHypothesis(HGc, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(HGc))
HG_Hc <- HG_Hc[2,4]

# 8. Какао
CCc = lm(data = Data22, Cocoa ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

CC_Hc <- linearHypothesis(CCc, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(CCc))
CC_Hc <- CC_Hc[2,4]

# 9. Кофе
KCc = lm(data = Data22, Coffee ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

KC_Hc <- linearHypothesis(KCc, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(KCc))
KC_Hc <- KC_Hc[2,4]

# 10. Сахар
SBc = lm(data = Data22, Sugar ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

SB_Hc <- linearHypothesis(SBc, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(SBc))
SB_Hc <- SB_Hc[2,4]

# 11. Кукуруза
ZCc = lm(data = Data22, Corn ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

ZC_Hc <- linearHypothesis(ZCc, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(ZCc))
ZC_Hc <- ZC_Hc[2,4]

# 12. Пшеница
ZWc = lm(data = Data22, Wheat ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

ZW_Hc <- linearHypothesis(ZWc, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(ZWc))
ZW_Hc <- ZW_Hc[2,4]

# Агрегирование результатов
stargazer(CLc, NYc, NGc, GCc, SIc, PLc, HGc,
          CCc, KCc, SBc, ZCc, ZWc,
          se=list(cse(CLc),cse(NYc), cse(NGc), cse(GCc), cse(SIc), cse(PLc), cse(HGc),
                  cse(CCc),cse(KCc), cse(SBc), cse(ZCc), cse(ZWc)),
          title="Регрессии во время мирового экономического кризиса", type="text", 
          df=FALSE, digits=2, out = "Great_Rec.html", summary = FALSE)

hyp_3 <- c(CL_Hc, NY_Hc, NG_Hc, GC_Hc, SI_Hc, PL_Hc, HG_Hc,
           CC_Hc, KC_Hc, SB_Hc, ZC_Hc, ZW_Hc)
round (hyp_3, digits = 3)


## D. Регрессии - Ковид
# 1. Нефть (CL)
CLd = lm(data = Data23, CrudeOil ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

CL_Hd <- linearHypothesis(CLd, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(CLd))
CL_Hd <- CL_Hd[2,4]

# 2. Мазут
NYd = lm(data = Data23, HeatingOil ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

NY_Hd <- linearHypothesis(NYd, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(NYd))
NY_Hd <- NY_Hd[2,4]

# 3. Природный газ
NGd = lm(data = Data23, Gas ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

NG_Hd <- linearHypothesis(NGd, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(NGd))
NG_Hd <- NG_Hd[2,4]

# 4. Золото
GCd = lm(data = Data23, Gold ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

GC_Hd <- linearHypothesis(GCd, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(GCd))
GC_Hd <- GC_Hd[2,4]

# 5. Серебро
SId = lm(data = Data23, Silver ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

SI_Hd <- linearHypothesis(SId, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(SId))
SI_Hd <- SI_Hd[2,4]

# 6. Платина
PLd = lm(data = Data23, Platinum ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

PL_Hd <- linearHypothesis(PLd, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(PLd))
PL_Hd <- PL_Hd[2,4]

# 7. Медь
HGd = lm(data = Data23, Copper ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

HG_Hd <- linearHypothesis(HGd, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(HGd))
HG_Hd <- HG_Hd[2,4]

# 8. Какао
CCd = lm(data = Data23, Cocoa ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

CC_Hd <- linearHypothesis(CCd, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(CCd))
CC_Hd <- CC_Hd[2,4]

# 9. Кофе
KCd = lm(data = Data23, Coffee ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

KC_Hd <- linearHypothesis(KCd, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(KCd))
KC_Hd <- KC_Hd[2,4]

# 10. Сахар
SBd = lm(data = Data23, Sugar ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

SB_Hd <- linearHypothesis(SBd, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(SBd))
SB_Hd <- SB_Hd[2,4]

# 11. Кукуруза
ZCd = lm(data = Data23, Corn ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

ZC_Hd <- linearHypothesis(ZCd, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(ZCd))
ZC_Hd <- ZC_Hd[2,4]

# 12. Пшеница
ZWd = lm(data = Data23, Wheat ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

ZW_Hd <- linearHypothesis(ZWd, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(ZWd))
ZW_Hd <- ZW_Hd[2,4]

# Агрегирование результатов
stargazer(CLd, NYd, NGd, GCd, SId, PLd, HGd,
          CCd, KCd, SBd, ZCd, ZWd,
          se=list(cse(CLd),cse(NYd), cse(NGd), cse(GCd), cse(SId), cse(PLd), cse(HGd),
                  cse(CCd),cse(KCd), cse(SBd), cse(ZCd), cse(ZWd)),
          title="Регрессии во время пандемии коронавируса", type="text", 
          df=FALSE, digits=2, out = "Covid.html", summary = FALSE)

hyp_4 <- c(CL_Hd, NY_Hd, NG_Hd, GC_Hd, SI_Hd, PL_Hd, HG_Hd,
           CC_Hd, KC_Hd, SB_Hd, ZC_Hd, ZW_Hd)
round (hyp_4, digits = 3)



## E. Регрессии - СВО
# 1. Нефть (CL)
CLe = lm(data = Data24, CrudeOil ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

CL_He <- linearHypothesis(CLe, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(CLe))
CL_He <- CL_He[2,4]

# 2. Мазут
NYe = lm(data = Data24, HeatingOil ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

NY_He <- linearHypothesis(NYe, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(NYe))
NY_He <- NY_He[2,4]

# 3. Природный газ
NGe = lm(data = Data24, Gas ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

NG_He <- linearHypothesis(NGe, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(NGe))
NG_He <- NG_He[2,4]

# 4. Золото
GCe = lm(data = Data24, Gold ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

GC_He <- linearHypothesis(GCe, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(GCe))
GC_He <- GC_He[2,4]

# 5. Серебро
SIe = lm(data = Data24, Silver ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

SI_He <- linearHypothesis(SIe, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(SIe))
SI_He <- SI_He[2,4]

# 6. Платина
PLe = lm(data = Data24, Platinum ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

PL_He <- linearHypothesis(PLe, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(PLe))
PL_He <- PL_He[2,4]

# 7. Медь
HGe = lm(data = Data24, Copper ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

HG_He <- linearHypothesis(HGe, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(HGe))
HG_He <- HG_He[2,4]

# 8. Какао
CCe = lm(data = Data24, Cocoa ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

CC_He <- linearHypothesis(CCe, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(CCe))
CC_He <- CC_He[2,4]

# 9. Кофе
KCe = lm(data = Data24, Coffee ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

KC_He <- linearHypothesis(KCe, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(KCe))
KC_He <- KC_He[2,4]

# 10. Сахар
SBe = lm(data = Data24, Sugar ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

SB_He <- linearHypothesis(SBe, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(SBe))
SB_He <- SB_He[2,4]

# 11. Кукуруза
ZCe = lm(data = Data24, Corn ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

ZC_He <- linearHypothesis(ZCe, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(ZCe))
ZC_He <- ZC_He[2,4]

# 12. Пшеница
ZWe = lm(data = Data24, Wheat ~    + ConSt	+ Heal
         + Fin + InfTec	+ TelServ	+ Util   )

ZW_He <- linearHypothesis(ZWe, c("(Intercept) = 0", "   + ConSt	+ Heal
             + Fin + InfTec	+ TelServ	+ Util    = 1"),
                          test = "F", vcov. = vcovHC(ZWe))
ZW_He <- ZW_He[2,4]

# Агрегирование результатов
stargazer(CLe, NYe, NGe, GCe, SIe, PLe, HGe,
          CCe, KCe, SBe, ZCe, ZWe,
          se=list(cse(CLe),cse(NYe), cse(NGe), cse(GCe), cse(SIe), cse(PLe), cse(HGe),
                  cse(CCe),cse(KCe), cse(SBe), cse(ZCe), cse(ZWe)),
          title="Регрессии во время специальной военной операции", type="text", 
          df=FALSE, digits=2, out = "SVO.html", summary = FALSE)

hyp_5 <- c(CL_He, NY_He, NG_He, GC_He, SI_He, PL_He, HG_He,
           CC_He, KC_He, SB_He, ZC_He, ZW_He)
round (hyp_5, digits = 3)


### Summary
## P-values
p_values <- cbind(hyp_1,hyp_2,hyp_3, hyp_4, hyp_5)
stargazer(p_values, title="P-value для гипотезы H0",
          column.labels = c("CrudeOil", "HeatingOil",  "Gas", "Gold",  "Silver",  "Platinum",
                            "Copper",  "Cocoa",  "Coffee",  "Sugar",  "Corn",  "Wheat"),
          df=FALSE, digits.extra=2, type="text", out = "p_value.html", summary = FALSE)






###################################################################
# Тестируем для продовольственных commodities (убираем ConDis + ConSt + BondsT + BondsC)

## B. Регрессии - Пузырь доткомов
# 1. Нефть (CL)
CLb = lm(data = Data21, CrudeOil ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

CL_Hb <- linearHypothesis(CLb, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(CLb))
CL_Hb <- CL_Hb[2,4]

# 2. Мазут
NYb = lm(data = Data21, HeatingOil ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

NY_Hb <- linearHypothesis(NYb, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(NYb))
NY_Hb <- NY_Hb[2,4]

# 3. Природный газ
NGb = lm(data = Data21, Gas ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

NG_Hb <- linearHypothesis(NGb, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(NGb))
NG_Hb <- NG_Hb[2,4]

# 4. Золото
GCb = lm(data = Data21, Gold ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

GC_Hb <- linearHypothesis(GCb, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(GCb))
GC_Hb <- GC_Hb[2,4]

# 5. Серебро
SIb = lm(data = Data21, Silver ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

SI_Hb <- linearHypothesis(SIb, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(SIb))
SI_Hb <- SI_Hb[2,4]

# 6. Платина
PLb = lm(data = Data21, Platinum ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

PL_Hb <- linearHypothesis(PLb, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(PLb))
PL_Hb <- PL_Hb[2,4]

# 7. Медь
HGb = lm(data = Data21, Copper ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

HG_Hb <- linearHypothesis(HGb, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(HGb))
HG_Hb <- HG_Hb[2,4]

# 8. Какао
CCb = lm(data = Data21, Cocoa ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

CC_Hb <- linearHypothesis(CCb, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(CCb))
CC_Hb <- CC_Hb[2,4]

# 9. Кофе
KCb = lm(data = Data21, Coffee ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

KC_Hb <- linearHypothesis(KCb, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(KCb))
KC_Hb <- KC_Hb[2,4]

# 10. Сахар
SBb = lm(data = Data21, Sugar ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

SB_Hb <- linearHypothesis(SBb, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(SBb))
SB_Hb <- SB_Hb[2,4]

# 11. Кукуруза
ZCb = lm(data = Data21, Corn ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

ZC_Hb <- linearHypothesis(ZCb, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(ZCb))
ZC_Hb <- ZC_Hb[2,4]

# 12. Пшеница
ZWb = lm(data = Data21, Wheat ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

ZW_Hb <- linearHypothesis(ZWb, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(ZWb))
ZW_Hb <- ZW_Hb[2,4]

# Агрегирование результатов
stargazer(CLb, NYb, NGb, GCb, SIb, PLb, HGb,
          CCb, KCb, SBb, ZCb, ZWb,
          se=list(cse(CLb),cse(NYb), cse(NGb), cse(GCb), cse(SIb), cse(PLb), cse(HGb),
                  cse(CCb),cse(KCb), cse(SBb), cse(ZCb), cse(ZWb)),
          title="Регрессии во время Пузыря Доткомов", type="text", 
          df=FALSE, digits=2, out = "Dot_Com.html", summary = FALSE)

hyp_2 <- c(CL_Hb, NY_Hb, NG_Hb, GC_Hb, SI_Hb, PL_Hb, HG_Hb,
           CC_Hb, KC_Hb, SB_Hb, ZC_Hb, ZW_Hb)
round (hyp_2, digits = 3)


## C. Регрессии - Великая рецессия
# 1. Нефть (CL)
CLc = lm(data = Data22, CrudeOil ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

CL_Hc <- linearHypothesis(CLc, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(CLc))
CL_Hc <- CL_Hc[2,4]

# 2. Мазут
NYc = lm(data = Data22, HeatingOil ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

NY_Hc <- linearHypothesis(NYc, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(NYc))
NY_Hc <- NY_Hc[2,4]

# 3. Природный газ
NGc = lm(data = Data22, Gas ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

NG_Hc <- linearHypothesis(NGc, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(NGc))
NG_Hc <- NG_Hc[2,4]

# 4. Золото
GCc = lm(data = Data22, Gold ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

GC_Hc <- linearHypothesis(GCc, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(GCc))

GC_Hc <- GC_Hc[2,4]

# 5. Серебро
SIc = lm(data = Data22, Silver ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

SI_Hc <- linearHypothesis(SIc, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(SIc))
SI_Hc <- SI_Hc[2,4]

# 6. Платина
PLc = lm(data = Data22, Platinum ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

PL_Hc <- linearHypothesis(PLc, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(PLc))
PL_Hc <- PL_Hc[2,4]

# 7. Медь
HGc = lm(data = Data22, Copper ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

HG_Hc <- linearHypothesis(HGc, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(HGc))
HG_Hc <- HG_Hc[2,4]

# 8. Какао
CCc = lm(data = Data22, Cocoa ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

CC_Hc <- linearHypothesis(CCc, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(CCc))
CC_Hc <- CC_Hc[2,4]

# 9. Кофе
KCc = lm(data = Data22, Coffee ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

KC_Hc <- linearHypothesis(KCc, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(KCc))
KC_Hc <- KC_Hc[2,4]

# 10. Сахар
SBc = lm(data = Data22, Sugar ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

SB_Hc <- linearHypothesis(SBc, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(SBc))
SB_Hc <- SB_Hc[2,4]

# 11. Кукуруза
ZCc = lm(data = Data22, Corn ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

ZC_Hc <- linearHypothesis(ZCc, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(ZCc))
ZC_Hc <- ZC_Hc[2,4]

# 12. Пшеница
ZWc = lm(data = Data22, Wheat ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

ZW_Hc <- linearHypothesis(ZWc, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(ZWc))
ZW_Hc <- ZW_Hc[2,4]

# Агрегирование результатов
stargazer(CLc, NYc, NGc, GCc, SIc, PLc, HGc,
          CCc, KCc, SBc, ZCc, ZWc,
          se=list(cse(CLc),cse(NYc), cse(NGc), cse(GCc), cse(SIc), cse(PLc), cse(HGc),
                  cse(CCc),cse(KCc), cse(SBc), cse(ZCc), cse(ZWc)),
          title="Регрессии во время мирового экономического кризиса", type="text", 
          df=FALSE, digits=2, out = "Great_Rec.html", summary = FALSE)

hyp_3 <- c(CL_Hc, NY_Hc, NG_Hc, GC_Hc, SI_Hc, PL_Hc, HG_Hc,
           CC_Hc, KC_Hc, SB_Hc, ZC_Hc, ZW_Hc)
round (hyp_3, digits = 3)



## D. Регрессии - Ковид
# 1. Нефть (CL)
CLd = lm(data = Data23, CrudeOil ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

CL_Hd <- linearHypothesis(CLd, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(CLd))
CL_Hd <- CL_Hd[2,4]

# 2. Мазут
NYd = lm(data = Data23, HeatingOil ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

NY_Hd <- linearHypothesis(NYd, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(NYd))
NY_Hd <- NY_Hd[2,4]

# 3. Природный газ
NGd = lm(data = Data23, Gas ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

NG_Hd <- linearHypothesis(NGd, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(NGd))
NG_Hd <- NG_Hd[2,4]

# 4. Золото
GCd = lm(data = Data23, Gold ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

GC_Hd <- linearHypothesis(GCd, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(GCd))
GC_Hd <- GC_Hd[2,4]

# 5. Серебро
SId = lm(data = Data23, Silver ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

SI_Hd <- linearHypothesis(SId, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(SId))
SI_Hd <- SI_Hd[2,4]

# 6. Платина
PLd = lm(data = Data23, Platinum ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

PL_Hd <- linearHypothesis(PLd, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(PLd))
PL_Hd <- PL_Hd[2,4]

# 7. Медь
HGd = lm(data = Data23, Copper ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

HG_Hd <- linearHypothesis(HGd, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(HGd))
HG_Hd <- HG_Hd[2,4]

# 8. Какао
CCd = lm(data = Data23, Cocoa ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

CC_Hd <- linearHypothesis(CCd, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(CCd))
CC_Hd <- CC_Hd[2,4]

# 9. Кофе
KCd = lm(data = Data23, Coffee ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

KC_Hd <- linearHypothesis(KCd, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(KCd))
KC_Hd <- KC_Hd[2,4]

# 10. Сахар
SBd = lm(data = Data23, Sugar ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

SB_Hd <- linearHypothesis(SBd, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(SBd))
SB_Hd <- SB_Hd[2,4]

# 11. Кукуруза
ZCd = lm(data = Data23, Corn ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

ZC_Hd <- linearHypothesis(ZCd, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(ZCd))
ZC_Hd <- ZC_Hd[2,4]

# 12. Пшеница
ZWd = lm(data = Data23, Wheat ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

ZW_Hd <- linearHypothesis(ZWd, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(ZWd))
ZW_Hd <- ZW_Hd[2,4]

# Агрегирование результатов
stargazer(CLd, NYd, NGd, GCd, SId, PLd, HGd,
          CCd, KCd, SBd, ZCd, ZWd,
          se=list(cse(CLd),cse(NYd), cse(NGd), cse(GCd), cse(SId), cse(PLd), cse(HGd),
                  cse(CCd),cse(KCd), cse(SBd), cse(ZCd), cse(ZWd)),
          title="Регрессии во время пандемии коронавируса", type="text", 
          df=FALSE, digits=2, out = "Covid.html", summary = FALSE)

hyp_4 <- c(CL_Hd, NY_Hd, NG_Hd, GC_Hd, SI_Hd, PL_Hd, HG_Hd,
           CC_Hd, KC_Hd, SB_Hd, ZC_Hd, ZW_Hd)
round (hyp_4, digits = 3)



## E. Регрессии - СВО
# 1. Нефть (CL)
CLe = lm(data = Data24, CrudeOil ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

CL_He <- linearHypothesis(CLe, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(CLe))
CL_He <- CL_He[2,4]

# 2. Мазут
NYe = lm(data = Data24, HeatingOil ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

NY_He <- linearHypothesis(NYe, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(NYe))
NY_He <- NY_He[2,4]

# 3. Природный газ
NGe = lm(data = Data24, Gas ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

NG_He <- linearHypothesis(NGe, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(NGe))
NG_He <- NG_He[2,4]

# 4. Золото
GCe = lm(data = Data24, Gold ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

GC_He <- linearHypothesis(GCe, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(GCe))
GC_He <- GC_He[2,4]

# 5. Серебро
SIe = lm(data = Data24, Silver ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

SI_He <- linearHypothesis(SIe, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(SIe))
SI_He <- SI_He[2,4]

# 6. Платина
PLe = lm(data = Data24, Platinum ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

PL_He <- linearHypothesis(PLe, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(PLe))
PL_He <- PL_He[2,4]

# 7. Медь
HGe = lm(data = Data24, Copper ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

HG_He <- linearHypothesis(HGe, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(HGe))
HG_He <- HG_He[2,4]

# 8. Какао
CCe = lm(data = Data24, Cocoa ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

CC_He <- linearHypothesis(CCe, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(CCe))
CC_He <- CC_He[2,4]

# 9. Кофе
KCe = lm(data = Data24, Coffee ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

KC_He <- linearHypothesis(KCe, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(KCe))
KC_He <- KC_He[2,4]

# 10. Сахар
SBe = lm(data = Data24, Sugar ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

SB_He <- linearHypothesis(SBe, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(SBe))
SB_He <- SB_He[2,4]

# 11. Кукуруза
ZCe = lm(data = Data24, Corn ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

ZC_He <- linearHypothesis(ZCe, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(ZCe))
ZC_He <- ZC_He[2,4]

# 12. Пшеница
ZWe = lm(data = Data24, Wheat ~ Ener + Mat  	+ Heal
         + Fin + InfTec	+ TelServ	+ Util + RealEs  )

ZW_He <- linearHypothesis(ZWe, c("(Intercept) = 0", "Ener + Mat  	+ Heal
             + Fin + InfTec	+ TelServ	+ Util + RealEs   = 1"),
                          test = "F", vcov. = vcovHC(ZWe))
ZW_He <- ZW_He[2,4]

# Агрегирование результатов
stargazer(CLe, NYe, NGe, GCe, SIe, PLe, HGe,
          CCe, KCe, SBe, ZCe, ZWe,
          se=list(cse(CLe),cse(NYe), cse(NGe), cse(GCe), cse(SIe), cse(PLe), cse(HGe),
                  cse(CCe),cse(KCe), cse(SBe), cse(ZCe), cse(ZWe)),
          title="Регрессии во время специальной военной операции", type="text", 
          df=FALSE, digits=2, out = "SVO.html", summary = FALSE)

hyp_5 <- c(CL_He, NY_He, NG_He, GC_He, SI_He, PL_He, HG_He,
           CC_He, KC_He, SB_He, ZC_He, ZW_He)
round (hyp_5, digits = 3)


### Summary
## P-values
p_values <- cbind(hyp_1,hyp_2,hyp_3, hyp_4, hyp_5)
stargazer(p_values, title="P-value для гипотезы H0",
          column.labels = c("CrudeOil", "HeatingOil",  "Gas", "Gold",  "Silver",  "Platinum",
                            "Copper",  "Cocoa",  "Coffee",  "Sugar",  "Corn",  "Wheat"),
          df=FALSE, digits.extra=2, type="text", out = "p_value.html", summary = FALSE)






















