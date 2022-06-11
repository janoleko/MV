###########################################################################
# Multivariate Verfahren 2 ------------------------------------------------
###########################################################################

getwd()
setwd("/Users/jan-ole/R/MV")
data = read.csv("datensatz.csv")
attach(data)
colnames(data)

# Aufgabe 1 ---------------------------------------------------------------

mod = lm(aggre_score ~ frust_score)
summary(mod)

predict(mod, newdata = data.frame(frust_score=5))

plot(frust_score, aggre_score, pch = 16)


# Aufgabe 2 ---------------------------------------------------------------

plot(mod)


# Aufgabe 3 ---------------------------------------------------------------

mod2 = lm(formula = aggre_score ~ frust_score * alter, data = data)

predict(mod2, newdata = data.frame(frust_score=5, alter=15))

summary(mod2)

0.163341 -0.046589*16

-0.161685 -0.046589*3


# Aufgabe 5 ---------------------------------------------------------------

library(lavaan)

help(package="lavaan")

model = "
frust =~ 1*frus1 + frus2 + frus3
aggr =~ 1*aggr1 + aggr2 + aggr3
# =~ heißt gemessen durch: latente Variable modellieren

# Intercepts fixieren
# 1 spricht den Intercept an --> 0* fixiert auf 0
frus1 ~ 0*1
aggr1 ~ 0*1

# Stattdessen Mittelwerte/ Intercepts der latenten variablen frei schätzen lassen
frust ~ mfrust*1
# mfrust ist der name des frei zu schätzenden Parameters

# Strukturmodell
aggr ~ b0*1 + b1*frust
# b0*1 sagt Intercept frei schätzen und b0 nennen
# b1*frust sagt anderen Parameter frei schätzen und b1 nennen.
"

?sem
mod3 = sem(model, data = data)
summary(mod3, fit.measures = TRUE) # damit RMSE angezeigt wird

# wenn . davor im Outuput --> Intercept (bei endogenen Variablen)
# wenn kein . --> Mittelwert (bei exogenen Variablen)

# wenn kein. dann Varianz
# wenn . Fehlervarianz


# Aufgabe 6 ---------------------------------------------------------------

















###########################################################################
# Poissonregression -------------------------------------------------------
###########################################################################

library(foreign)
library(MASS)

getwd()
setwd("/Users/jan-ole/R/MV")

d = read.spss("Muench_data.sav", to.data.frame = TRUE)

d$DrinkSUM_3mo_bl_AG_cent = scale(d$DrinkSUM_3mo_bl_AG, scale = FALSE)

# Poisson-GLM

m1 = glm(DrinkSUM_3mo_w12_AG ~ DrinkSUM_3mo_bl_AG_cent + Adaptive, data = d,
         family = poisson(link = "log"))
summary(m1)

exp(m1$coefficients[1])
exp(m1$coefficients[2]) # 3 % Zunahme in der AV, wenn die Baseline um 1 Getränk zunimmt
exp(m1$coefficients[3]) # durch die Intervention 20 % weniger Getränke


m2 = glm(DrinkSUM_3mo_w12_AG ~ DrinkSUM_3mo_bl_AG_cent + Adaptive, data = d,
         family = quasipoisson(link = "log"))
summary(m2)
# Anpassung für Überdispersion scheint hier sehr sinnvoll zu sein
# Beachte, keine Likelihood, deswegen kein AIC


# NB Regression
m3 = glm.nb(DrinkSUM_3mo_w12_AG ~ DrinkSUM_3mo_bl_AG_cent + Adaptive, data = d)
summary(m3)
# Punktschätzungen sind verändert

alpha = 1/8.8181


m4 = glm(DrinkSUM_3mo_w12_AG ~ conditio5 + DrinkSUM_3mo_bl_AG_cent, data = d,
         family = quasipoisson)
summary(m4)


exp(3.094207 + 0.025126*mean(d$DrinkSUM_3mo_bl_AG_cent))
exp(3.094207)

exp(3.094207 - 0.272383)

###########################################################################






# Logistische Regression --------------------------------------------------

getwd()
setwd("/Users/jan-ole/R/MV")

load("samstag.RData")

m1 = glm(ausgehen ~ extra + gestern, data = samstag, family = binomial)
summary(m1)

## Bedingte Wahrscheinlichkeiten

extra = 2
gestern = 0

eta = m1$coefficients[1] + m1$coefficients[2]*extra + m1$coefficients[3]*gestern

plogis(eta) # Wahrscheinlichkeit, dass so eine Person am Samstag Abend ausgeht

## Bedingte Wettquotienten

exp(m1$coefficients)
# oder
exp(coef(m1))

0.8219918 / (1 - 0.8219918) # odds (W-keit / Gegenw-keit)

4.617719 * 0.04797647 # für Personen die am Freitag schon weg waren und Extraversion 0 haben


## andere Linkfunktion

m2 = glm(ausgehen ~ extra + gestern, data = samstag, family = binomial(link = "probit"))
summary(m2)

m3 = glm(ausgehen ~ extra + gestern, data = samstag, family = binomial(link = "cloglog"))
summary(m3)

## keine Interpretierbarkeit der Koeffizienten


## predict-Funktion

predict(m1, newdata = data.frame(extra = 3, gestern = 0), type = "response")





###########################################################################
# Logistische Regression: Übungsaufgaben ----------------------------------
###########################################################################

getwd()
setwd("/Users/jan-ole/R/MV")
erstis = load("erstis2.RData")

head(erstis)
dim(erstis)

mod2 = glm(berlin2 ~ geschl + lz.1, data = erstis, family = binomial)
summary(mod2)

# Wahrscheinlichkeit einer Frau mit LZ 0 in Berlin zu wohnen
plogis(4.48385)

# Wahrscheinlichkeit eines Mannes mit LZ 25 in Berlin zu wohnen
plogis(coef(mod2)[1] + coef(mod2)[2] + coef(mod2)[3]*25)

# exp(beta1)
exp(coef(mod2)[2])

exp(coef(mod2))

predict(mod2, newdata = data.frame(geschl = "männlich", lz.1 = 25), type = "response")






###########################################################################
# Multinomiale und ordinale logistische Regression ------------------------
###########################################################################

getwd()
setwd("/Users/jan-ole/R/MV")

library(foreign)
library(nnet)


# Multinomiale logistische Regression -------------------------------------

data1 = read.dta("hsbdemo.dta")
head(data1)

# ses = socioeconomical status

m1 = multinom(prog ~ ses + write, data = data1)
summary(m1)

exp(coef(m1))

predict(m1, newdata = data.frame(ses = "high", write = 70), "probs")
predict(m1, newdata = data.frame(ses = "low", write = 40), "probs")



# Ordinale logistische Regression -----------------------------------------

data2 = read.dta("ologit.dta")
head(data2)

unique(data2$apply)

library(MASS)

m2 = polr(apply ~ pared + public + gpa, data = data2)
summary(m2)

# Parametrisierung mit bereits umgekehrten Vorzeichen

exp(coef(m2)) # odds-ratios

predict(m2, newdata = data.frame(pared = 0, public = 1, gpa = 3), "probs")





# Mulitnomiale und ordinale log. Regression: Übungsaufgaben --------------

getwd()
setwd("/Users/jan-ole/R/MV")

library(foreign)
erstis = read.spss("erstis.sav", to.data.frame = TRUE)
head(erstis)

library(MASS)

mod = polr(lz15 ~ geschl + alter + kinder, data = erstis)
summary(mod)

# erster Koeffizient negativ --> exp(-...) < 1 --> Kategorie unwahrscheinlicher
exp(mod$coefficients)

# i läuft von 1 bis 6

predict(mod, newdata = data.frame(geschl = "männlich", alter = 20, kinder = "ja"), "probs")

exp(-0.03489)





###########################################################################
# Modellgüte VL -----------------------------------------------------------
###########################################################################

library(foreign)
library(MASS)

d = read.spss("Muench_data.sav")
d$DrinkSUM_3mo_bl_AG_cent = scale(d$DrinkSUM_3mo_bl_AG, scale = FALSE) # Zentrierung

# einfaches Modell

m1 = glm(DrinkSUM_3mo_w12_AG ~ Adaptive, family = poisson, data = d)
summary(m1)

# Null deviance: Devianz des Modells nur mit Intercepts
# Residual deviance: Devianz des geschätzten Modells

m0 = glm(DrinkSUM_3mo_w12_AG ~ 1, family = poisson, data = d)
summary(m0)

# im Nullmodell stimmen die Null deviance und Residual deviance überein


# Modellgüte vergleichen
# Formel: pchisq(diff_deviance, diff_df, lower.tail = FALSE)

## Goodness-of-Fit Test
pchisq(709.98 - 678.17, 161-160, lower.tail = FALSE)
# lower tail = TRUE --> P(X <= x), FALSE --> P(X > x)
?pchisq

## Badness-of-Fit Test
pchisq(678.17, 160, lower.tail = FALSE)
# Modell ist deutlich schlechter als das saturierte Modell


# Vergleich genesteter Modelle:

m2 = glm(DrinkSUM_3mo_w12_AG ~ Adaptive + DrinkSUM_3mo_bl_AG_cent, family = poisson,
         data = d)
summary(m2)
# deutliche Reduktion der Devianz

anova(m1, m2)
pchisq(177.17, 1, lower.tail = FALSE)
# signifikant --> zweites Modell ist signifikant besser

anova(m0, m1, m2) # Funktion auch für mehr als 2 Modelle


# Modellvergleich bei nicht genesteten Modellen

m3 = glm.nb(DrinkSUM_3mo_w12_AG ~ Adaptive + DrinkSUM_3mo_bl_AG_cent,
            data = d) # für negativ-binomial gibt es Likelihood
summary(m3)

c(AIC(m3), AIC(m2))
# nb Regression hat kleineren AIC

m4 = glm(DrinkSUM_3mo_w12_AG ~ Adaptive + DrinkSUM_3mo_bl_AG_cent,
         data = d, family = quasipoisson)
summary(m4)
# es gibt kein AIC (da keine Likelihood) --> kein Vergleich nicht genesteter Modelle möglich



# Aufgaben Modellvergleich ------------------------------------------------

head(erstis)

mod = glm(berlin2 ~ geschl + lz.1, data = erstis, family = binomial)
summary(mod)

# Goodness of fit (Verlgleich mit Nullmodell)
pchisq(124.96 - 119.17, 161-159, lower.tail = FALSE)
# nicht signifikant zu einem Niveau von alpha = 0.05

# Badness of fit (Vergleich mit saturieretem Modell)
pchisq(119.17, 159, lower.tail = FALSE)
# gar nicht signifikant

library(dplyr)
erstis2 <- erstis %>% filter(!is.na(berlin2) & !is.na(geschl) & !is.na(lz.1) & !is.na(zuf.bel.1))
# NAs entfernen

# weiteres Modell mit anderem Prädiktor

mod2 = glm(berlin2 ~ zuf.bel.1, data = erstis2, family = binomial)
summary(mod2)

AIC(mod)
AIC(mod2)
BIC(mod)
BIC(mod2)



# Survival analysis -------------------------------------------------------

library(survival)

d = read.csv("marriage.csv")
head(d)
    
Surv(d$years, d$divorced)
# erstellt survival Variable --> "+" heißt rechtszensiert

m1 = survreg(Surv(years, divorced) ~ age + together, data = d, dist = "weibull")
summary(m1)

exp(coef(m1))
# multiplicative Effekte
# 5.6 Jahre erwartete Ehe bei Kovariaten Werten von 0
# pro zusätzliches Jahr + 3.79 %

5.589155*1.037888^20
# noch nicht zusammengewohnt und 20 bei der Hochzeit --> erwartet 11 Jahre bis zur Scheidung

predict(m1, newdata = data.frame(age = 20, together = "no"))

## Hazard ratios
mean(d$age)
exp(-coef(m1)[2] / m1$scale)
# hazard ratio: 0.954
# Mit jedem Jahr sinkt die hazard rate für eine Scheidung um ca. 5%

exp(-coef(m1)[2]*1 / m1$scale)

exp(-coef(m1)[2]*2 / m1$scale)

exp(-coef(m1)[2]*3 / m1$scale)

# harard rate vom mittelwert wird mit faktor 0.95 multipliziert
 


## Naive Modellierung mit Poisson Regression -> keine Berücksichtung der Zensierung

m2 = glm(years ~ age + together ,family = poisson, data = d)
summary(m2)
# völlig andere Koeffizienten --> viel geringere Erwartung, da wir ja bei Zensierten Daten annehmen, dass Ehe nach 9 Jahren endete
summary(m1)





# Hierarchische lineare Modelle -------------------------------------------

install.packages("AER")
library(AER)
data(STAR)

d = subset(STAR, select = c(read3, read1, gender, star1, schoolid1, school1))
d = na.omit(d) #hier alle missing values löschen (nicht generell machen)

### Einfache Multilevel Regression

with(d, tapply(read3, schoolid1, mean))
# Mittelwerte in den Schulen


### ALM
m1a = lm(read3 ~ schoolid1 - 1, data = d)
summary(m1a)
# 74 Gruppenmittelwerte


### HLM
library(lme4)
m1 = lmer(read3 ~ 1 + (1|schoolid1), data = d)
summary(m1)
# Unterteilung im Output in Random Effekts und fixed Effekts

coef(m1)
# etwas anders als die eben geschätzten Mittelwerte (anderer Schätzmechanismus) --> nicht ganz so genau bzgl. einzelnen Mittelwerten
# dafür genauer in der Schätzung der Varianz dieser

fixef(m1)
# extrahiert die fixed Effekts

ranef(m1)
# extrahiert die geschätzten Individuellen Abweichungen vom Gesamtmittelwert (u_c)

# Intraklassenkorrelation:
194.5 / (194.5 + 1289.0)
# 13 % der Varianz lassen sich durch die Schulzugehörigkeit aufklären


### Zweifache Multilevel Regression
m2 = lmer(read3 ~ 1 + read1 + (1|schoolid1), data = d)
summary(m2)
# mit fixed Slope
arm::display(m2) # nicht wissenschaftliche Schreibweise der Koeffizienten

coef(m2)
# Schulbedingten Regressionen: Für jede Schule Intercept und Slope (hier ist Slope noch in jeder Schule gleich)


### Bedingte lineare Multilevel Regression
m3 = lmer(read3 ~ 1 + gender + (1 + gender | schoolid1), data = d)
summary(m3)
