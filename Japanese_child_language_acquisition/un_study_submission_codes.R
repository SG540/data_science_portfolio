## Codes by Tomoko Tatsumi & Giovanni Sala ----

# model codes for the study on children's Japanese un (February 7 2022)
library(ggplot2)
library(gamlss)

######## 1st model:un after questions  ###########
turnIC_analysis <- read.csv("https://raw.githubusercontent.com/SG540/data_science_portfolio/main/Japanese_child_language_acquisition/undataforanalysis_resub.csv")

#### GLM1: UN ~ age * role ####
turnIC_q <- subset(turnIC_analysis, !is.na(Qall)) 
turnIC_q$corpus <- as.factor(turnIC_q$corpus)
turnIC_q$role <- as.factor(turnIC_q$role)
xtabs(~UN,turnIC_q)

#full model
Qmodel = gamlss(UN ~ agemonths * role + re(random =~1 |corpus), family = BI(), data = turnIC_q)
summary(Qmodel)
# child model
Qcmodel = gamlss(UN ~ agemonths + re(random =~ 1|corpus), family = BI(), data = subset(turnIC_q, role == "Child"))
summary(Qcmodel)
# caregiver model
Qcgmodel = gamlss(UN ~ agemonths + re(random =~ 1|corpus), family = BI(), data = subset(turnIC_q, role == "Caregiver"))
summary(Qcgmodel)

## plot: UN ~ age + role
p = ggplot(data = subset(turnIC_q, role %in% c("Caregiver", "Child")), aes(y = UN, x = agemonths, color = role))
p = p + stat_smooth(method = "lm") 
p = p + scale_color_brewer(palette = "Dark2")
p = p + coord_cartesian(ylim = c(0, 1))
p = p + theme_bw() 
p = p + labs(x = "Child age (in months)", y = expression(paste("Probability of  ", italic("un"))), color = "Speaker")
p

#### GLM2: UN ~ age * speaker * question ####
turnIC_q$Qall <- as.factor(turnIC_q$Qall)
# full model
QQmodel = gamlss(UN ~ agemonths * role * Qall - agemonths : role : Qall +  re(random =~ 1|corpus), family = BI(), data = turnIC_q)
summary(QQmodel)
# child model
QQcmodel = gamlss(UN ~ agemonths * Qall + re(random =~ 1|corpus), family = BI(), data = subset(turnIC_q, role == "Child"))
summary(QQcmodel)
# caregiver model
QQcgmodel = gamlss(UN ~ agemonths * Qall +
                    re(random =~ 1|corpus), family = BI(), data = subset(turnIC_q, role == "Caregiver"))
summary(QQcgmodel)

## Plot: UN ~ agemonths + role + Qall
turnIC_q$Qall = ifelse(turnIC_q$Qall == 0, "Not following yes-no question", "Following yes-no question")
p = ggplot(data = turnIC_q, aes(y = UN, x = agemonths, colour = role)) 
p = p + stat_smooth(method = "lm") 
p = p + scale_color_brewer(palette = "Dark2")
p = p + coord_cartesian(ylim = c(0, 1))
p = p + theme_bw() + facet_wrap(~Qall)
p = p + labs(x = "Child age (in months)", y = expression(paste("Probability of  ", italic("un"))), colour = "Speaker")
p

#### GLM3: UN ~ age* role * cues for question ####
turnIC_q$KAFINAL <- as.factor(turnIC_q$KAFINAL)
turnIC_q$NEFINAL <- as.factor(turnIC_q$NEFINAL)
turnIC_q$CONNFINAL <- as.factor(turnIC_q$CONNFINAL)
turnIC_q$Qwh <-as.factor(turnIC_q$Qwh)
# full model 
QCmodel = gamlss(UN ~ agemonths * role * KAFINAL - agemonths : role : KAFINAL +
                   agemonths * role * Qwh - agemonths : role : Qwh +
                  re(random =~ 1|corpus), family = BI(), data = turnIC_q)
summary(QCmodel)
# child model 
QCcmodel = gamlss(UN ~ agemonths * KAFINAL + agemonths * Qwh + re(random =~ 1|corpus), family = BI(), data = subset(turnIC_q, role == "Child"))
summary(QCcmodel)
# caregiver model 
QCcgmodel = gamlss(UN ~ agemonths * KAFINAL + agemonths * Qwh + re(random =~ 1|corpus), family = BI(), data = subset(turnIC_q, role == "Caregiver"))
summary(QCcgmodel)

## Plot: UN ~ agemonths + role + cue type
turnIC_q$cuetype2 = ifelse(turnIC_q$cuetype %in% c("Wh words", "Final particle ka"), turnIC_q$cuetype, "none")
p = ggplot(data = subset(turnIC_q, cuetype2 != "none"), aes(y = UN, x = agemonths, colour = role)) 
p = p + stat_smooth(method = "lm") 
p = p + scale_color_brewer(palette = "Dark2")
p = p + coord_cartesian(ylim = c(0, 1))
p = p + theme_bw() + facet_wrap(~ cuetype2)
p = p + labs(x = "Child age (in months)", y = expression(paste("Probability of  ", italic("un"))), colour="Following turns with")
p

#### GLM4: UN ~ age* role * continuation cues (final particle ne and connectives) ####
# full model 
CCmodel = gamlss(UN ~ agemonths * role * NEFINAL - agemonths : role : NEFINAL +
                   agemonths * role * CONNFINAL - agemonths : role : CONNFINAL +
                   re(random =~ 1|corpus) , family = BI(), data = turnIC_q)
summary(CCmodel)
# child model 
CCcmodel = gamlss(UN ~ agemonths * NEFINAL + agemonths * CONNFINAL + re(random =~ 1|corpus), family = BI(), data = subset(turnIC_q, role == "Child"))
summary(CCcmodel)
# caregiver model 
CCcgmodel = gamlss(UN ~ agemonths * NEFINAL + agemonths * CONNFINAL + re(random =~ 1|corpus), family = BI(), data = subset(turnIC_q, role == "Caregiver"))
summary(CCcgmodel)

## Plot: UN ~ agemonths + role + cue type
turnIC_q $ cuetype3 = ifelse(turnIC_q$cuetype %in% c("Connective predicate","Final particle ne"), turnIC_q$cuetype, "none")
p = ggplot(data = subset(turnIC_q, cuetype3 != "none"), aes(y = UN, x = agemonths, colour = role)) 
p = p + stat_smooth(method = "lm") 
p = p + scale_color_brewer(palette = "Dark2")
p = p + coord_cartesian(ylim = c(0, 1))
p = p + theme_bw() + facet_wrap(~cuetype3)
p = p + labs(x = "Child age (in months)", y = expression(paste("Probability of  ", italic("un"))), colour = "Role")
p

#### additional analysis on the likeliness of continuation following the final particle ne ####
turnIC_q$suspendedNEFINAL = as.factor(turnIC_q$suspendedNEFINAL)
# full model
Smodel = gamlss(UN ~ suspendedNEFINAL * role + re(random =~ 1|corpus), family = BI(), data = turnIC_q)
summary(Smodel)
# child model 
Scmodel = gamlss(UN ~ suspendedNEFINAL + re(random =~ 1|corpus), family = BI(),  data = subset(turnIC_q, role == "Child"))
summary(Scmodel)
# caregiver model 
Scgmodel = gamlss(UN ~ suspendedNEFINAL + re(random =~ 1|corpus), family = BI(),  data = subset(turnIC_q, role == "Caregiver"))
summary(Scgmodel)

## Plot: UN ~ role + likeliness
turnIC_q$suspendedNEFINAL = ifelse(turnIC_q$suspendedNEFINAL == 1, "Likely", "Unlikely")
meandf = aggregate(UN ~ suspendedNEFINAL + role, turnIC_q, mean)
p = ggplot(data = meandf, aes(y = UN, x = role , fill = as.factor(suspendedNEFINAL)))
p = p + geom_bar(stat = "identity", position = "dodge")
p = p + coord_cartesian(ylim = c(0, 1))
p = p + labs(x = "Speaker type", y = expression(paste("Probability of  ", italic("un"))), fill = "Signal continuation")
p


