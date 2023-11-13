install.packages("heplots")
require(heplots)
install.packages("readxl")
library(readxl)
install.packages("agricolae")
library(agricolae)

Data0123<-read_excel("D:/GWSB/COURSES/23FALL/DNSC 6317 Praticum/Analysis R/connection of the numeric variables.xlsx",sheet="0123")

Data0123$State <-as.factor(Data0123$State)
Data0123$Region <-as.factor(Data0123$Region)
Data0123$Party <-as.factor(Data0123$Party)
summary(Data0123)

#One way_Region

ANOVA1<-lm(V1~Region,data=Data0123)
summary(ANOVA1)

ANOVA2<-lm(V2~Region,data=Data0123)
summary(ANOVA2)

ANOVA3<-lm(V3~Region,data=Data0123)
summary(ANOVA3)########################################significant
etasq(ANOVA3, anova=TRUE, partial=FALSE)
scheffe.test(ANOVA3,"Region", group=TRUE, console=TRUE, main="Scheffe Test")

ANOVA4<-lm(V4~Region,data=Data0123)
summary(ANOVA4)

ANOVA5<-lm(V5~Region,data=Data0123)
summary(ANOVA5)########################################significant
etasq(ANOVA5, anova=TRUE, partial=FALSE)
scheffe.test(ANOVA5,"Region", group=TRUE, console=TRUE, main="Scheffe Test")

ANOVA6<-lm(V6~Region,data=Data0123)
summary(ANOVA6)

ANOVA7<-lm(V7~Region,data=Data0123)
summary(ANOVA7)

#One way_Party

ANOVA1_1<-lm(V1~Party,data=Data0123)
summary(ANOVA1_1)

ANOVA2_1<-lm(V2~Party,data=Data0123)
summary(ANOVA2_1)

ANOVA3_1<-lm(V3~Party,data=Data0123)
summary(ANOVA3_1)

ANOVA4_1<-lm(V4~Party,data=Data0123)
summary(ANOVA4_1)

ANOVA5_1<-lm(V5~Party,data=Data0123)
summary(ANOVA5_1)

ANOVA6_1<-lm(V6~Party,data=Data0123)
summary(ANOVA6_1)

ANOVA7_1<-lm(V7~Party,data=Data0123)
summary(ANOVA7_1)

#Two way_Region+Party
ANOVA1_2<-lm(V1~Region+Party,data=Data0123)
summary(ANOVA1_2)

ANOVA2_2<-lm(V2~Region+Party,data=Data0123)
summary(ANOVA2_2)

ANOVA3_2<-lm(V3~Region+Party,data=Data0123)
summary(ANOVA3_2)
etasq(ANOVA3_2,data=Data123,anova=TRUE,partial=FALSE)

ANOVA4_2<-lm(V4~Region+Party,data=Data0123)
summary(ANOVA4_2)

ANOVA5_2<-lm(V5~Region+Party,data=Data0123)
summary(ANOVA5_2)
etasq(ANOVA3_2,data=Data123,anova=TRUE,partial=FALSE)

ANOVA6_2<-lm(V6~Region+Party,data=Data0123)
summary(ANOVA6_2)

ANOVA7_2<-lm(V7~Region+Party,data=Data0123)
summary(ANOVA7_2)
