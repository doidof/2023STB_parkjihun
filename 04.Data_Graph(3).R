#Gender 1개의 인자로 만든 도수분포표
table(X2023_STB_survey$Gender)

#Gender 1개의 인자로 만든 상대도수분포표
ECN <- table(X2023_STB_survey$Gender)
prop.table(ECN)

#Gender와 Grade 2개인자로 만든 교차표
table(X2023_STB_survey$Gender, X2023_STB_survey$Grade)

#Nationality 인자 1개로 만든 막대그래프
barplot(table(X2023_STB_survey$Nationality))

#residental area 인자 1개로 만든 (가로)막대그래프
barplot(table(X2023_STB_survey$`residential area`),horiz=TRUE)

#Gender와 Grade 인자 2개로 만든 막대그래프
entry <- table(X2023_STB_survey$Gender,X2023_STB_survey$Grade)
barplot(entry, legend=TRUE)

#Grade 인자 1개로 만든 파이차트
pie(table(X2023_STB_survey$Grade))

#Age 인자 1개로 만든 히스토그램
hist(X2023_STB_survey$Age, main = "경통분2 나이분포",col=terrain.colors(12),freq = FALSE)
lines(density(X2023_STB_survey$'Age'),lwd=2)

#Grade별Age비교하는 박스플롯
boxplot(X2023_STB_survey$Grade,X2023_STB_survey$Age,main="학년별 나이",col="yellow",names=c("학년","나이"))
summary(X2023_STB_survey$Age,X2023_STB_survey$Grade,na.rm=T)
summary(X2023_STB_survey$Grade,X2023_STB_survey$Age,na.rm=T)
boxplot(X2023_STB_survey$Grade,X2023_STB_survey$Age,names=c("학년","나이"))

#Grade x값 Age y값으로 하는 산점도
plot(x=X2023_STB_survey$Grade,y=X2023_STB_survey$Age,xlab="학년",ylab="나이",main="학년별 나이")
