#실습2
#실습에 필요한 패키지 라이브러리에 등록
library(dplyr)
library(ggplot2)

#CSV형식의 파일을 불러와서 congestion 객체에 입력하고 구조확인
str(congestion)

#변수의 이상치와 결측치 확인하고 처리
summary(congestion)
#결측치 개수 확인
is.na(congestion)
sum(is.na(congestion))
colSums(is.na(congestion))
#결측치 처리
#6시 출발기차 결측치 제거
congestion1 <- congestion[!is.na(congestion$s0600),]
colSums(is.na(congestion1))
#23시30분 출발기차 결측치 제거
congestion1 <- congestion1[!is.na(congestion1$s2330),]
colSums(is.na(congestion1))
#남은 결측치 0으로 대체
congestion1[is.na(congestion1)] <- 0
colSums(is.na(congestion1))

#이상치 확인
ggplot(congestion1,aes(y=s0530))+
  geom_boxplot()
summary(congestion1$s0530)

#분석목적에 따른 파생변수 만들기
#1.지하철역 하루 평균 혼잡도
congestion1$day_mean <- rowMeans(congestion1[,c('s0530','s0600','s0630','s0700','s0730','s0800','s0830','s0900','s0930','s1000','s1030','s1100','s1130','s1200','s1230','s1300','s1330','s1400','s1430','s1500','s1530','s1600','s1630','s1700','s1730','s1800','s1830','s1900','s1930','s2000','s2030','s2100','s2130','s2200','s2230','s2300','s2330')])

congestion1%>%
  summarise(day_congestion_mean=mean(day_mean))

#1.1 지하철 호선별 하루 평균 혼잡도
line_pct<-congestion1%>%
  group_by(line)%>%
  summarise(total=sum(day_mean))%>%
  mutate(all=sum(total),pct=round(total/all*100,2))

line_pct

#2 지하철호선별 출근시간(07:00 ~ 09:00)대의 평균혼잡도
congestion2<-congestion1%>%
  group_by(line)%>%
  summarise(mean_s0700=mean(s0700),mean_s0730=mean(s0730),mean_s0800=mean(s0800),mean_s0830=mean(s0830),mean_s0900=mean(s0900))

congestion2

#기술통계분석
summary(congestion2)

# 평균혼잡도가 가장 높은 시간대를 막대그래프로 그리기
ggplot(congestion2, aes(x = factor(line), y = mean_s0800)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Line", y = "Mean_s0800") +
  ggtitle("평균혼잡도가 가장 높은 시간대 막대그래프")

#평균혼잡도 상위 4개 호선의 역별 기여도
linecongestion1<-congestion2%>%
  group_by(line) %>%
  summarise(total=sum(mean_s0700,mean_s0730,mean_s0800,mean_s0830,mean_s0900))

linecongestion1%>%
  arrange(desc(total))%>%
  head(4)

congetionstation1<-congestion1%>%
  filter(line%in%c(2,4,7,8))%>%
  mutate(total=s0700+s0730+s0800+s0830+s0900)%>%
  select(line,way,station,total)%>%
  unique()

stationcon1<-congetionstation1%>%
  mutate(all=sum(total),pct=round(total/all*100,2))

stationcon1%>%
  arrange(desc(pct))%>%
  head(10)

#3 8시의 지하철 혼잡도 범주화/범주별 빈도분석
congestion1%>%
  mutate(s80_grade=ifelse(s0800<=80,"good",ifelse(s0800<=130,"normal",ifelse(s0800<=150,"caution","bad"))))%>%
  group_by(s80_grade)%>%
  summarise(n=n())%>%
  mutate(total=sum(n),pct=round(n/total*100,1))%>%
  select(s80_grade,n,pct)%>%
  arrange(desc(n))

#3-1 8시 호선별로 지하철 혼잡도 범주화
congestion1%>%
  mutate(s80_grade=ifelse(s0800<=80,"good",ifelse(s0800<=130,"normal",ifelse(s0800<=150,"caution","bad"))))%>%
  group_by(line,s80_grade)%>%
  summarise(n=n())%>%
  mutate(total=sum(n),pct=round(n/total*100,1))%>%
  select(line,s80_grade,n,pct)%>%
  arrange(desc(pct))%>%
  head(5)

#3-2 출발시간 8시, 호선별 “caution”의 빈도와 caution이 전체 등급에서 차지하는 비율 계산
congestion1 %>%
  mutate(s80_grade = ifelse(s0800 <= 80, "good", ifelse(s0800 <= 130, "normal", ifelse(s0800 <= 150, "caution", "bad")))) %>%
  group_by(line, s80_grade) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n), pct = round(n / total * 100, 1)) %>%
  filter(s80_grade == "caution") %>%
  select(line, s80_grade, n, pct)

#4 지하철 호선별 퇴근시간 (18:00~20:00)대의 평균혼잡도
congestion3<-congestion1%>%
  group_by(line)%>%
  summarise(mean_s1800=mean(s1800),mean_s1830=mean(s1830),mean_s1900=mean(s1900),mean_s1930=mean(s1930),mean_s2000=mean(s2000))

congestion3

#기술통계분석
summary(congestion3)

#평균혼잡도가 가장 높은 시간대 막대그래프로 그리기
ggplot(congestion3, aes(x = factor(line), y = mean_s1800)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Line", y = "Mean_s1800") +
  ggtitle("평균혼잡도가 가장 높은 시간대 막대그래프")

#평균혼잡도 상위 4개 호선의 역별 기여도
linecongestion2<-congestion3%>%
  group_by(line) %>%
  summarise(total=sum(mean_s1800,mean_s1830,mean_s1900,mean_s1930,mean_s2000))

linecongestion2%>%
  arrange(desc(total))%>%
  head(4)

congetionstation2<-congestion1%>%
  filter(line%in%c(2,3,4,7))%>%
  mutate(total=s1800+s1830+s1900+s1930+s2000)%>%
  select(line,way,station,total)%>%
  unique()

stationcon2<-congetionstation2%>%
  mutate(all=sum(total),pct=round(total/all*100,2))

stationcon2%>%
  arrange(desc(pct))%>%
  head(10)

#5 출발시간 18시의 지하철 혼잡도 범주화/범주별 빈도분석
congestion1%>%
  mutate(s1800_grade=ifelse(s1800<=80,"good",ifelse(s1800<=130,"normal",ifelse(s1800<=150,"caution","bad"))))%>%
  group_by(s1800_grade)%>%
  summarise(n=n())%>%
  mutate(total=sum(n),pct=round(n/total*100,1))%>%
  select(s1800_grade,n,pct)%>%
  arrange(desc(n))

#18시 호선별로 지하철 혼잡도 범주화
congestion1%>%
  mutate(s1800_grade=ifelse(s1800<=80,"good",ifelse(s1800<=130,"normal",ifelse(s1800<=150,"caution","bad"))))%>%
  group_by(line,s1800_grade)%>%
  summarise(n=n())%>%
  mutate(total=sum(n),pct=round(n/total*100,1))%>%
  select(line,s1800_grade,n,pct)%>%
  arrange(desc(pct))%>%
  head(5)

# 출발시간 18시, 호선별 “bad”의 빈도와 badrk 전체 등급에서 차지하는 비율 계산
congestion1 %>%
  mutate(s1800_grade = ifelse(s1800 <= 80, "good", ifelse(s1800 <= 130, "normal", ifelse(s1800 <= 150, "caution", "bad")))) %>%
  group_by(line, s1800_grade) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n), pct = round(n / total * 100, 1)) %>%
  filter(s1800_grade == "bad") %>%
  select(line, s1800_grade, n, pct)
