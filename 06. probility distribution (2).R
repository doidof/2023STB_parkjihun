#정규분포
#정규분포 수강생들의 평균키 175cm, 표준편차 10cm이라는 전제

#키 180cm이상일 확률
1 - pnorm(180,175,10)

#키가 170cm에서 175cm 사이일 확률
pnorm(175,175,10) - pnorm(170,175,10)

#키가 160cm 이하일 확률
pnorm(160,175,10)
