## R-31일차(2018.2.7)

#[문제218] JOB_ID 별로 월급의 차이가 존재하는지 crosstable로 분석하려고 합니다. 
#          월급 10000을 기준으로 JOB_ID 별로 각각 10000 이상인 사원과 이하인 사원들의 인원수가 출력하세요.

library(RJDBC) # java 이용해서 sql 접근(rJava)
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/r/ojdbc6.jar")
conn <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@localhost:1521/xe", "hr",  "hr")
emp <- dbGetQuery(conn, "select employee_id,job_id from employees where salary >= 10000")
emp

library(gmodels)
emp["SAL_YN"] <- ifelse(emp$SALARY >= 10000, "10000이상", "10000이하")
CrossTable(emp$JOB_ID, emp$SAL_YN)
CrossTable(emp$EMPLOYEE_ID,emp$SALARY)

j_id.1 <- emp[emp$SALARY >= 10000, "JOB_ID"]
j_id.2 <- emp[emp$SALARY < 10000, "JOB_ID"]

table(j_id.1)
table(j_id.2)

tapply(emp$EMPLOYEE_ID, list(emp$JOB_ID, emp[emp$SALARY >= 10000,]), length)
xtabs(~SALARY+JOB_ID,emp)

#ex) 자동차 소음

55.9 63.8 57.2 59.8 65.7 62.7 60.8 51.3 61.8 56.0 66.9 56.8 66.2 64.6 59.5 63.1 60.6 62.0 59.4 67.2 63.6 60.5 66.8 61.8 64.8 55.8 55.7 77.1 62.1 61.0 58.9 60.0 66.9 61.7 60.3 51.5 67.0 60.2 56.2 59.4 67.9 64.9 55.7 61.4 62.6 56.4 56.4 69.4 57.6 63.8

sort(carN)
min(carN)
sort(carN)[13] # length(carN)*.25 = 12.5
median(carN)
sort(carN)[38]  # length(carN)*.75 = 37.5
max(carN)

# fivenum() 만 실제 데이터에서 값을 가져옴 
quantile(carN)
summary(carN)
fivenum(carN)
boxplot(carN, ylim = c(45,80), horizontal = T)
abline(v = fivenum(carN)[4]+IQR(carN)*1.5, lty = 3)
abline(v = fivenum(carN)[2]-IQR(carN)*1.5, lty = 3)
ggplot(as.data.frame(carN),aes(x = NA, y = carN))+
  geom_boxplot()+
  geom_jitter()

IQR(carN) # 64.6-57.6


## 다음은 미국 캘리포니아 주에 거주하는 일본계 미국인 27,873명의 연령을 정리한 표이다. 
"
계급구간(세)    도수 
 0 이상 10 미만 2,568 
10 이상 20 미만 2,230 
20 이상 30 미만 6,355 
30 이상 40 미만 4,181 
40 이상 50 미만 3,651 
50 이상 60 미만 3,317 
60 이상 2,871 
합계 27,873
"

#(1) 각 계급의 상대도수를 계산하라. 
#(2) 히스토그램을 그리기 위하여 각 계급에 해당하는 막대의 높이를 계산하라. 
#(3) 히스토그램을 그려라.

df <- data.frame(계급 = c("[0,10)","[10,20)","[20,30)","[30,40)","[40,50)","[50,60)","[60,100)"),
                   도수 = c(2568,2230,6355,4181,3651,3317,2871), stringsAsFactors = F)
df

df <- rbind(df, data.frame(계급 = "합계", 도수 = sum(df$도수)))
df

df <- cbind(df, 상대도수 = df[,2]/df[nrow(df),2])
df$비율 <- c(round(prop.table(df[-nrow(df),2])*100),NA)
df

barplot(df$비율[-nrow(df)], names.arg = df$계급[-nrow(df)])
ggplot(data = df, aes(x = 0:60, y = df$도수))+
  geom_histogram(binwidth = 10, aes(fill=..count..))

hist(df$비율[-nrow(df)], breaks = seq(0,60,10))
sum(df$비율, na.rm = T)
rbinom(n = 5, size = 10, prob = 0.5)
pbinom(2, 20, .3)
dbinom(6, 20, .3)


## 어떤 바이러스 질환은 감염자와 접촉을 하게 되면 감염되며, 
#  건강한 사람이 감염자와 한 번 접 촉하였을 때 감염될 확률은 20%라고 한다. 
#  감염자가 임의의 건강한 사람 5명과 접촉했을 때, 5명 모두 감염될 확률은 얼마인가? 
#  그리고 감염자 수의 기대값과 분산, 표준편차는 얼마인가?
  
p = .2
n = 5

dbinom(5, 5, .2)
5*.2