## R-12일차(2018.1.11)

  # 12-1. stacked bar chart(스택형 바 차트)
  # 12-2. grouped bar chart(그룹형 바 차트)


#[문제150] 성별 현황을 조사 자료를 이용하여 성별 인구 비율을 원형 차트로 만드세요.

성별 현황

구분	조사수
남자	226965 여자	241319

sex <- c("남자", "여자")
res <- c(226965, 241319)
rad <- round(res * 100/sum(res))
rad
pie(res, labels = paste(sex, rad,"%"), main = "성별 현황",
    init.angle = 90, radius = 1,
    col = c("skyblue","green"), density = 40, border = "red")


#선생님 풀이
library(plotrix)
pie3D(rad, labels = paste(sex, rad,"%"), main = "성별 현황",
      radius = 1, explode = 0.1,
      col = c("skyblue","green"), density = 40, border = "red")
legend(x = 0.1, y = 1, legend = sex, cex = 0.6, fill = c("skyblue","green"))


#[문제151] 성별 현황을 조사 자료를 이용하여 성별 인구수를 막대그래프로 만드세요.

성별 현황

구분	조사수

남자	226965 여자	241319

bp <- barplot(height = res, names.arg = sex, 
              main = "성별 현황", 
              ylim = c(0,250000),
              ylab = "조사수",
              col = c("skyblue","green"), density = 40,
              cex.names = 2, font = 15, axes = FALSE, cex.lab = 2)  # axes : y축 보이는거 여부

text(x = bp, y = res, labels = paste(res,"명",sep = ''), pos = 1, col = 'purple', font = 15, cex = 2)


## 12-1. stacked bar chart(스택형 바 차트)
x1 <- c(2,6,9,5)
x2 <- c(8,10,15,6)
data <- rbind(x1,x2) ; data
name <- c("영업1팀", "영업2팀", "영업3팀", "영업4팀")
label <- c("2016년", "2017년")
barplot(data, 
        names.arg = name,
        main = "영업팀별 실적",
        xlab = "영업팀",
        ylab = "판배실적(억원)",
        ylim = c(0,30),
        legend.text = label,
        col = c("darkblue", "red"), density = 50, border = "gold")


## 12-2. grouped bar chart(그룹형 바 차트)
barplot(data,
        names.arg = name,
        main = "영업팀별 실적",
        beside = TRUE,            # 막대그래프를 옆에 배치
        xlab = "영업팀",
        ylab = "판매실적(억원)",
        ylim = c(0,20),
        legend.text = label,      # 범례
        args.legend = list(x = "topright", cex = 1, border = "green", box.col = "gold", text.col = "red"), 
        col = c("darkblue", "red"),
        desity = 40)


#[문제152] fruits_sales.csv file 읽어 들인 년도, 과일이름별 판매량을 그룹형 막대 그래프로 만드세요

fruits_sales
h <- tapply(fruits_sales$qty, list(fruits_sales$name, fruits_sales$year), sum)
h <- t(h)
class(h) ; h  # barplot matrix column 단위로 표시하는 
barplot(h,
        names.arg = colnames(h),
        main = "년도별 과일 판매량",
        legend.text = rownames(h),
        beside = TRUE,
        xlab = "과일종류",
        ylab = "판매량",
        ylim = c(0,20),
        col = c("red", "yellow", "darkgreen", "darkorange"),
        args.legend = list(box.col = "skyblue"),
        font = 4, font.lab = 15, col.lab = "blue", font.main = 11, col.main = "purple"
)

barplot(t(h),
        names.arg = rownames(h),
        main = "년도별 과일 판매량",
        beside = TRUE,
        xlab = "과일종류",
        ylab = "판매량",
        ylim = c(0,20),
        col = c("red", "yellow", "darkgreen", "darkorange"),
        legend.text = colnames(h),
        args.legend = list(x = 'topleft', box.col = "skyblue"),
        font = 4, font.lab = 15, col.lab = "blue", font.main = 11, col.main = "purple"
)


#[문제153] 부서별 인원수 막대그래프를 생성하세요. 단 부서없는 사원들의 인원수도 포함하세요.

t <- append(tapply(emp$EMPLOYEE_ID, emp$DEPARTMENT_ID, length), sum(is.na(emp$DEPARTMENT_ID)))
names(t)[length(t)] <- "NA"
str(t) ; t

barplot(t,
        ylim = c(0,50),
        col = rainbow(length(t)),
        density = 40,
        main = "부서별 인원수",
        xlab = "부서", ylab = "인원수(명)",
        las = 1)  # 0 : 기본, 1 : y축 , 2 : x축, 3 : x,y

#선생님 풀이(주의해야 함 형변환 일어남)
x <- aggregate(EMPLOYEE_ID ~ DEPARTMENT_ID, emp, length)
y <- c('부서없음', sum(is.na(emp$DEPARTMENT_ID)))
z <- rbind(x,y)
names(z) <- c("dept_id", "cn")

barplot(as.numeric(z$cn),      # as.numeric : 숫자형만 구형되기 때문에 
        names.arg = z$dept_id,
        main = "부서별 인원수",
        xlab = "부서",
        ylab = "인원수",
        ylim = c(0,50),
        col = rainbow(length(z$dept_id)),
        cex.names = 0.7,
        las = 0)
dept_cn <- aggregate(EMPLOYEE_ID ~ ifelse(is.na(DEPARTMENT_ID),'NA',DEPARTMENT_ID), emp, length)


#[문제154] 부서 이름별 급여 총액에 대해서 막대그래프를 생성하세요.단 부서없는 사원들의 인원수도 포함하세요.

library(plyr)

#sol.1 : ddply & merge
ex_154 <- ddply(emp, 'DEPARTMENT_ID', summarise, sum_sal = sum(SALARY))
ex_154 <- merge(ex_154, dept[,c("DEPARTMENT_ID", "DEPARTMENT_NAME")], all.x = T)
bp_154 <- barplot(ex_154$sum_sal,
                  names.arg = ex_154$DEPARTMENT_NAME,
                  ylim = c(0,350000),
                  main = "부서별 급여총액", 
                  xlab = "부서",
                  ylab = "급여합(원)",
                  col = cm.colors(nrow(ex_154)),
                  cex.names = 0.7, 
                  las = 3)

text(x = bp_154, y = ex_154$sum_sal, labels = ex_154$sum_sal, pos = 1, cex = .5)

text(x = bp_154[is.na(ex_154$DEPARTMENT_ID)], 
     y = ex_154$sum_sal[is.na(ex_154$DEPARTMENT_ID)], 
     labels = "부서없음", pos = 3, cex = .7, col = "red")

text(x = bp_154[ex_154$sum_sal == max(ex_154$sum_sal)],
     y = max(ex_154$sum_sal), 
     labels = "최고급여", pos = 3, cex = .7, col = "red")

#sol.2 : dplyr & merge
oth_sol <- emp%>%
  group_by(DEPARTMENT_ID)%>%
  summarise_at("SALARY", sum, na.rm = T)
oth_sol <- merge(oth_sol, dept[,c("DEPARTMENT_ID", "DEPARTMENT_NAME")], all.x = T)
oth_bp <- barplot(oth_sol$SALARY,
                  names.arg = oth_sol$DEPARTMENT_NAME,
                  col = rainbow(nrow(oth_sol)),
                  ylim = c(0,350000),
                  cex.names = 0.7, 
                  las = 2)

text(x = oth_bp, y = oth_sol$SALARY,
     labels = oth_sol$SALARY, pos = 1, cex = 0.7)

text(x = oth_bp[is.na(oth_sol$DEPARTMENT_NAME)], 
     y = oth_sol$SALARY[is.na(oth_sol$DEPARTMENT_NAME)],
     labels = "부서없음", pos = 3, cex = 0.7, col = "red")

text(x = oth_bp[oth_sol$SALARY == max(oth_sol$SALARY)],
     y = max(oth_sol$SALARY),
     labels = "최고급여", pos = 3, cex = 0.7, col = "red")

#선생님 풀이
library(sqldf)
dept_sal <- sqldf('SELECT department_name dept_name, sum(salary) sum_sal
                  FROM emp e LEFT OUTER JOIN dept d 
                  ON e.department_id=d.department_id
                  GROUP BY department_name')

bp <- barplot(dept_sal$sum_sal,
              names.arg=dept_sal$dept_name,
              col=rainbow(length(dept_sal$dept_name)),
              main='Salary Bar Chart',
              xlab='Dept Name', ylab='Salary',
              ylim=c(0,350000),
              cex.names=0.5)

text(x=bp, y=dept_sal$sum_sal,
     labels=dept_sal$sum_sal, pos=1, cex=0.5)

text(x=bp[is.na(dept_sal$dept_name)],
     y=dept_sal$sum_sal[is.na(dept_sal$dept_name)],
     labels='부서(x)',
     pos=3,
     col='red')

text(x=bp[dept_sal$sum_sal==max(dept_sal$sum_sal)],
     y=dept_sal$sum_sal[dept_sal$sum_sal==max(dept_sal$sum_sal)],
     labels='최고급여',
     pos=3, 
     col='red')

#번외 연구 : pie chart
library("doBy")
oth_p <- orderBy(~-SALARY, oth_sol)
pie(oth_p$SALARY, oth_p$DEPARTMENT_NAME, 
    radius = 1, col = rainbow(nrow(oth_p)), lty = 10)