## svm_body_svm : 분류기
body.csv

성별  
나이 
키 
가슴둘레 
허리둘레 
배둘레 
엉덩이둘레 
겨드랑둘레 
얼굴수직길이 
머리둘레 
골격근량 
체지방량 
체수분 
단백질 
무기질 
체지방률 
복부지방률 
기초대사량 
복부지방률평가
대사량평가

body <- read.csv("/Users/hbk/data/body.csv", header = F)
# body <- read.csv("desktop/body.csv", header = F, fileEncoding="euc-kr") 한글깨짐으로 안 읽어질 경우
body

# 라이브러리 
install.packages("e1071")
library(e1071)

# na값 제거
body1 <- na.omit(body)

# 컬럼명 
colnames(body1) <- c("gender", "age", "height", "chest", "heory", "bae", "ass", "kyeo", 
                     "face_vertical", "head", "bone", "body_fat", "body_water", 
                     "protein", "mineral", "body_fat_per", "bae_fat_per", "work", "bae_fat_test",
                     "work_test")
str(body1)

# 랜덤시드 생성
set.seed(12345)

# 셔플(Shuffle)
body_ran <- body1[order(runif(12894)), ]

# 트레이닝셋 80%

body_train <- body_ran[1:10314, ]

# 테스트셋 20%

body_test <- body_ran[10315:12894, ]

# 선형SVM 훈련

body_svm <- svm(work_test~., data = body_train, kernel="linear")

body_svm

# 모델 테스트
p <- predict(body_svm, body_test, type="class")

p

table(p, body_test[, 20])

# 분류 결과 확인
mean(p == body_test[, 20])


## kNN 활용
library(class)

kbody <- na.omit(read.csv("/Users/hbk/data/body.csv", stringsAsFactors = FALSE, h = F))
kbody$V1 <- ifelse(kbody$V1 == '³²', 0, 1)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))}

kbody_n <- as.data.frame(lapply(kbody[ , 1:18], normalize))

nrow(kbody)

train_data <- kbody_n[-nrow(kbody),]

train_label <- kbody[-nrow(kbody), 19]

test_label <- kbody[nrow(kbody), 19]

test_data <- kbody_n[nrow(kbody), ]

k <- round(sqrt(nrow(kbody)))

knn(train_data, test_data, train_label, k, prob = T)

