## R-22일차(2018.1.25)

  # 22-1. 제주도 여행경로
  # 22-2. seoul
  # 22-3. tm
  # 22-4. obama
  # 22-5. 문재인 


## 22-1. 제주도 여행경로
library(ggplot2)
library(ggmap)

jeju <- read.csv("c:/r/제주도여행코스.csv",header=T) 
jeju 
jeju1 <- get_map(location = c(lon = 126.52916660000005, lat = 33.3616666), zoom=10, maptype = "roadmap") 
jeju.map <- ggmap(jeju1)+geom_point(data=jeju, aes(x=LON, y=LAT), size=1, alpha=0.7, col="red")

#geom_path 함수를 써서 경로를 선으로 연결해서 표시합니다.
jeju.map + 
  geom_path(data=jeju,aes(x=LON,y=LAT),size=1,linetype=2,col="blue")+ 
  geom_text(data=jeju, aes(x = LON, y = LAT+0.01, label=장소),size=3)

ggsave("c:/r/jeju.png",dpi=300)

library(ggplot2)
library(ggmap)
jeju_road <- read.csv("c:/r/제주도여행코스.csv", header = T, stringsAsFactors = F)
jeju_road
geocode(enc2utf8("한라산"))
qmap(location = enc2utf8("한라산"), zoom = 10, maptype = "roadmap")
jeju_map <- get_map(location = as.numeric(geocode(enc2utf8("한라산"))), zoom=10, maptype = "roadmap")
ggmap(jeju_map)+
  geom_point(data = jeju_road, aes(x=LON, y=LAT),size=2, alpha=0.4,col="red")+  # alpha : color density 
  geom_path(data=jeju_road,aes(x=LON,y=LAT),size=1,linetype=2,col="blue")+
  geom_text(data=jeju_road, aes(x = LON, y = LAT+0.01, label=장소), size=3)


## 22-2. seoul
strx5
seoul_road <- as.data.frame(strx5)
seoul_road$strx3 <- as.character(seoul_road$strx3)
names(seoul_road) <- c("loc", "cnt")


seoul_road <- seoul_road[!seoul_road$loc %in% c("생태문화길", "고궁", "한옥마을","남산"),]

seoul_road[seoul_road$loc == "남산타워",2] <- 21

library(doBy)
seoul_road <- orderBy(~-cnt, seoul_road)

seoul_road
library(ggmap)
seoul_gc <- geocode(enc2utf8(seoul_road$loc))  # ggmap 
seoul_gc

seoul_road$lon <- seoul_gc$lon
seoul_road$lat <- seoul_gc$lat

seoul_road <- orderBy(~-lat+lon, seoul_road)

seoul_road
cen <- as.numeric(colMeans(seoul_road[,3:4]))
qmap(location = enc2utf8("성동구"), maptype = "roadmap", zoom = 12)
seoul_map <- get_map(location = enc2utf8("seo"), maptype = "roadmap", zoom = 11, color = "bw")
ggmap(seoul_map)+
  geom_point(data = seoul_road, aes(x = lon, y = lat, size = cnt, colour = loc), alpha = .5)+  # ggplot2
  geom_path(data = seoul_road, aes(x = lon, y = lat), linetype = 2, col = "red")+
  geom_text(data = seoul_road, aes(x = lon, y = lat, label = loc), check_overlap = TRUE, nudge_x = .02)

ggsave("c:/r/seoul_road.png",dpi=300)
res1 <- c()
res2 <- c()

for(i in 1:(nrow(seoul_road)-1)){
  for(j in (i+1):nrow(seoul_road)){
    res1 <- c(res1,abs(seoul_road$lon[i]-seoul_road$lon[j]) + abs(seoul_road$lat[i]-seoul_road$lat[j]))
  }
  res2 <- c(res2, min(res1))
}
res2

"I like apple and banana, but hate orange. I love banana, but not mango I hate peach, but like cherry I want to eat strawberry ~~!"
  
tm_ex <- readLines("c:/r/tm_ex.txt")
tm_ex


## 22-3. 말뭉치(tm)
# - tm : text mining 
install.packages("tm")
library("tm")
corp <- VCorpus(VectorSource(tm_ex))  # vector -> corpus(말뭉치) trans
#만약에 tm_ex가 data.frame이면 Corpus(DataframeSource()) 사용

corp
#documents : 문서의 수 (corpus 결과가 4개 만들어짐)

summary(corp)
inspect(corp)
corp[[3]]
corp[[3]]$meta
corp[[3]]$content
#말뭉치 문서를 matrix 변환(문서 X 단어), 가로줄 단어, 세로줄 단어
tdm <- TermDocumentMatrix(corp)
tdm

43 * 100 /64
#terms : 단어수 64개의 셀이 만들어짐 
#Non- : 21개의 빈도 셀수(43은 빈도정보 제공 안 됨 비어있는 칸) 
#Sparsity : 43개의 퍼센트(43 * 100 /64) 
#Maximal term length: 10(가장 긴 단어 길이)

m <- as.matrix(tdm)
m  # nrow = 16, ncol = 4
rowSums(m)

#말뭉치에 있는 2개 이상 연이어 있는 공백을 1개의 공백으로 변환
corp1 <- tm_map(corp, stripWhitespace) 
corp1

#대문자를 소문자로 변환
corp1 <- tm_map(corp1, tolower)
corp1

#숫자표현을 제가하는 방법
corp1 <- tm_map(corp1, removeNumbers)
corp1

#문장부호, 특수문자를 제거
corp1 <- tm_map(corp1, removePunctuation)
corp1
corp1 <- tm_map(corp1, PlainTextDocument)
corp1

#말뭉치에서 gsub 사용하는 방법 : 함수로 돌아가게 함
tostring <- content_transformer(function(x,from,to) gsub(from,to,x))
corp1 <- tm_map(corp1, tostring, "~", "")
corp1 <- tm_map(corp1, tostring, "!", "")
corp1 <- tm_map(corp1, tostring, ",", "")
corp1

#불용어 등록
sword2 <- c(stopwords("en"), "end", "but", "not")
corp1 <- tm_map(corp1, removeWords, sword2)  # 불용어 제거(전치사, 관사...)

#결과 확인
tdm1 <- TermDocumentMatrix(corp1)
m1 <- as.matrix(tdm1)
m1
colnames(m1) <- c(1:4)
m1
freq1 <- sort(rowSums(m1), decreasing = T)
freq1

freq2 <- sort(colSums(m1), decreasing = T)
freq2
docs <- data.frame(doc_id = c("doc_1", "doc_2"),
text = c("This is a text.",
"This another one."),
stringsAsFactors = F)

docs
ds <- DataframeSource(docs)
x <- Corpus(ds)
inspect(x)
library("tm")


## 22-4. obama 연설문 text mining
obama <- readLines("c:/r/obama.txt")
obama
class(obama)
obama1 <- VCorpus(VectorSource(obama))
obama2 <- TermDocumentMatrix(obama1)
m1 <- as.matrix(obama2)
rowSums(m1)
obama3 <- tm_map(obama1, stripWhitespace)   # 연이은 2개 공백 -> 1개
obama3 <- tm_map(obama3, tolower)           # 소문자로 변환
obama3 <- tm_map(obama3, removeNumbers)     # 숫자 제거
obama3 <- tm_map(obama3, removePunctuation) # 특수문자 제거
obama3 <- tm_map(obama3, PlainTextDocument) # 마무리 꼭 해줘야 하는거 
tostring <- content_transformer(function(x,from,to) gsub(from,to,x))
obama3 <- tm_map(obama3, tostring, "”", "")
obama3 <- tm_map(obama3, tostring, "“", "")
obama3 <- tm_map(obama3, tostring, "’s", "")
obama3 <- tm_map(obama3, tostring, "’ll", "")
obama3 <- tm_map(obama3, tostring, "’ve", "")
obama3 <- tm_map(obama3, tostring, "’re", "")
sword2 <- c(stopwords("en"), "end", "but", "not")
obama3 <- tm_map(obama3, removeWords, sword2)  # 불용어 제거(전치사, 관사...)
obama4 <- TermDocumentMatrix(obama3)
m2 <- as.matrix(obama4)
obama5 <- sort(rowSums(m2), decreasing = T)
obama5
library(wordcloud2)
library(reshape2)

obama5
wordcloud2(head(as.table(obama5), 40))


## 22-5. 문재인 취임사
moon <- readLines("c:/r/문재인대통령취임사.txt")
moon
moon1 <- VCorpus(VectorSource(moon))  
moon2 <- TermDocumentMatrix(moon1)
moon3 <- tm_map(moon1, stripWhitespace)
moon3 <- tm_map(moon3, tolower)
moon3 <- tm_map(moon3, removeNumbers)
moon3 <- tm_map(moon3, removePunctuation)
moon3 <- tm_map(moon3, PlainTextDocument)
moon4 <- TermDocumentMatrix(moon3)
moon5 <- as.matrix(moon4)
rowSums(moon5)

#Alice's Adventures in Wonderland
alice <- readLines("c:/r/alice.txt")
alice
library(tm)
alice1 <- VCorpus(VectorSource(alice))
alice1
alice2 <- tm_map(alice1, stripWhitespace)  # 2개 이상 연이어 있는 공백을 1개의 공백으로 변환
alice2 <- tm_map(alice2, tolower)          # 소문자로 통일 
alice2 <- tm_map(alice2, removeNumbers)
alice2 <- tm_map(alice2, removePunctuation)
alice2 <- tm_map(alice2, PlainTextDocument)
sword2 <- c(stopwords("en"), "end", "but", "not")
alice2 <- tm_map(alice2, removeWords, sword2)
stopwords("SMART")
alice3 <- TermDocumentMatrix(alice2)
m_alice <- as.matrix(alice3)
alice4 <- sort(rowSums(m_alice), decreasing = T)
library(wordcloud2)
wordcloud2(as.table(alice4))


#THUS SPAKE ZARATHUSTRA
zara <- readLines("c:/r/THUS SPAKE ZARATHUSTRA.txt")
zara1 <- VCorpus(VectorSource(zara))
zara1
summary(zara1)
inspect(zara1)
zara2 <- tm_map(zara1, stripWhitespace)
zara2 <- tm_map(zara2, tolower)
zara2 <- tm_map(zara2, removeNumbers)
zara2 <- tm_map(zara2, removePunctuation)
zara2 <- tm_map(zara2, PlainTextDocument)
sword2 <- c(stopwords("en"), "end", "but", "not")
zara2 <- tm_map(zara2, removeWords, sword2)
tostring <- content_transformer(function(x,from,to) gsub(from,to,x))
zara2 <- tm_map(zara2, tostring, "”", "")
zara2 <- tm_map(zara2, tostring, "‘", "")
zara2 <- tm_map(zara2, tostring, "’", "")
zara2 <- tm_map(zara2, tostring, "“", "")
zara3 <- TermDocumentMatrix(zara2)
zara_m <- as.matrix(zara3)
zara4 <- sort(rowSums(zara_m), decreasing = T)
zara4
wordcloud2(as.table(zara4))