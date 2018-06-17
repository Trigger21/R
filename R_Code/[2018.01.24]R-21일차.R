## R-21일차(2018-1-24)
  # 21-1. 시군구별 이동현황
  # 21-2. seoul trip


## 21-1. 시군구별 이동현황
move <- read.csv("c:/r/시군구별_이동자수.csv", header = T, stringsAsFactors = F)
move
mo1 <- move[,c("행정구역.시군구.별",
               "X2017..09","X2017..09.1","X2017..09.2",
               "X2017..10","X2017..10.1","X2017..10.2",
               "X2017..11","X2017..11.1","X2017..11.2")]
mo1
mo2 <- mo1[,c(1,4,7,10)]
mo2 <- mo2[-1,]
names(mo2) <- c("행정구역", "순이동(9월)", "순이동(10월)","순이동(11월)")
mo2$`순이동(9월)` <- as.numeric(mo2$`순이동(9월)`)
mo2$`순이동(10월)` <- as.numeric(mo2$`순이동(10월)`)
mo2$`순이동(11월)` <- as.numeric(mo2$`순이동(11월)`)
mo3 <- melt(mo2)
mo3
barplot(mo3$value, names.arg = mo3$행정구역, las = 2)
mo2$avs <- rowMeans(mo2[,-1])
mo2
barplot(mo2$avs[-1], names.arg = mo2$행정구역[-1], las = 2)
pop <- read.csv("c:/r/인구(2016).csv", header = T, stringsAsFactors = F)
pop <- pop[-(1:5),]
names(pop) <- c("loc","cnt")
pop  # 시,도별 인구수 
library(ggmap)

# test 
geocode("south korea")
map <- qmap("south korea", zoom = 7, maptype = "roadmap")
map
library(ggplot2)
loc_df <- geocode(location = enc2utf8(as.character(pop$loc)))
loc_df           

pop$lon <- loc_df$lon
pop$lat <- loc_df$lat

pop$cnt <- as.numeric(as.character(pop$cnt))

library(doBy)
orderBy(~-cnt, pop) 

colMeans(loc_df)

m <- get_map(location = colMeans(loc_df), zoom = 7,maptype = "roadmap", source = "google", color = "bw")
ggmap(m)

ggmap(m)+
  geom_point(data = pop, aes(x = lon, y = lat, size = cnt, colour = loc))

install.packages('maps')
library('maps')
md <- map_data(region = pop$loc)


## 21-2. seoul trip
seoul_trip <- readLines("c:/r/seoul_trip.txt")
seoul_trip
library(KoNLP)

buildDictionary(ext_dic="sejong",
                user_dic=data.frame(readLines("c:/r/seoul_trip_plc.txt"),c("ncn")),replace_usr_dic=T)

stxt1 <- SimplePos09(seoul_trip)
library(stringr)
strx2 <- str_extract_all(stxt1, '([가-�R]+)/N')
strx2 <- unlist(strx2)
strx2

strx3 <- unlist(strsplit(strx2, "/N"))
strx3
grep("^서울$",strx3,value = T)
strx3[strx3 == "서울"] <- NA

strx3[strx3 == "명소"] <- NA
strx3[strx3 == "서울야경명소낙산공원"] <- "낙산공"
strx3[strx3 == "낙산공"] <- "낙산공원"

grep("서울", strx3, value = T)

strx3 <- gsub(grep("명소",strx3,value = T),"",strx3)

grep("데이트", strx3, value = T)
strx3 <- gsub(grep("데이트", strx3, value = T),NA,strx3)

grep("전쟁기념$", strx3 ,value = T)
strx3 <- gsub(grep("전쟁기념$", strx3 ,value = T), "전쟁기념관", strx3)

grep("응봉$", strx3, value = T)
strx3[strx3 == "응봉"] <- NA

grep("국립중앙", strx3, value = T)
strx3[strx3 == "국립중앙"] <- "국립중앙박물관"
strx3[strx3 == "국립중앙박"] <- "국립중앙박물관"
strx3[strx3 == "국립중앙박물"] <- "국립중앙박물관"

grep("[^시크릿]가.",strx3,value = T)
strx3 <- gsub(grep("[^시크릿]가.",strx3,value = T), NA, strx3)

grep("북악스카이", strx3, value = T)
strx3 <- gsub("북악스카이$", "북악스카이웨이", strx3)

grep("힐튼", strx3, value = T)
strx3[strx3 == "그랜드힐튼서울에서"] <- "그랜드힐튼"
strx3[strx3 == "그랜드그랜드힐튼울"] <- "그랜드힐튼"

grep("블로그", strx3, value = T)
strx3 <- gsub("블로그", NA, strx3)

grep("검색", strx3, value = T)
strx3 <- gsub("검색", NA, strx3)

strx3 <- gsub("야경", NA, strx3)
strx3 <- gsub("추천", NA, strx3)
strx3 <- gsub("벚꽃", NA, strx3)
strx3 <- gsub("사진", NA, strx3)
strx3 <- gsub("오늘", NA, strx3)
strx3 <- gsub("주말", NA, strx3)
strx3 <- gsub("나들이", NA, strx3)
strx3 <- gsub("단풍", NA, strx3)
strx3 <- gsub("요한", NA, strx3)
strx3 <- gsub("소개", NA, strx3)
strx3 <- gsub("행복", NA, strx3)
strx3 <- gsub("출사", NA, strx3)
strx3 <- gsub("서울에", NA, strx3)
strx3 <- gsub("코스", NA, strx3)
strx3 <- gsub("하나", NA, strx3)
strx3 <- gsub("근교", NA, strx3)
strx3 <- gsub("모습", NA, strx3)
strx3 <- gsub("이곳", NA, strx3)
strx3 <- gsub("날씨", NA, strx3)
strx3 <- gsub("시간", NA, strx3)
strx3 <- gsub("여행", NA, strx3)
strx3 <- gsub("감성", NA, strx3)
strx3 <- gsub("구경", NA, strx3)
strx3 <- gsub("촬영", NA, strx3)
strx3 <- gsub("관광", NA, strx3)
strx3 <- gsub("안녕", NA, strx3)
strx3 <- gsub("유명한", NA, strx3)
strx3 <- gsub("풍경", NA, strx3)

grep("북촌한옥", strx3, value = T)
strx3[strx3 == "북촌한옥마"] <- "북촌한옥마을"
table(unlist(str_extract_all(seoul_trip, "[[:alpha:]]{0,}[[:space:]]{0,}한옥[[:space:]]{0,}[[:alpha:]]{0,}")))
numeric 무조건 double -> 방지법 1L

strx3 <- strx3[nchar(strx3) > 1]
strx4 <- table(strx3)

strx5 <- sort(strx4, decreasing = T)[1:19]
strx5
library(wordcloud2)
wordcloud2(strx5)
barplot(strx5, las = 2, ylim = c(0,30))
sum(integer(0))