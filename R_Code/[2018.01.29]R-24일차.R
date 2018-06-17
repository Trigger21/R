## R-24일차(2018.1.29)

# 24-1. 크롤링 실습

library(rvest)
library(dplyr)

html <- read_html("https://search.naver.com/search.naver?query=%EC%8A%A4%ED%85%8C%ED%8C%90+%EC%BB%A4%EB%A6%AC&where=news&ie=utf8&sm=nws_hty")
html

url <- html_nodes(html, css = ".news")%>%
  html_nodes("a")%>%
  html_attr("href")
url <- unique(url)
url <- url[-1]
url

html1 <- read_html(url[1])
html1

html_nodes(html1, "#article")%>%
  html_text()
text <- NULL
html <- NULL

for(i in 600:670){
  #if(nchar(i) == 1){ i <- paste0("00",i) }
  #else if(nchar(i) == 2){ i <- paste0("0",i) }
  
  html <- read_html(paste0("http://sports.news.naver.com/basketball/news/read.nhn?oid=486&aid=0000000", i))
  
  text <- c(text, html_nodes(html, "#content")%>% html_text())
}
text
html <- read_html("https://section.blog.naver.com/Search/Post.nhn?pageNo=1&rangeType=ALL&orderBy=sim&keyword=%ED%95%98%EC%A1%B0%EB%8C%80")
html

url <- html_nodes(html, css = ".list_search_post .info_post .title_post")%>%
  html_nodes("a")%>%
  html_attr("href")
url       

# 니체 어록
html <- read_html("https://ko.wikiquote.org/wiki/%ED%94%84%EB%A6%AC%EB%93%9C%EB%A6%AC%ED%9E%88_%EB%8B%88%EC%B2%B4")
html
text <- html_nodes(html, css = "#bodyContent")%>%
  html_text()
text

library(KoNLP)
useSejongDic()
useNIADic()
library(stringr)

text1 <- gsub("[[:cntrl:]]", " ", text)
text1

text2 <- SimplePos09(text1)
text2

text3 <- unlist(str_match_all(text2, '([가-�R]+)/N'))
text3 <- text3[!str_detect(text3, "/")]
text3

text2[grep("<", text2)] <- ""
text2[grep("위키", text2)] <- ""
text2[grep("니체", text2)] <- ""

text3 <- SimplePos09(text2)
text3 <- text3[!str_detect(text3, 
                           "것|년|그|나|수|자|일|리|글|말|나|데|덕|를|속|저|판|중|만|안|이|너|지|세|듯|대|살|때")]

text4 <- sort(table(text3), decreasing = T)
head(text4, 10)

library(wordcloud2)
wordcloud2(head(text4, 30))

library(colorspace)
barplot(head(text4, 10), cex.names = .8, las = 2, 
        ylim = c(0, 20), col = heat_hcl(10))
html <- read_html("https://www.youtube.com/watch?v=U1BH36BQrOw")
html

txt <- html_nodes(html, "#content")%>%
  html_text()
txt

install.packages("RSelenium")
library(RSelenium)
Sys.sleep(1) # 1초 뒤에 시작해라 