## R-20일차(2018.1.23)

  # 20-1. regexpr
  # 20-2. gregexpr
  # 20-3. Shilla Hotel Review


#[문제183] jeju.txt 파일에 있는 데이터를 R로 읽은 후 모든 숫자들의 빈도수를 확인하세요.

jeju <- readLines("c:/r/jeju.txt") 
jeju <- setdiff(jeju, '') 
jeju <- setdiff(jeju, " ")

View(jeju)

library(KoNLP)
useSejongDic()

library(stringr)
tx1 <- unlist(strsplit(jeju,'[^[:digit:]]'))
tx1
nchar(tx1)
tx1 <- tx1[nchar(tx1) > 0]
tx1 <- sort(as.numeric(tx1))
tx2 <- table(tx1)
tx2

library(wordcloud2)
wordcloud2(tx2)

library(colorspace)

barplot(tx2, 
        las = 2, cex.names = .7, col = heat_hcl(length(tx2)))


#[문제184] jeju.txt 파일에 있는 데이터를 R로 읽은 후 모든 숫자들의 앞,뒤에 문자가 있거나 없는경우 빈도수를 확인하세요.

#문자가 있는 숫자
ex_184 <- grep("[0-9]+", strsplit(jeju,'-'), value = T)

tx_1 <- unlist(extractNoun(ex_184))

tx_2 <- grep("([0-9][가-�R])+", tx_1, value = T)

tx_2 <- strsplit(tx_2,"\"")

tx_3 <- grep("[[:digit:]]", unlist(tx_2), value = T)

tx_3

#sol
grep("곳정도가",tx_3,value = T)
tx_3 <- gsub("부터", "", tx_3)
tx_3 <- gsub("곳정도가", "", tx_3)
tx_3 <- gsub("까지", "", tx_3)
tx_3 <- gsub("까", "", tx_3)
tx_3 <- gsub("까지", "", tx_3)
ex_184 <- strsplit(unlist(strsplit(jeju," ")), "[[:punct:]]")

table(unlist(str_extract_all(ex_184, "\\w?\\d+\\w?")))

#선생님 풀이(중요)
table(unlist(str_extract_all(jeju,"\\d+")))
mydigits <- str_extract_all(jeju, "[[:digit:]]{1,}")
table(unlist(mydigits))
table(unlist(str_extract_all(jeju, "\\w?\\d+\\w?")))
mydigits <- str_extract_all(jeju, '[[:alpha:]]{0,}[[:digit:]]{1,}[[:alpha:]]{0,}')
table(unlist(mydigits))
mypunct <- str_extract_all(jeju, '[[:alpha:]]{0,}[[:punct:]]{1,}[[:alpha:]]{0,}')
table(unlist(mypunct))
mystc <- str_extract_all(jeju, "[[:alpha:]]{0,}습니다.")  # {0,} : 0번 이상 매칭하면 찾는다 
table(unlist(mystc))


## 20-1. regexpr
# - 지정된 패턴 문자가 처음 나오는 위치를 반환(리스트형)하는 함수
sentence <- 'r big data analysis for value creation'
regexpr('big', sentence)
attr(regexpr('big',sentence), "match.length")


## 20-2. gregexpr
# - 지정된 패턴이 등장하는 모든 텍스트 위치를 반환 
regexpr('a', sentence)
gregexpr('a', sentence)
gregexpr('a', sentence)[[1]][5]
txt <- "abcdefABCDEF012345+!-ghizk가나다라마바"

#알파벳만 찾아야 할 때
g_alpha <- gregexpr('[A-z]', txt)
g_alpha
x <- unlist(strsplit(txt, character(0)))
x[unlist(g_alpha)]


unlist(str_extract_all(txt, "[A-z]"))
grep("[A-z]",unlist(strsplit(txt, character(0))), value = T)
index <- g_alpha[[1]]
index

len <- length(index)
len

for(i in 1:len){
  cat(substr(txt, index[i], index[i]))
}
x <- unlist(str_extract_all(txt, "[A-z]"))
r <- c()

for(i in 1:length(x)){
  r <- paste(r, x[i],sep="")
}

r
for(i in unlist(g_alpha)){
  cat(substr(txt, i, i))
}

#한글만 뽑아보자
han <- gregexpr('[가-�R]', txt)

for(i in unlist(han)){
  cat(substr(txt, i, i))
}

regmatches(txt, han)
regmatches(txt, han, invert = T)  # not
y <- unlist(strsplit(txt, character(0)))[unlist(han)]
r <- c()

for(i in 1:length(y)){
  r <- paste(r, y[i], sep = "")
}
r

#-- \p{Hangul}

gregexpr("\\p{Hangul}", txt, perl = T)
"""
\d : Digit, 0,1,2,..9

\D : 숫자가 아닌 것

\s : 공백

\S : 공백이 아닌 것

\w : 단어

\W : 단어가 아닌 것

\t : Tab

\n : New Line(엔터문자)

^ : 시작되는 문자

$ : 마지막 글자

\ : excape Character(탈출문자)

| : 두개이상의 조건을 동시에 지정

[ab] : a 또는 b

[^ab] : a와 b를 제외한 모든 문자

[0-9] : 모든 숫자

[A-Z] : 영어 대문자

[a-z] : 영어소문자

[A-z] : 모든 영문자(대소문자 전부)

i+ : i가 최소 1회는 나오는 경우

i* : i가 최소 0회 이상 나오는 경우

i? : i가 최소 0회에서 최대 1회만 나오는 경우

i{n} : i가 연속적으로 n회 나오는 경우

i{n1,n2} : i가 n1에서 n2회 나오는 경우

i{n,} : i가 n회 이상 나오는 경우

[:alnum:] : 문자와 숫자가 나오는 경우; [:alpha:] and [:digit:]

[:alpha:] : 문자가 나오는 경우; [:lower:] and [:upper:] (한글도 나옴)

[:blank:] : 공백이 있는 경우

[:cntrl:] : 제어 문자가 있는 경우(\n, \t)

[:digit:] : Digits : 0 1 2 3 4 5 6 7 8 9

[:graph:] : Graphical characters: [:alnum:] and [:punct:]

[:lower:] : 소문자가 있는 경우

[:print:] : 숫자, 문자, 특수문자, 공백 모두

[:punct:] : 특수문자

[:space:] : 공백문자

[:upper:] : 대문자가 있는 경우

[:xdigit:] : 16진수가 있는 경우 : 0 1 2 3 4 5 6 7 8 9 A B C D E F a b c d e f
"""

## 20-3. Shilla Hotel Review
shilla <- readLines("c:/r/Shilla_Hotel_Review.txt")
shilla
jumsu <- str_extract_all(shilla, '(점수: )[0-9]')
jumsu <- unlist(jumsu)
num <- grep("[0-9]",unlist(strsplit(jumsu, " ")), value = T)
num <- sort(num)
table(num)

sum(as.numeric(num))/length(as.numeric(num))
num_df <- as.data.frame(table(num))
num_df

for(i in 1:nrow(num_df)){
  
  num_df$pro[i] <- as.numeric(num_df$num)[i] * num_df$Freq[i]
  
}

num_df
bp <- barplot(num_df$pro, names.arg = num_df$num,
              main = "신라호텔 리뷰점수", 
              ylim = c(0,350), col = rainbow_hcl(5), las = 1)
text(x = bp, y = num_df$pro, label = paste(num_df$pro,"(",num_df$Freq,"표)",sep = ""), 
     pos = 3, col = "blue")
abline(h = mean(num_df$pro), lwd = 2, col = "red")
buildDictionary(ext_dic="sejong",
                user_dic=data.frame(readLines("c:/r/Shilla_word.txt"),c("ncn")),replace_usr_dic=T)
shx1 <- setdiff(unlist(strsplit(shilla, "=")),"")
shx2 <- SimplePos22(shx1)

shx3 <- unlist(str_extract_all(shx2, '([A-z가-�R]+)/N'))
shx4 <- unlist(strsplit(shx3, '/N'))


grep("리뷰", shx4, value = T)
shx4 <- gsub("리뷰",NA,shx4)

shx4 <- shx4[nchar(shx4) > 1]

shx5 <- table(shx4)
wordcloud2(shx5)