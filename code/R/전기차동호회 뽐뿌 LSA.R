library(KoNLP)
library(tm)
library(rvest)
library(stringr)
library(lsa)

free_board <- read.csv('완결본에서 배터리만 찾은것.csv',encoding = 'CP949', stringsAsFactors = F)
#free_board <- read.csv('동호회 뽐뿌 모음.csv',encoding = 'CP949', stringsAsFactors = F)
free_board1 <- na.omit(free_board)
free_board.uni <- unique(free_board1)
free_board3 <- c()

for(k in 1:2157){#16801
free_board2 <- gsub('[\n]', '', free_board.uni[k,])
free_board2 <- gsub('[\r]', '', free_board2)
free_board2 <- gsub('[\t]', '', free_board2)
#free_board2 <- gsub('  ', ' ', free_board2)
free_board2 <- gsub('ㅠ', '', free_board2)
free_board2 <- gsub('ㅋ', '', free_board2)
free_board2 <- gsub('ㅎ', '', free_board2)
free_board2 <- gsub('ㅡ', '', free_board2)
free_board2 <- gsub('[;]', '', free_board2)
free_board2 <- gsub('[->]', '', free_board2)
free_board2 <- gsub('-_-;;;', '', free_board2)
free_board2 <- gsub('[★]', '', free_board2)
free_board2 <- gsub('[→]', '', free_board2)
free_board2 <- gsub('[│]', '', free_board2)
free_board2 <- gsub('[▲]', '', free_board2)
free_board2 <- gsub('[■]', '', free_board2)
#free_board2 <- str_replace_all(free_board2, '[^[:alpha:][:blank]^]','')
#Error in `Encoding<-`(`*tmp*`, value = "UTF-8") 뜰때 사용!!
free_board3 <- c(free_board3,free_board2)
}


#stopwords 설정함수
ko.words = function(doc){
  d = as.character(doc)
  d = str_split(d, ' ')[[1]] ## 띄어쓰기(' ')를 기준으로 한 문장을 여러 단어로 나눔
  d = paste(d[nchar(d) <= 20], collapse = ' ') ## 각 단어들에서 20자 이하인 것만 선택하여 다시 문장으로 합침
  extractNoun(d)
}

#corpus(글자 묶음)함수
free_corpus <- Corpus(VectorSource(free_board3))

#하나의 코어만 사용하게 함. 에러방지
options(mc.cores=1)

#TermDocumentMatrix생성함수
free_tm <- TermDocumentMatrix(free_corpus, control=list(tokenize=ko.words,
                                                        removePunctuation=T,
                                                        removeNumbers=T,
                                                        wordLengths=c(2, Inf),
                                                        weighting=weightBin))

#lsa 패키지
#library(lsa)
dim(free_tm)





library(slam)
word.count <- as.array(rollup(free_tm,2))
word.order <- order(word.count, decreasing = T)
freq.word <- word.order[1:1000]
row.names(free_tm[freq.word,])

#tdm을 메트릭스로 저장 1000개만
free_tm_matx <- as.matrix(free_tm[freq.word,])

#binary와 IDF 가중치를 주어 저장한다
free_tmw<-lw_bintf(free_tm_matx)*gw_idf(free_tm_matx)
free_tm_lsa <- lsa(free_tmw , 40)

#해석을 쉽게 하기 위해 Varimax 회전을 한다. 이때 GPArotation패키지 사용

library(GPArotation)
free_tm_tk <- Varimax(free_tm_lsa$tk)$loadings



for(i in 1:40){
  print(i)
  importance <- order(abs(free_tm_tk[,i]), decreasing = T)
  print(free_tm_tk[importance[1:10],i])
}


