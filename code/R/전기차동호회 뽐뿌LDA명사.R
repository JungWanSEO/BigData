library(KoNLP)
library(tm)
library(rvest)
library(stringr)
library(rJava)

free_board <- read.csv('동호회 뽐뿌 모음.csv',encoding = 'CP949', stringsAsFactors = F)

free_board1 <- na.omit(free_board)
free_board.uni <- unique(free_board1)
free_board3 <- c()

for(k in 1:16801){
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

dim(free_tm)
library(slam)
word.count <- as.array(rollup(free_tm,2))
word.order <- order(word.count, decreasing = T)
freq.word <- word.order[1:1000]
row.names(free_tm[freq.word,])

dtm <- as.DocumentTermMatrix(free_tm[freq.word,]) #as.DocumentTermMatrix 는 TermDocumentMatrix를 DocumentTermMatrix로 바꿔준다.//tdm[freq.word,] 어렇게하면 tdm에서 상위 1000개 단어만 뽑아준다.

library(topicmodels) #lda패키지에도 있지만 성능이 딸림. 그래서 topicmodels사용
#dtm2ldaformat : DocumentTermMatrix를 LDA를 변환하기 위한 함수
ldaform <- dtm2ldaformat(dtm, omit_empty = T)

library(lda) #LDA를 사용하기 위한 패키지
#lda.collapsed.gibbs.sampler : 이 함수 전까지는 그냥 데이터 전처리 과정임.LDA는 이거 하나면 됨. 단 옵션이 좀 많음.
#documents = : documents를 설정 해줌, dtm2ldaformat함수로 지정한변수(ldaform)에 documents를 넣어줌(documents = ldaform$documents)
#K = 숫자 : 토픽의 갯수를 정해줌
#vocab = :어떤 단어들이 있는지 알려주는것, dtm2ldaformat함수로 지정한변수(ldaform)에 vocab있음(vocab = ldaform$vocab)
#num.iterations = : LDA는 한번에 결과가 나오는것이 아니라 계속 반복 계산을 하면 계산을 하면 할 수록 LDA결과가 점점 좋아짐, 그 반복 횟수를 정해줌.대중이 없다. 5000정도가 적당
#burnin = : 앞부분의 결과는 부정확한 결과가 많음. 따라서 앞부분의 계산결과를 버림. 대중이 없다.
#alpha =  : 30개의 토픽이 얼마나 골고루 들어갈지를 정해주는 것. 1일때 경우의 수가 일정(이건 말이 안됨) 따라서, 1을 초과하게 설정하면 모든문서가 토픽의 배합비율이 비슷하게 됨. 따라서 0.0XX이런식으로 하는데 대중이 없다.계속 수정하면서 경험적으로 알아야 함
#eta = : 한 토픽내에서 단어가 얼마나 골고루 섞여 있느냐 이런것, 0에 가까운 값을 써야함.대중이 없음

result.lda <- lda.collapsed.gibbs.sampler(documents = ldaform$documents,
                                          K = 30,
                                          vocab = ldaform$vocab,
                                          num.iterations = 100000,
                                          burnin = 1000,
                                          alpha = 0.01,
                                          eta = 0.01) 

top_word <- top.topic.words(result.lda$topics)

write.csv(top_word, '배터리만 모은것 LDA명사10만번계산.csv')

write.csv(result.lda$topic_sums, '배터리만 모은것 LDA명사10만번계산sums.csv')
