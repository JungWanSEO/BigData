library(KoNLP)
library(tm)
library(stringr)
library(qgraph)

nw1.p1 <- read.csv('모두 모은것.csv', encoding = 'CP949', stringsAsFactors = F)

nw1.p1.omit <- na.omit(nw1.p1)
nw1.p1.uniq <- unique(nw1.p1.omit)
nw1.p1.gsub <- gsub('지난해','', nw1.p1.uniq)
nw1.p1.gsub <- gsub('2016','', nw1.p1.gsub)
nw1.p1.gsub <- gsub('2014','', nw1.p1.gsub)
nw1.p1.gsub <- gsub('2015','', nw1.p1.gsub)
nw1.p1.gsub <- gsub('2020','', nw1.p1.gsub)
nw1.p1.gsub <- gsub('5000','', nw1.p1.gsub)
nw1.p1.gsub <- gsub('2013','', nw1.p1.gsub)
nw1.p1.gsub <- gsub('글로벌','', nw1.p1.gsub)
nw1.p1.gsub <- gsub('관계자','', nw1.p1.gsub)
nw1.p1.gsub <- gsub('모터쇼','', nw1.p1.gsub)
nw1.p1.gsub <- gsub('하반기','', nw1.p1.gsub)
nw1.p1.gsub <- gsub('기아차','', nw1.p1.gsub)
nw1.p1.gsub <- gsub('바로가기','', nw1.p1.gsub)
nw1.p1.gsub <- gsub('가운데','', nw1.p1.gsub)
nw1.p1.gsub <- gsub('테슬라는','', nw1.p1.gsub)
nw1.p1.gsub <- gsub('테슬라','', nw1.p1.gsub)
nw1.p1.gsub <- gsub('2018','', nw1.p1.gsub)
nw1.p1.gsub <- gsub('대통령','', nw1.p1.gsub)
nw1.p1.gsub <- gsub('상반기','', nw1.p1.gsub)
nw1.p1.gsub <- gsub('우리나라','', nw1.p1.gsub)
nw1.p1.gsub <- gsub('bmw','', nw1.p1.gsub)
nw1.p1.gsub <- gsub('ess','', nw1.p1.gsub)
nw1.p1.gsub <- gsub('머스크','', nw1.p1.gsub)
nw1.p1.gsub <- gsub('제네시스','', nw1.p1.gsub)
nw1.p1.gsub <- gsub('자동차','', nw1.p1.gsub)



ko.words = function(doc){
  d = as.character(doc)
  d = str_split(d, ' ')[[1]] ## 띄어쓰기(' ')를 기준으로 한 문장을 여러 단어로 나눔
  d = paste(d[nchar(d) <= 20], collapse = ' ') ## 각 단어들에서 20자 이하인 것만 선택하여 다시 문장으로 합침
  extractNoun(d)
}



nw1.corpus <- Corpus(VectorSource(nw1.p1.gsub))

options(mc.cores=1)

nw1.tdm <- TermDocumentMatrix(nw1.corpus, control=list(tokenize=ko.words, control=list(tokenize=ko.words,
                                                                                     removePunctuation=T,
                                                                                     removeNumbers=T,
                                                                                     wordLengths=c(2, 5),
                                                                                     weighting=weightBin)))
head(nw1.tdm)

nw1.tdm.matx <- as.matrix(nw1.tdm)

head(nw1.tdm.matx)

dim(nw1.tdm.matx)

nw1.count <- rowSums(nw1.tdm.matx)
nw1.order <- order(nw1.count, decreasing = T)
rownames(nw1.count)
#<u+00a0>
rownames(nw1.tdm.matx)[nw1.order[1:60]]
nw1.frequence <- nw1.tdm.matx[nw1.order[1:30],]

nw1.coo.mtx <- nw1.frequence%*%t(nw1.frequence)

qgraph(nw1.coo.mtx, 
       labels=row.names(nw1.coo.mtx), 
       diag=F, 
       layout='spring',
       edge.color='seagreen3',
       vsize=sqrt(sqrt(diag(nw1.coo.mtx)))/2
)