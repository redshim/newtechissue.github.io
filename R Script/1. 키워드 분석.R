#install.packages("extrafont")

#library(extrafont)
#font_import()
#font()

setwd("D://CMI//save_cpt//CC0005//CC0007")
#D:\CMI\save_cpt\CC0005\CC0007

df <- read.csv(file='K0000023.txt', sep = ",", header = FALSE)

#헤더
names(df) <- c("portal","type","keyword","date","url","contents")

#데이터 정제
df_filter <- subset(df, (contents != "no_save") ,select=c(keyword, contents))

df_filter <- subset(df_filter, (contents != "parsing fail"),select=c(keyword, contents))
df_filter <- subset(df_filter, (contents != ""),select=c(keyword, contents))
df_filter <- subset(df_filter, (contents != ""),select=c(keyword, contents))

df_filter <- unique(df_filter)



#filter(df_filter, grepl('문제|이슈', df_filter$contents))
#grepl('문제|이슈', df_filter$contents)


library(tidyverse)
library(stringr)

df_filter <- df_filter %>%
            filter(str_detect(contents, '문제|이슈'))

#원본데이터 제거
rm(df)

###filter example
# rownames(df_filter)
# filter(mtcars, grepl('Toyota|Mazda', mtcars$type))



##############################################################
#
# 한국어 형태소 분석
#
##############################################################

#
# 형태소 분석기 설치
#
#install.packages('KoNLP')

#
# 불러오기
#

Sys.setenv(JAVA_HOME="")  # 충돌 방지
library(KoNLP)

library(stringr)
##############################################################
#
# Term-Document Matrix
#
##############################################################

#
# 명사, 용언 추출 함수
#
ko.words <- function(doc){
  d <- as.character(doc)
  pos <- paste(SimplePos09(d))
  extracted <- str_match(pos, '([가-힣]+)/[NP]')
  keyword <- extracted[,2]
  keyword[!is.na(keyword)]
}


ko.words2 <- function(doc){
  d <- as.character(doc)
  pos <- paste(SimplePos09(d))
  extracted <- str_match(pos, '([가-힣]+)/[NP]')
  keyword <- extracted[,2]
  keyword[!is.na(keyword)]
}

#
# TDM 만들기
#
#install.packages('tm')
library(tm)
options(mc.cores=1)  # 윈도에서 충돌 방지
#df_filter[2]

cps <- Corpus(VectorSource(as.matrix(df_filter)))
#Term Document Matrix
tdm <- TermDocumentMatrix(cps,
                          control=list(tokenize=ko.words, # 명사, 용언만 추출
                                       removePunctuation=T, # 문장 부호 제거
                                       removeNumbers=T, # 숫자 제거
                                       wordLengths=c(2, 5))      # 2~5자
                          )
#Document Term Matrix with TF-IDF Score
dtm <- DocumentTermMatrix(cps,
                          control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE),
                                         stopwords = TRUE,
                                         tokenize=ko.words, # 명사, 용언만 추출
                                         removePunctuation=T, # 문장 부호 제거
                                         removeNumbers=T, # 숫자 제거
                                         wordLengths=c(2, 5)
                                       ))
tdm_tf <- t(dtm)

#data(crude)
#dtm <- DocumentTermMatrix(crude,
#                          control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE),
#                                         stopwords = TRUE))


inspect(tdm)
#tdm

attributes(tdm)
tdm$dimnames


##############################################################
#
# Term-Document Matrix
#
##############################################################

#
# 단어별 사용 횟수
#
library(slam)

word.occur = as.array(rollup(tdm, 2))

length(word.occur)


#
# 빈도순 정렬
#
word.order = order(word.occur, decreasing = T)
row.names(tdm)[word.order]
word.occur

##############################################################
#
# 시각화
#
##############################################################

# 상위 20개 단어만 추출
most20 = word.order[1:20]
tdm.mat = as.matrix(tdm[most20,])

# 상관행렬
cormat = cor(t(tdm.mat))

# 시각화)
library(qgraph)
qgraph(cormat, labels=rownames(cormat), diag=F, layout='spring'
                                      , theme = 'Hollywood'
                                      , label.scale=F
                                      , label.font =4 , label.fill.horizontal = 0
                                      )


##############################################################
#
# Word Cloud 시각화
#
##############################################################
## @knitr wordcloud
#Word Cloud
library(wordcloud)
m <- as.matrix(tdm)
wordFreq <- sort(rowSums(m),decreasing=TRUE)
wordFreq[1:20]

wordFreq_F <- subset(wordFreq, wordFreq < 300)
wordFreq_F <- subset(wordFreq_F, wordFreq_F >=10)

wordFreq_F[1:20]

length(wordFreq_F)

set.seed(length(wordFreq_F))
pal <- brewer.pal(8,"Dark2")
windowsFonts(malgun=windowsFont("맑은 고딕"))
wordcloud(words=names(wordFreq_F),freq=wordFreq_F
          ,min.freq=1 ,random.order=F ,rot.per=.5 ,colors=pal ,family="malgun")


##############################################################
#
# Word Cloud 시각화 -tf-idf
#
##############################################################
## @knitr wordcloud
#Word Cloud
library(wordcloud)
m_tf <- as.matrix(tdm_tf)
wordFreq <- sort(rowSums(m_tf),decreasing=TRUE)
wordFreq[1:20]

wordFreq_F <- subset(wordFreq, wordFreq < 300)
wordFreq_F <- subset(wordFreq_F, wordFreq_F >=20)

wordFreq_F[1:20]

length(wordFreq_F)

set.seed(80)
pal <- brewer.pal(8,"Dark2")
windowsFonts(malgun=windowsFont("맑은 고딕"))
wordcloud(words=names(wordFreq_F),freq=wordFreq_F
          ,min.freq=2 ,random.order=F ,rot.per=.1 ,colors=pal ,family="malgun")


##############################################################
#
# Weighting
#
##############################################################

# weighting
library(lsa)
tdm.mat = as.matrix(tdm)

# local weight
lw_tf(tdm.mat)
lw_bintf(tdm.mat)
lw_logtf(tdm.mat)

# global wegiht
gw_normalisation(tdm.mat)
gw_idf(tdm.mat)
gw_gfidf(tdm.mat)
gw_entropy(tdm.mat)

##############################################################
#
# topic modeling
#
##############################################################

library(tm)
library(lda)
library(topicmodels)
dtm = as.DocumentTermMatrix(tdm)
ldaform = dtm2ldaformat(dtm, omit_empty = T)
result.lda = lda.collapsed.gibbs.sampler(documents = ldaform$documents,
                                         K = 10,
                                         vocab = ldaform$vocab,
                                         num.iterations = 5000,
                                         burnin = 1000,
                                         alpha = 0.01,
                                         eta = 0.01)

#속성값 확인
attributes(result.lda)

#토픽 분류 상태확인
result.lda$assignments
result.lda$topics
top.topic.words(result.lda$topics)[1:10,]
result.lda$topic_sums
result.lda$document_sums


#fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab,
#                                   num.iterations = G, alpha = alpha,
#                                   eta = eta, initial = NULL, burnin = 0,
#                                   compute.log.likelihood = TRUE)
#############################################################################################################
#K <- 20
#G <- 5000
alpha <- 0.02
eta <- 0.02
D <- length(ldaform$documents)  # number of documents (2,000)
W <- length(ldaform$vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(ldaform$documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)

term.table <- table(unlist(word.order))
term.table <- sort(term.table, decreasing = TRUE)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]

theta <- t(apply(result.lda$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(result.lda$topics) + eta, 2, function(x) x/sum(x)))
#
#############################################################################################################

CustComplaints <- list(phi = phi,
                       theta = theta,
                       doc.length = doc.length,
                       vocab = ldaform$vocab,
                       term.frequency = term.frequency)


library(LDAvis)
library(servr)

# create the JSON object to feed the visualization:
json <- createJSON(phi = CustComplaints$phi,
                   theta = CustComplaints$theta,
                   doc.length = CustComplaints$doc.length,
                   vocab = CustComplaints$vocab,
                   term.frequency = CustComplaints$term.frequency,
                   encoding="UTF-8",
                   family= "Nanumgothic")

localeToCharset()
Sys.getlocale()
#iconv(~, "CP949", "UTF-8") #— "CP949" 인코딩 변환


setwd("C://R")
#시각화
serVis(json, out.dir = 'vis4', open.browser = FALSE, encoding = "UTF-8", family= "Nanumgothic")


