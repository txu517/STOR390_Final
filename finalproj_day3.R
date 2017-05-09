library(gutenbergr)
library(tidyverse)
library(stringr)
library(tidytext)
library(GGally)

cosSimilarity <- function(A, B)
{
  ip = t(A) %*% B
  lA = sqrt(sum(A^2))
  lB = sqrt(sum(B^2))
  
  return(ip / (lA * lB))
}

cosSimilarityMatrix <- function(colOrderDF)
{
  n = ncol(colOrderDF)
  df <- matrix(0, nrow = n, ncol = n)
  for(i in 1:n)
  {
    for(j in 1:n)
    {
      df[i,j] = cosSimilarity(colOrderDF[,i], colOrderDF[,j])
    }
  }
  
  return(df)
}

getwordfreq <- function(idvec, mirror = 'http://mirrors.xmission.com/gutenberg/'){
    first <- idvec[1]
    others <- idvec[-1]
    
    totalBookCount = length(idvec)
    
    result <- gutenberg_download(first, mirror) %>% 
                         unnest_tokens(word, text) %>% 
                         anti_join(stop_words) %>% 
                         count(word)
    colnames(result)[2] = paste0("c", first)
    
    cnt = 0
    for(i in others){
        book = gutenberg_download(i, mirror)
        book <- book %>% 
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>% 
            count(word)
        colnames(book)[2] = paste0("c", i)
        result <- full_join(book, result, by = "word")
        cnt = cnt + 1
        print(paste0("finished ", cnt, " / ", totalBookCount))
    }
    
    freq <- result[,2:ncol(result)]
    
    freq[!is.na(freq)] = T
    freq[is.na(freq)] = F
    
    freq <- rowSums(freq)
    
    result[is.na(result)] = 0
    
    result <- result %>% add_column(rs = freq) %>% filter(rs >= totalBookCount * 0.75) %>% select(-rs)
    
    return(result)
}


meta <- gutenberg_works()
meta <- filter(meta, !is.na(title) & !is.na(author) & str_detect(rights, 'Public domain'))

#meta <- meta[sample(1:nrow(meta), 50), ]
View(meta)

filt <- gutenberg_subjects %>% 
    separate(subject, paste0('c',as.character(1:10)), sep = "\\s+--\\s+") %>%
    gather(num, subject, c1:c10) %>%
    filter(!is.na(subject), subject_type != 'lcc') %>%
    select(-num)

filt$subject <- str_to_lower(filt$subject)

filt <- unique(filt)

filt <- filter(filt, subject == 'fiction')
filt <- filt[1:350,]

temp <- getwordfreq(c(1,2,3,5,10,14,15))
View(temp)

# svd
origVecs <- t(data.matrix(temp[,2:ncol(temp)]))
vecMeans <- colMeans(origVecs)

for(i in 1:nrow(origVecs))
{
  origVecs[i,] = origVecs[i,] - vecMeans
}

View(origVecs)

origVecsSvd <- svd(origVecs, nu = 0, nv = 50)

transVecs <- origVecs %*% origVecsSvd$v
View(transVecs)

pcaframe <- as.data.frame(transVecs)
ggplot(data = pcaframe) + geom_point(aes(x = V1, y = V2))

simMat <- cosSimilarityMatrix(t(transVecs)[,1:7])
View(simMat)

simMat <- simMat + 1
net = network(simMat, directed = FALSE, ignore.eval = FALSE, names.eval = "weights")
ggnet2(net, label = colnames(temp)[2:11], edge.size = "weights")

# subject counts
subjCounts <- group_by(filt, subject) %>% summarise(freq = n()) %>% arrange(desc(freq))

subjCounts <- subjCounts[1:20,]

subjCounts$subject <- factor(subjCounts$subject, levels = subjCounts$subject)

ggplot(data = subjCounts) +
    geom_bar(aes(x = subject, weight = freq)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

