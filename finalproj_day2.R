library(gutenbergr)
library(tidyverse)
library(stringr)
library(tidytext)

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

asdf <- getwordfreq(c(2,3))

mirror <- 'http://mirrors.xmission.com/gutenberg/'
test <- gutenberg_download(768, mirror)

tidytest <- test %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>% 
    count(word, sort = TRUE)


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

temp <- getwordfreq(filt$gutenberg_id)
str(temp)
View(temp)

# subject counts
subjCounts <- group_by(filt, subject) %>% summarise(freq = n()) %>% arrange(desc(freq))

subjCounts <- subjCounts[1:20,]

subjCounts$subject <- factor(subjCounts$subject, levels = subjCounts$subject)

ggplot(data = subjCounts) +
    geom_bar(aes(x = subject, weight = freq)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

