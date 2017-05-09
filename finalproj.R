library(gutenbergr)
library(tidyverse)
library(stringr)
library(tidytext)

#inner join - all books contain --------------------------------------

getwordfreq <- function(idvec, mirror = 'http://mirrors.xmission.com/gutenberg/'){
    first <- idvec[1]
    others <- idvec[-1]
    
    result <- gutenberg_download(first, mirror) %>% 
                         unnest_tokens(word, text) %>% 
                         anti_join(stop_words) %>% 
                         count(word)
    colnames(result)[2] <- first
    
    for(i in others){
        book = gutenberg_download(i, mirror)
        book <- book %>% 
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>% 
            count(word)
        
        colnames(book)[2] <- i

        result <- inner_join(book, result, by = "word")
        
    }
    return(result)
}

#testing func on 2 books ------------------------------------
asdf <- getwordfreq(c(2,3))
    

mirror <- 'http://mirrors.xmission.com/gutenberg/'
test <- gutenberg_download(768, mirror)

tidytest <- test %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>% 
    count(word, sort = TRUE)

#subject counts -------------------------------------------
meta <- gutenberg_works()
meta <- filter(meta, !is.na(title) & !is.na(author) & str_detect(rights, 'Public domain'))

meta <- meta[sample(1:nrow(meta), 50), ]

filt <- gutenberg_subjects %>% 
    separate(subject, paste0('c',as.character(1:10)), sep = "\\s+--\\s+") %>%
    gather(num, subject, c1:c10) %>%
    filter(!is.na(subject), subject_type != 'lcc') %>%
    select(-num)

filt$subject <- str_to_lower(filt$subject)

#filt <- unique(filt) %>% filter(subject == 'fiction')
#filt <- filt[1:10, ]

#getwordfreq(filt$gutenberg_id)

filt <- unique(filt) %>% group_by(subject) %>% summarise(freq = n()) %>% arrange(desc(freq))

filt <- filt[1:20,]

filt$subject <- factor(filt$subject, levels = filt$subject)

ggplot(data = filt) +
    geom_bar(aes(x = subject, weight = freq)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#less selective method -------------------------------

getbestwords <- function(idvec, mirror = 'http://mirrors.xmission.com/gutenberg/'){
    first <- idvec[1]
    others <- idvec[-1]
    
    result <- gutenberg_download(first, mirror) %>% 
        unnest_tokens(word, text) %>% 
        anti_join(stop_words) %>% 
        count(word)
    colnames(result)[2] <- first
    
    for(i in others){
        book = gutenberg_download(i, mirror)
        book <- book %>% 
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>% 
            count(word)
        
        colnames(book)[2] <- i
        
        result <- full_join(book, result, by = "word")
        
    }
    
    result[result[,2:ncol(result)] > 0] <- TRUE
    result[is.na(result)] <- FALSE
    
    return(result)
}

temp <- getbestwords(c(2,3), mirror)

