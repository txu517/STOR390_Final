library(gutenbergr)
library(tidyverse)
library(stringr)
library(tidytext)

# compute the cosine similarity between vectors 'A', 'B'
cosine_similarity <- function(A, B)
{
  ip = t(A) %*% B
  lA = sqrt(sum(A^2))
  lB = sqrt(sum(B^2))
  
  return(ip / (lA * lB))
}

# compute the cosine similarity matrix between all columns of a matrix/df 'col_ordered_vectors'
cosine_similarity_matrix <- function(col_ordered_vectors)
{
  n = ncol(col_ordered_vectors)
  df = matrix(0, nrow = n, ncol = n)
  for(i in 1:n)
  {
    for(j in 1:n)
    {
      df[i,j] = cosine_similarity(col_ordered_vectors[,i], col_ordered_vectors[,j])
    }
  }
  
  return(df)
}

# create a df of non-stop word frequencies for each book id in 'book_ids' and filter out words
# that are not shared by at least 'keep_threshold' percent of the books, optional 'mirror' parameter
# for downloading book tokens
get_word_freq_vectors <- function(book_ids, keep_threshold, mirror = 'http://mirrors.xmission.com/gutenberg/')
{
  first = book_ids[1]
  others = book_ids[-1]
  
  totalBookCount = length(book_ids)
  
  result = gutenberg_download(first, mirror) %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>% 
    count(word)
  
  colnames(result)[2] = paste0("id_", first)
  
  cnt = 0
  for(i in others)
  {
    book = gutenberg_download(i, mirror)
    book = book %>% 
      unnest_tokens(word, text) %>% 
      anti_join(stop_words) %>% 
      count(word)
    colnames(book)[2] = paste0("id_", i)
    result = full_join(book, result, by = "word")
    cnt = cnt + 1
    message(paste0("finished ", cnt, " / ", totalBookCount))
  }
  
  freq = result[,2:ncol(result)]
  
  freq[!is.na(freq)] = T
  freq[is.na(freq)] = F
  
  freq = rowSums(freq)
  
  result[is.na(result)] = 0
  
  result = result %>% add_column(rs = freq) %>% filter(rs >= totalBookCount * keep_threshold) %>% select(-rs)
  
  return(result)
}

# get subjects for all books in gutenberg repo
get_all_book_subjects <- function()
{
  filt = gutenberg_subjects %>% 
    separate(subject, paste0('c',as.character(1:10)), sep = "\\s+--\\s+") %>%
    gather(num, subject, c1:c10) %>%
    filter(!is.na(subject), subject_type != 'lcc') %>%
    select(-num)
  
  filt$subject = str_to_lower(filt$subject)
  
  filt = unique(filt)
  
  return(filt)
}

# get all unique authors in the gutenberg repo
get_all_authors <- function()
{
  return(gutenberg_authors %>% unique)
}

# retreive all filtered works
get_all_works <- function()
{
  return(gutenberg_works() %>% filter(!is.na(title) & !is.na(author) & str_detect(rights, 'Public domain')))
}

# filter out non-english, non-unique, non-public, empty book ids from a vector of 'book_ids'
filter_book_ids <- function(book_ids)
{
  keep_ids = get_all_works()$gutenberg_id
  
  return(book_ids[book_ids %in% keep_ids])
}

# helper column sample variance function
colVariance <- function(m)
{
  cm = colMeans(m)
  cv = 1:ncol(m)
  for(i in 1:ncol(m))
  {
    cv[i] = 0
    for(j in 1:nrow(m))
    {
      cv[i] = cv[i] + (m[j,i] - cm[i])^2
    }
    cv[i] = cv[i] / (nrow(m) - 1)
  }
  
  return(cv)
}

# helper function for obtaining the mode
vecMode <- function(v)
{
  temp = unique(v)
  temp[which.max(tabulate(match(v, temp)))]
}

# helper function for packing method of obtaining classification error based on raw word frequency vectors
# takes a 'raw_df' dataframe of words in 1 column followed by instances as following columns and a 'numPC' amount of components to use. 'trueIds' is a df with a column gutenberg_author_id
# containing the true author ids
errorCalculation <- function(raw_df, numPC, authorCount, trueIds)
{
  vec = t(data.matrix(raw_df[,2:ncol(wordFreqs)]))
  vecMeans = colMeans(vec)
  vecVar = colVariance(vec)
  
  # standardize by subtracting mean and dividing by column std
  for(i in 1:nrow(vec))
  {
    vec[i,] = (vec[i,] - vecMeans) / sqrt(vecVar)
  }
  
  # perform SVD on the standardized data
  vecSVD = svd(vec, nu = 0, nv = numPC)
  
  # transform the data into the new domain spanned by the first 'numPC' principle vectors
  vec = vec %*% vecSVD$v
  
  # calculate cosine similarity matrix for our weighted network representation
  A = cosine_similarity_matrix(t(vec)[,1:nrow(vec)])
  
  # calculate the graph Laplacian for spectral partitioning
  A = A + 1
  diag(A) = 0
  D = diag(rowSums(A))
  L = D - A
  
  # find the e.v. corresponding to smallest eigenvalues, discarding the trivial vector of (1,1,1,...,1)
  eigenL = eigen(L, symmetric = TRUE)
  U = eigenL$vectors[,ncol(eigenL$vectors):1]
  U = U[,2:authorCount]
  
  # perform k means clustering on the transformed vectors to determine cluster assignments
  set.seed(24)
  cl = kmeans(U, authorCount, nstart = 64)
  assignments = rev(cl$cluster)
  error = trueIds %>% 
    add_column(assignments) %>% 
    group_by(gutenberg_author_id) %>% 
    summarise(numMissed = sum(assignments != vecMode(assignments))) %>% 
    summarise(misclassified = sum(numMissed)) / length(assignments)
  
  return(error)
}