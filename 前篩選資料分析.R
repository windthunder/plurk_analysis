library(readr)
library(jiebaR)
library(stringr)
library(stats)
library(text2vec)

data = read_csv('csv/subdata.csv')

content = data$content

# 設定基本的分詞器
seg = worker(bylines = TRUE, dict = "./dict/dict_zhtw.txt", stop_word = "./dict/stop_words.utf8.txt")

#設定包含特殊詞的分詞器
seg2 = worker(bylines = TRUE, dict = "./dict/dict_zhtw.txt", stop_word = "./dict/stop_words.utf8.txt")

new_user_word(seg2, readLines('tags/handmade_tags.txt', encoding = 'UTF-8'))

keywords_seg = worker("keywords", topn = 5)

# 參考用函數
get = function(index){
  print(content[[index]])
  print(seg_results[[index]])
}

# 取得dtm的函數
get_dtm_train = function(seg_result){
  token = itoken(seg_result)
  vocab = create_vocabulary(token)
  
  vectorizer = vocab_vectorizer(vocab)
  dtm_train = create_dtm(token, vectorizer)  
}

try_kmeans_number = function(data){
  for(i in 2:10) {
    kmeans_result = kmeans(data, i)
    print(i)
    print(kmeans_result$tot.withinss / kmeans_result$totss)
  }  
}

# 將英文部份先拋出去 避免jieba的英文分詞問題
eng_cut = str_match_all(content, '[a-zA-Z0-9]{2,}')

content_without_eng = gsub('[a-zA-Z0-9]{2,}', "", content, ignore.case = T)

# 分詞
seg_results = segment(content_without_eng, seg2)

# 把英文部份塞回去
tmp = list()
for(index in 1:length(seg_results)) {
  concat = c(seg_results[[index]], eng_cut[[index]][,1])
  concat = concat[!grepl("^\\d+$", concat)] # 排除完全是數字的部份
  concat = concat[!grepl("^[a-zA-Z]$", concat)] # 排除只有一個英文字的類型(如果這作法排除過頭 應該要修正的是詞庫)
  tmp[[index]] = concat
}
seg_results = tmp
rm(tmp)


seg_results = lapply(seg_results, function(x){
  # TODO: 將場次名取代成EVENTNAME這個關鍵字 解決場次名混亂問題
  # CHECK: 或許非必要？ 或許把他統一成ff和cwt兩個字而非全部一樣的EVENTNAME？
  x = gsub('^FF\\d*$', 'EVENTNAME', x, ignore.case = T)
  x = gsub('^CWT(T|K)*\\d*$', 'EVENTNAME', x, ignore.case = T)
  
  # TRY: 將看起來像攤位號的東西轉換標記?
  x = gsub('^[a-z]\\d{1,2}$', 'TABLENUMBER', x, ignore.case = T)
  
  
  # TODO: 同義詞整合
  # 不知道要做到什麼程度
  x = gsub('^cosplay$', 'COS', x, ignore.case = T)
})

keyword_seg_results = lapply(seg_results, function(x){
  vector_keywords(x, keywords_seg)
})

try_kmeans_number(get_dtm_train(seg_results))



# dtm_result = get_dtm_train(keyword_seg_results)
# kmeans_result = kmeans(dtm_result, 9)
