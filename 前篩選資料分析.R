library(readr)
library(jiebaR)
library(stringr)

data = read_csv('csv/subdata.csv')

contents = data$content

# 設定基本的分詞器
seg = worker(bylines = TRUE, dict = "./dict/dict_zhtw.txt", stop_word = "./dict/stop_words.utf8.txt")

#設定包含特殊詞的分詞器
seg2 = worker(bylines = TRUE, dict = "./dict/dict_zhtw.txt", stop_word = "./dict/stop_words.utf8.txt")

new_user_word(seg2, '工商')
new_user_word(seg2, '手滑')
new_user_word(seg2, '場前')
new_user_word(seg2, '新刊')

# 參考用函數
get = function(index){
  print(contents[[index]])
  print(seg_results[[index]])
}

# 將英文部份先拋出去 避免jieba的英文分詞問題
eng_cut = str_match_all(contents, '[a-zA-Z0-9]{2,}')

contents_without_eng = gsub('[a-zA-Z0-9]{2,}', "", contents, ignore.case = TRUE)

# 分詞
seg_results = segment(contents_without_eng, seg)

# 把英文部份塞回去
q = list()
for(index in 1:length(seg_results)) {
  concat = c(seg_results[[index]], eng_cut[[index]][,1])
  concat = concat[!grepl("^\\d+$", concat)] # 排除完全是數字的部份
  concat = concat[!grepl("^[a-zA-Z]$", concat)] # 排除只有一個英文字的類型(如果這作法排除過頭 應該要修正的是詞庫)
  q[[index]] = concat
}
seg_results = q

# TODO: 將場次名取代成EVENTNAME這個關鍵字 解決場次名混亂問題
# CHECK: 或許非必要？ 或許把他統一成ff和cwt兩個字而非全部一樣的EVENTNAME？
seg_results = lapply(seg_results, function(x){
  x = gsub('^FF\\d*$', 'EVENTNAME', x, ignore.case = T)
  x = gsub('^CWT(T|K)*\\d*$', 'EVENTNAME', x, ignore.case = T)
})

# TODO: 同義詞整合

