library(readr)
library(jiebaR)
library(stringr)

data = read_csv('csv/subdata.csv')

contents = data$content

# 設定基本的分詞器
seg = worker(bylines = TRUE, dict = "./dict/dict_zhtw.txt", stop_word = "./dict/stop_words.utf8.txt")

#設定包含特殊詞的分詞器
seg2 = worker(bylines = TRUE, dict = "./dict/dict_zhtw.txt", stop_word = "./dict/stop_words.utf8.txt")

new_user_word(seg2, 'FF')
new_user_word(seg2, 'CWT')
new_user_word(seg2, '工商')
new_user_word(seg2, '手滑')
new_user_word(seg2, '場前')

# 將英文部份先拋出去 避免jieba的英文分詞問題
eng_cut = str_match_all(contents, '[A-z]{2,}')

contents_without_eng = gsub("[A-z]{2,}", "", contents, ignore.case = TRUE)

# 分詞
seg_results = segment(contents_without_eng, seg)

# TODO: 把英文部份塞回去
q = list()

for(index in 1:length(seg_results)) {
  q[[index]] = c(seg_results[[index]], eng_cut[[index]][,1])
}

seg_results = q

get = function(index){
  print(contents[[index]])
  print(seg_results[[index]])
  
}