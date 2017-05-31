# 主分析程序
options(stringsAsFactors = F)

# 以一般分詞方式確認詞庫的效果 實際分析或許用關鍵字取得方式比較好
library(text2vec)
library(jiebaR)
library(stats)
library(stringr)

# init jieba segment
seg = worker(bylines = TRUE, dict = "./dict/dict_zhtw.txt", stop_word = "./dict/stop_words.utf8.txt")

# set user dictionary
new_user_word(seg, 'FF')
new_user_word(seg, 'CWT')
new_user_word(seg, '工商')
new_user_word(seg, '手滑')

data = read.csv('data.csv')

contents = data$content
rm(data)

# clear data
# clear html tag
contents = gsub("<.*?>", " ", contents)
# 清除 wwwww 這個特殊詞
contents = gsub("w+$", "", contents, ignore.case = TRUE)
contents = gsub("^w+", "", contents, ignore.case = TRUE)
# 清除 xd 這個特殊詞
contents = gsub("xd+", "", contents, ignore.case = TRUE)
# 清除從頭到尾都沒有中文的內容
contents = gsub("^[^\u4E00-\u9FA5]*$", "", contents)
# 清理掉被篩選掉完全為空的條目
contents = contents[contents!=""]

# try just get content has 'FF' or 'CWT'
contents = contents[grepl('FF', contents, ignore.case = TRUE) | grepl('CWT', contents, ignore.case = TRUE)]

# 將英文部份先拋出去 避免jieba的英文分詞問題
eng_cut = str_match_all(contents, '[A-z]{2,}')

contents = gsub("[A-z]{2,}", "", contents, ignore.case = TRUE)

# 分詞
seg_results = segment(contents, seg)

# TODO: 把英文部份塞回去
q = list()

for(index in 1:length(seg_results)) {
  q[[index]] = c(seg_results[[index]], eng_cut[[index]][,1])
}

seg_results = q
rm(q)
rm(eng_cut)

# TRY: 或許這邊試試看進行提取關鍵字？



token = itoken(seg_results)
vocab = create_vocabulary(token)

vectorizer = vocab_vectorizer(vocab)
dtm_train = create_dtm(token, vectorizer)

# kms <- kmeans(dtm_train, centers = 3, nstart = 1)
# ratio <- kms$tot.withinss / (kms$tot.withinss + kms$betweenss)

