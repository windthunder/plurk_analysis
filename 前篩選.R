library(readr)
data = read_csv('csv/data.csv')

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

contents = trimws(contents)

content = contents

write_csv(as.data.frame(content, stringsAsFactors = F), 'csv/subdata.csv')

rm(contents)
rm(content)
