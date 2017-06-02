library(readr)
data = read_csv('csv/data.csv')

contents = data$content
rm(data)

# clear data
lapply(contents, function(x){
  # clear html tag
  x = gsub("<.*?>", " ", x)
  # 清除 wwwww 這個特殊詞
  x = gsub("w{2,}", "", x, ignore.case = TRUE)
  # 清除 xd 這個特殊詞
  x = gsub("xd+", "", x, ignore.case = TRUE)
  # 排除escape掉的html符號 (&nbsp;之類)
  gsub("&.*?;", "", x)
  # 清除從頭到尾都沒有中文的內容
  x = gsub("^[^\u4E00-\u9FA5]*$", "", x)
})

# 清理掉被篩選掉完全為空的條目
contents = contents[contents!=""]

# try just get content has 'FF' or 'CWT'
contents = contents[grepl('(^|[^a-zA-Z])*FF\\d*([^a-zA-Z]|$)', contents, ignore.case = TRUE) | grepl('(^|[^a-zA-Z])*CWT(T|K)*\\d*([^a-zA-Z]|$)', contents, ignore.case = TRUE)]

contents = trimws(contents)

content = contents

write_csv(as.data.frame(content, stringsAsFactors = F), 'csv/subdata.csv')

rm(contents)
rm(content)
