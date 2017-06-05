library(readr)
data = read_csv('csv/data.csv')

content = data$content
pid = data$pid
rm(data)

# clear data
content = lapply(content, function(x){
  # clear html tag
  x = gsub("<.*?>", " ", x)
  # 清除 wwwww 這個特殊詞
  x = gsub("w{2,}", "", x, ignore.case = TRUE)
  # 清除 xd 這個特殊詞
  x = gsub("xd+", "", x, ignore.case = TRUE)
  # 排除escape掉的html符號 (&nbsp;之類)
  x = gsub("&.*?;", "", x)
  # 清除從頭到尾都沒有中文的內容
  gsub("^[^\u4E00-\u9FA5]*$", "", x)
})

# 清理掉被篩選掉完全為空的條目
check = content!=""
content = content[check]
pid = pid[check]

# try just get content has 'FF' or 'CWT'
check = grepl('(^|[^a-zA-Z])*FF\\d*([^a-zA-Z]|$)', content, ignore.case = TRUE) | grepl('(^|[^a-zA-Z])*CWT(T|K)*\\d*([^a-zA-Z]|$)', content, ignore.case = TRUE)
content = content[check]
pid = pid[check]

content = trimws(content)

content = as.data.frame(content, stringsAsFactors = F)
pid = as.data.frame(pid, stringsAsFactors = F)

data = cbind(pid, content)

write_csv(data, 'csv/subdata.csv')
