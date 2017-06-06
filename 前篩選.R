library(readr)
library(magrittr)
data = read_csv('csv/data.csv')

content = data$content
pid = data$pid
rm(data)

# clear data

content = content %>%
  gsub("<.*?>", " ", x = .) %>%                 # clear html tag
  gsub("w{2,}", "", x = ., ignore.case = T) %>% # 清除 wwwww 這個特殊詞
  gsub("xd+", "", x = ., ignore.case = T) %>%   # 清除 xd 這個特殊詞
  gsub("&.*?;", " ", x = .) %>%                 # 排除escape掉的html符號 (&nbsp;之類)
  gsub("^[^\u4E00-\u9FA5]*$", " ", x = .)       # 清除從頭到尾都沒有中文的內容

# 清理掉被篩選掉完全為空的條目
check = content!=""
content = content[check]
pid = pid[check]

# try just get content has 'FF' or 'CWT'
check = grepl('(^|[^a-zA-Z])*FF\\d*([^a-zA-Z]|$)', content, ignore.case = T) | grepl('(^|[^a-zA-Z])*CWT(T|K)*\\d*([^a-zA-Z]|$)', content, ignore.case = T)
content = content[check]
pid = pid[check]
rm(check)

content = trimws(content)

content = as.data.frame(content, stringsAsFactors = F)
pid = as.data.frame(pid, stringsAsFactors = F)

data = cbind(pid, content)

write_csv(data, 'csv/subdata.csv')
rm(content)
rm(data)
rm(pid)
