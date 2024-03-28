library(rvest)  
library(httr)
library(XML)
library(xml2)
library(tidyverse)

## 获取每一个人员公示网页的链接（网址）

links = list()
# 获取 2-9页的链接，并添加到links列表中
for(i in 1:8){
  main_link = paste0("https://rsc.zufe.edu.cn/rczp1/rygs/",i,'.htm')
  response <- GET(main_link)
  html <- read_html(response)  
  # temp_link <- html_nodes(html, "a.c57994") 
  temp_links = html_attr(html_nodes(html, "a.c57994"), "href")
  for(link in temp_links){
    temp_link = substr(link, 7, nchar(link))
    temp_link = paste0('https://rsc.zufe.edu.cn/',temp_link)
    links = c(links,temp_link)
  }
}

# 第1页的命名逻辑与2-9页不同，需单独爬取
url <- "https://rsc.zufe.edu.cn/rczp1/rygs.htm"  
response <- GET(url)
html <- read_html(response)  
temp_links <- html_attr(html_nodes(html, "a.c57994"),"href")
for(link in temp_links){
  temp_link = substr(link, 4, nchar(link))
  temp_link = paste0('https://rsc.zufe.edu.cn/',temp_link)
  links = c(links,temp_link)
}

links

# 去重，实际上无重复链接
links = unique(links)  
paste(links[1])
links

## 从每一个网址获取表格（利用xpath定位）

trys = list()
# 手动添加xpath
xpaths = list(
  '//*[@id="vsb_content"]/div/table',
  '//*[@id="vsb_content"]/div/div/table',
  '//*[@id="vsb_content_2"]/div/table',
  '//*[@id="vsb_content_2"]/div/div/table',
  '//*[@id="vsb_content_6"]/div/div/table',
  '//*[@id="vsb_content_6"]/div/table',
  '//*[@id="vsb_content_100"]/div/table'
)
# 观察规律后利用循环构造可能的xpath字典
for(i in 1:8){
  xpaths = c(xpaths,paste0('//*[@id="vsb_content_',i,'"]/div/table'))
  xpaths = c(xpaths,paste0('//*[@id="vsb_content_',i,'"]/div/div/table'))
}

xpaths = unique(xpaths)
i = 1
error_links = list()
all_tables = list()
for(link in links){
  ## 网页地址
  # print(link)
  ## 读入网页并提取其中的表格节点
  urlb <- paste(link)
  for(xpath in xpaths){
    nodes <- html_nodes(
      read_html(urlb), xpath=paste(xpath))
    if(length(nodes) != 0){
      break
    }
  }

  ## 如果没有对应xpath，则将网址保存在error_links中
  if(length(nodes) == 0){
    error_links = c(error_links,link)
  }
  ## 从表格节点转换为表格列表
  tables <- html_table(nodes)
  all_tables = c(all_tables,tables)

  #显示爬取进度
  print(i)
  i = i+1
}

#用trys显示增加xpaths后成功的个数
table_df = data.frame(  
  error_links = length(error_links),  
  all_tables = length(all_tables) 
)
table_df
trys = rbind(trys, table_df)
trys
# 打印error_links，将存在表格网址中 的表格的xpath添加至xpaths
error_links

# trys最终数据
#       error_links   all_tables
# 1         102         65
# 2          98         69
# 3          94         73
# 4          94         73
# 5          94         73
# 6           6        161
# 7           2        165

# 下面两个地址无表格
# https://rsc.zufe.edu.cn/info/2117/8771.htm 转业士官林榕
# https://rsc.zufe.edu.cn/info/2117/8993.htm 退役士兵汪顺利

## 初步合并表格,数据清洗

summary_table1 = list()
summary_table_l6 = list()
summary_table_l7 = list()
summary_table_l8 = list()
summary_table_other = list()

# 将表格按六列、七列、八列分开汇总，其余表格添加至summary_table_others
for (table in all_tables){
  if(length(table) == 6){
    summary_table_l6 = bind_rows(summary_table_l6, table)
    next
  }
  if(length(table) == 7){
    summary_table_l7 = bind_rows(summary_table_l7, table)
    next
  }
  if(length(table) == 8){
    summary_table_l8 = bind_rows(summary_table_l8, table)
    next
  }
  summary_table_others = bind_rows(summary_table_others, table)
}

for (table in all_tables){
  summary_table1 = bind_rows(summary_table1, table)
}

# 查看分类汇总表行数
print(nrow(summary_table_l6)) # 92
print(nrow(summary_table_l7)) # 489
print(nrow(summary_table_l8)) # 47
print(nrow(summary_table_other)) # 0 
print(nrow(summary_table1)) # 628

# 存为csv格式方便查看
write_csv(summary_table_l6, "summary_table_l6.csv")
write_csv(summary_table_l7, "summary_table_l7.csv")
write_csv(summary_table_l8, "summary_table_l8.csv")

# 注意到一些数据出生日期格式不一致（年/月与年/月/日），判断此处无需修改

# 8列总表以下信息存在问题
# https://rsc.zufe.edu.cn/info/2117/9322.htm 杨泽、吴鹏、许诺三位老师网页内招聘岗位与岗位代码反了，需修改
# https://rsc.zufe.edu.cn/info/2117/9311.htm 胡稳权老师网页内招聘岗位与岗位代码反了，需修改

# 修改：调换四位老师的招聘岗位与岗位代码
# 注意到所有岗位代码均为A开头
for (i in 1:nrow(summary_table_l8)){
  if(substr(paste(summary_table_l8[i,1]),1,1) == 'A'){
    temp = summary_table_l8[i,1]
    summary_table_l8[i,1] = summary_table_l8[i,2]
    summary_table_l8[i,2] = temp
  }
}

# 7列总表以下信息存在问题
# 李佳慧、刘财国、李惠惠三位老师网页内信息无误，但格式与其他表格不符，以下为表格信息
# 招聘岗位        姓 名  准号证号       性别  出生年月   专 业        学历学位
# 专职辅导员（2） 李佳慧 000001201908   女    1996-04    应用心理学   研究生/硕士
# 专职辅导员（3） 刘财国 000001208929   男    1996-07    农艺与种业   研究生/硕士
# 专职辅导员（4） 李惠惠 000001200330   女    1996-11    公共管理     研究生/硕士

# 修改：将准考证信息删去，岗位代码列用NA填充
# 注意到三位老师第三列均以0开头
for (i in 100:nrow(summary_table_l7)){
  if(substr(paste(summary_table_l7[i,3]),1,1) == '0'){
    summary_table_l7[i,3] = summary_table_l7[i,2]
    summary_table_l7[i,2] = NA
    # temp = summary_table_l8[i,1]
    # summary_table_l8[i,1] = summary_table_l8[i,2]
    # summary_table_l8[i,2] = temp
  }
}

# 6列总表未发现问题

# 删除三个总表内多余的标题行
# 注意到所标题行均以‘招聘岗位’和‘岗位名称’开头
# 一共三张表格，因此不使用循环处理

# 处理表格summary_table_l6（下面六段代码运行时出现未知报错，但不影响程序正确执行）
for(i in 1:nrow(summary_table_l6)){
  if(summary_table_l6[i,1] == '招聘岗位'){
    summary_table_l6 = summary_table_l6[-i,]
  }
}
for(i in 1:nrow(summary_table_l6)){
  if(summary_table_l6[i,1] == '岗位名称'){
    summary_table_l6 = summary_table_l6[-i,]
  }
}

# 处理表格summary_table_l7
for(i in 1:nrow(summary_table_l7)){
  if(summary_table_l7[i,1] == '招聘岗位'){
    summary_table_l7 = summary_table_l7[-i,]
  }
}
for(i in 1:nrow(summary_table_l7)){
  if(summary_table_l7[i,1] == '岗位名称'){
    summary_table_l7 = summary_table_l7[-i,]
  }
}

# 处理表格summary_table_l8
for(i in 1:nrow(summary_table_l8)){
  if(summary_table_l8[i,1] == '招聘岗位'){
    summary_table_l8 = summary_table_l8[-i,]
  }
}
for(i in 1:nrow(summary_table_l8)){
  if(summary_table_l8[i,1] == '岗位名称'){
    summary_table_l8 = summary_table_l8[-i,]
  }
}

# 再次存为csv格式
write_csv(summary_table_l6, "summary_table_l6.csv")
write_csv(summary_table_l7, "summary_table_l7.csv")
write_csv(summary_table_l8, "summary_table_l8.csv")

## 将三张表格合并为一张

# 修改三张表的列名
names(summary_table_l6)
new_names <- c("岗位名称", "姓名", "性别", "出生日期", "毕业学校及专业", "学历/学位")  
summary_table_l6 <- summary_table_l6 %>%  
  rename_with(~ new_names, .cols = everything())  
names(summary_table_l6)

names(summary_table_l7)
new_names <- c("岗位名称","岗位代码", "姓名", "性别", "出生日期", "毕业学校及专业", "学历/学位")  
summary_table_l7 <- summary_table_l7 %>%  
  rename_with(~ new_names, .cols = everything())  
names(summary_table_l7)

names(summary_table_l8)
new_names <- c("岗位名称", "岗位代码","姓名", "性别", "出生日期", "毕业学校及专业", "学历/学位",'其 他')  
summary_table_l8 <- summary_table_l8 %>%  
  rename_with(~ new_names, .cols = everything())  
names(summary_table_l8)

# 合并列表
summary_table <- bind_rows(
  summary_table_l8,
  summary_table_l7, 
  summary_table_l6
)  
# 储存汇总表
write_csv(summary_table, "summary_table.csv")
