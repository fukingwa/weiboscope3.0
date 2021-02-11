require(RSelenium)
require(stringr)
require(RPostgreSQL)
require(rvest)
require("RCurl")
require("RJSONIO")

start <- readline("1 - cold start  2 - jump start\n")

## Envirnoment constants
load("WB.RData")

if (start == 1) {

Sys.setlocale(category = "LC_ALL", locale = "C")

eCaps <- list(chromeOptions = list(
	prefs = list("detach" = TRUE),
	detach = TRUE
))

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444,
  browserName = "chrome", extraCapabilities = eCaps)

q <- remDr$open()
q <- remDr$maxWindowSize()
q <- remDr$setWindowSize(1920, 1080)
#q <- remDr$setTimeout(type = "page load", milliseconds = 500000)
#q <- remDr$setAsyncScriptTimeout(milliseconds = 300000)
#q <- remDr$setAsyncScriptTimeout(milliseconds = 300000)

censored <- c()
all <- data.frame()

remDr$navigate("https://weibo.com/login.php")

cont <- readline("1) Login Weibo \n")

}

id2mid <- function(num){
  length <- nchar(num)
  str1 <- substr(num,1,length-14)
  str2 <- substr(num,length-13,length-7)
  str3 <- substr(num,length-6,length)
  return(paste(toBase(as.numeric(str1)),toBase(as.numeric(str2)),toBase(as.numeric(str3)),sep=""))
}

mid2id <- function(num){
  length <- nchar(num)
  str1 <- substr(num,1,length-8)
  str2 <- substr(num,length-7,length-4)
  str3 <- substr(num,length-3,length)
  return(paste(to10(str1),to10(str2),to10(str3),sep=""))
}

toBase <- function(num, base=62) {
  bv <- c(seq(0,9),letters,LETTERS)
  r <- num %% base
  res <- bv[r+1]
  q <- floor(num/base)
  while (q > 0L) {
    r <- q %% base
    q  <- floor(q/base)
    res <- paste(bv[r+1],res,sep='')
  }
  res
} 

to10 <- function(num, base=62) {
  bv <- c(seq(0,9),letters,LETTERS)
  vb <- list()
  for (i in 1:length(bv)) vb[[bv[i]]] <- i
  num <- strsplit(num,'')[[1]]
  res <- vb[[num[1]]]-1
  if (length(num) > 1)
    for (i in 2:length(num)) res <- base * res + (vb[[num[i]]]-1)
  res
}

parse_wb_rds <- function(txt){

html_txt <- read_html(txt)
each_post <- html_nodes(html_txt,xpath = "//div[@action-type='feed_list_item']")
all <- data.frame()
if (length(each_post) != 0){

for (i in 1:length(each_post)){
	hs_txt <- as.character(each_post[[i]])
	hs <- read_html(hs_txt)
## Screen name
	screen_name <- html_text(html_node(hs,xpath = "//div[@class='WB_info']//a/@nick-name"))
## Created_at
	created_at <- html_text(html_node(hs,xpath = "//div[@class='WB_from S_txt2']//a/@title"))
	created_at <- strptime(created_at,"%Y-%m-%d %H:%M")
## Href
	href <- html_text(html_node(hs,xpath = "//div[@class='WB_from S_txt2']//a/@href"))
	user_id <- strsplit(href,"/|\\?")[[1]][2]
	id <- mid2id(strsplit(href,"/|\\?")[[1]][3])
## text
	text <- html_text(html_node(hs,xpath = "//div[@class='WB_text W_f14']"))
## Img
	img <- html_text(html_node(hs,xpath = "//div[@class='WB_media_wrap clearfix']//ul/@action-data"))
	img <- gsub('^[^\\=]+\\=[^\\=]+\\=[^\\=]+\\=','',img)
## RT name
	rt_screen_name <- html_text(html_node(hs,xpath = "//div[@class='WB_feed_expand']//div[@class='WB_info']//a/@nick-name"))
## RT created_at
	rt_created_at <- html_text(html_node(hs,xpath = "//div[@class='WB_feed_expand']//div[@class='WB_from S_txt2']//a/@title"))
	rt_created_at <- strptime(rt_created_at,"%Y-%m-%d %H:%M")
## RT href
	rt_href <- html_text(html_node(hs,xpath = "//div[@class='WB_feed_expand']//div[@class='WB_from S_txt2']//a/@href"))
	if (is.na(rt_href)){
		retweeted_status_user_id <- NA
		retweeted_status <- NA
	} else {
		retweeted_status_user_id <- strsplit(rt_href,"/|\\?")[[1]][2]
		retweeted_status <- as.character(mid2id(strsplit(rt_href,"/|\\?")[[1]][3]))
	}
## RT text
	rt_text <- html_text(html_node(hs,xpath = "//div[@class='WB_feed_expand']//div[@class='WB_text']"),trim=T)
## numbers
	num1 <- html_text(html_node(hs,xpath = "//div[@class='WB_feed_handle']"))
	num2 <- strsplit(gsub('[:space:][:space:]','',trimws(gsub('[^0-9]',' ',num1))),' ')[[1]]
	reposts_count <- num2[1]
	comments_count <- num2[3]
	attitudes_count <- num2[5]

      w_item <- data.frame(id=id,user_id=user_id,screen_name=screen_name,
			retweeted_status_user_id=retweeted_status_user_id,retweeted_status=retweeted_status,
			created_at=created_at,href=href,text=text,original_pic = img, in_reply_to_screen_name=rt_screen_name,
                  rt_created_at=rt_created_at, rt_href=rt_href,rt_text=rt_text,
			reposts_count=reposts_count,comments_count=comments_count,attitudes_count=attitudes_count,stringsAsFactors = F)

	all <- rbind(all,w_item)
}
}
	return(all)
}

testid <- function(testid){
  h <- all$href[all$id == testid]
  remDr$navigate(paste0("https://weibo.com/",h))
  Sys.sleep(10)
  tryCatch({
	x <- remDr$findElement(using = "xpath","//div[@class='WB_info']")  
	test <- x$getElementText()[[1]]
	if (trimws(test) == all$screen_name[all$id == testid]){
		return(FALSE)
	} else {
		return(TRUE)
	}
  }, error = function(e){
		return(TRUE)
  })
}

check_censored <- function(id){
	url <- paste0('https://api.weibo.com/2/statuses/show.json?source=',token,'&id=',as.character(id))
	r <- fromJSON(getURL(url,ssl.verifypeer=TRUE))
	if ( r$error_code == 20112){
		return(TRUE)
	} else {
		return(FALSE)
	}
}

InsertDB <- function(df){
  tona <- function(x){
    x[x == ""] <- NA
  }
  con <- dbConnect(dbDriver("PostgreSQL"), user=DB_UNAME, dbname=DB_NAME, host=HOSTIP)
  dbGetQuery(con, "set client_encoding to 'utf-8'")
  dbinserted_time <- rep(as.character(Sys.time()),nrow(df))
  strSQL <- paste(
    'insert into rp_sinaweibo (id,user_id,screen_name,retweeted_status_user_id,retweeted_status,created_at,text,original_pic,in_reply_to_screen_name,reposts_count,comments_count,attitudes_count,dbinserted) values',
    paste(sprintf("(%s,%s,'%s',%s,%s,'%s','%s','%s','%s',%s,%s,%s,'%s')",df$id,tona(df$user_id),gsub("'","''",df$screen_name),df$retweeted_status_user_id,df$retweeted_status,df$created_at,gsub("'","''",df$text),df$original_pic,gsub("'","''",df$in_reply_to_screen_name),tona(df$reposts_count),tona(df$comments_count),tona(df$attitudes_count),dbinserted_time), collapse=', '),
    'on conflict (id) do update set dbinserted = EXCLUDED.dbinserted, reposts_count = EXCLUDED.reposts_count, comments_count = EXCLUDED.comments_count, attitudes_count = EXCLUDED.attitudes_count', sep = ' '
  )
  strSQL <- gsub(",NA,",",NULL,",strSQL)
  strSQL <- gsub("\\(NA,","\\(NULL,",strSQL)
  strSQL <- gsub(",NA\\)",",NULL\\)",strSQL)
  strSQL <- gsub(",NA,",",NULL,",strSQL)
  dbSendQuery(con, strSQL)
  dbDisconnect(con)
}

#remDr$navigate("https://weibo.com/login.php")

cont <- readline("Press Return to continue \n")

while (Sys.time() < strptime("2021-02-12 10:00:00","%Y-%m-%d %H:%M:%S")) {
	click <- remDr$findElement(using = "xpath","//a[@bpfilter='main']")
	click$clickElement()
#	remDr$refresh()
	Sys.sleep(60)
	webElem <- remDr$findElement("css", "body")
	webElem$sendKeysToElement(list("\uE010"))
	Sys.sleep(60)
	webElem <- remDr$findElement("css", "body")
	webElem$sendKeysToElement(list("\uE010"))
	Sys.sleep(60)
	webElem <- remDr$findElement("css", "body")
	webElem$sendKeysToElement(list("\uE010"))
	Sys.sleep(60)
	webElem <- remDr$findElement("css", "body")
	webElem$sendKeysToElement(list("\uE010"))
	Sys.sleep(60)
	whole_body <- remDr$findElement(using = "xpath","//body")
	text_html <- whole_body$getElementAttribute("innerHTML")[[1]]
	wb_df <- parse_wb_rds(text_html)
	wb_df <- wb_df[!duplicated(wb_df$id),]
	if (nrow(wb_df)!=0){
		InsertDB(wb_df)
	}
	if (nrow(all)==0){
		all <- wb_df
	} else {
		all <- rbind(all,wb_df[!(wb_df$id %in% all$id),])
	}
	for (u in unique(wb_df$user_id)){
		u_posts <- sort(unique(as.character(wb_df$id[wb_df$user_id == u])))
		ref  <- sort(unique(as.character(all$id[all$user_id == u])))
		if (sum(grepl('TRUE-FALSE-TRUE',paste(ref %in% u_posts,collapse='-'))) > 0){
			c <- as.character(ref[!(ref %in% u_posts)])
			for (cc in c){
				if (testid(cc)){
					if ( check_censored(cc)){
						print(c)
						censored <- c(censored,c)
					}	
				}
			}
		}
	}
	click <- remDr$findElement(using = "xpath","//a[@bpfilter='main']")
	click$clickElement()
	Sys.sleep(60)	
}

#InsertDB(tm_wb)
#cookies <- remDr$getAllCookies()
#saveRDS(cookies, "cookies.rds")

#cookies <- readRDS("cookies.rds")
#for (i in 1:length(cookies)) {
#  remDr$addCookie(name = cookies[[i]][["name"]], value = #cookies[[i]][["value"]])
#}
