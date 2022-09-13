require(RSelenium)
require(stringr)
require(RPostgreSQL)
require(rvest)
require(RCurl)
require(RJSONIO)
require(emayili)

set.seed(as.numeric(Sys.time()))

myip <- getURL("ifconfig.me/ip")

if (myip == ""){
	myip <- getURL("api.ipify.org")	
}

if (myip == ""){
	myip <- "0.0.0.0"	
}

what_vm <- function(x){
	a <- read.csv("https://docs.google.com/spreadsheets/d/1gB8RSRJC6hXx2bQHObpFj1lpog1cK4yztet09I-wTbg/gviz/tq?tqx=out:csv")
	return(a$VM.name[a$IP == trimws(x)])
}

myvm <- what_vm(myip)

if (!exists("start_v")){
	start_v <- readline("1 - cold start  2 - jump start\n")
}
## Envirnoment constants
if (file.exists("WB.RData")){
	load("WB.RData")
} else {
	print("Missing WB.RData")
	quit()
}
if (as.integer(start_v) == 1) {

	Sys.setlocale(category = "LC_ALL", locale = "C")

	eCaps <- list(chromeOptions = list(args = c("--disable-gpu","--window-size=1920,1080", "--lang=en", "<96>disable-features=RendererCodeIntegrity", "--disable-extensions", "--disable-software-rasterizer", "--no-sandbox"), debuggerAddress = c("127.0.0.1:9222")))

#	eCaps <- list(chromeOptions = list(
#		args = c('start-maximized','enable-automation','--disable-gpu', '--no-sandbox','--disable-extensions','--dns-prefetch-disable','--disable-infobars','--disable-dev-shm-usage','--disable-browser-side-navigation')
	#	prefs = list("detach" = TRUE),
	#	detach = TRUE
#	), debuggerAddress = c("127.0.0.1:9222"))

	remDr <- remoteDriver(
	  remoteServerAddr = "localhost",
	  port = 4444,
	  browserName = "chrome", extraCapabilities = eCaps)

	q <- remDr$open()
#	q <- remDr$maxWindowSize()
#	q <- remDr$setWindowSize(1920, 1080)
	q <- remDr$setTimeout(type = "page load", milliseconds = 500000)
	#q <- remDr$setAsyncScriptTimeout(milliseconds = 300000)
	#q <- remDr$setAsyncScriptTimeout(milliseconds = 300000)

	censored <- c()
	if (file.exists("all.rds")){
		all <- readRDS("all.rds")
	} else {
		all <- data.frame()
	}
	Sys.sleep(10)	

	remDr$navigate("https://weibo.com/login.php")
	
	start_v <- 2
	cont <- readline("1) Login Weibo ; 2) then Press Enter to start\n")
	print("Starting Weiboscope 3.0 .....")
}

start_date <- format(Sys.time(),"%Y-%m-%d")
#start_date <- '2021-12-16'
print(start_date)

checking_time <- 24
print(checking_time)

print(myip)
print(myvm)

id2mid <- function(num){
  length <- nchar(num)
  str1 <- substr(num,1,length-14)
  str2 <- substr(num,length-13,length-7)
  str3 <- substr(num,length-6,length)
#  return(paste(toBase(as.numeric(str1)),toBase(as.numeric(str2)),toBase(as.numeric(str3)),sep=""))
  return(gsub(' ','0',paste(sprintf("%s",toBase(as.numeric(str1))),sprintf("%4s",toBase(as.numeric(str2))),sprintf("%4s",toBase(as.numeric(str3))),sep="")))
}

mid2id <- function(num){
  length <- nchar(num)
  str1 <- substr(num,1,length-8)
  str2 <- substr(num,length-7,length-4)
  str3 <- substr(num,length-3,length)
  return(paste(sprintf("%02d",to10(str1)),sprintf("%07d",to10(str2)),sprintf("%07d",to10(str3)),sep=""))
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
		## Filter promotion
			prom <- html_text(html_node(hs,xpath = "//i[contains(@class,'promo')]"))
			if (!is.na(prom)){
				next
			}
		## Screen name
			screen_name <- html_text(html_node(hs,xpath = "//div[@class='WB_info']//a/@nick-name"))
		## Created_at
			created_at <- html_text(html_node(hs,xpath = "//div[@class='WB_from S_txt2']//a/@title"))
			created_at <- strptime(created_at,"%Y-%m-%d %H:%M")
		## Href
			href <- html_text(html_node(hs,xpath = "//div[@class='WB_from S_txt2']//a/@href"))
			user_id <- as.character(strsplit(href,"/|\\?")[[1]][2])
			id <- as.character(mid2id(strsplit(href,"/|\\?")[[1]][3]))
		## text
	#		text <- html_text(html_node(hs,xpath = "//div[@class='WB_text W_f14']"))
			text <- html_text(html_node(hs,xpath = "//div[@class='WB_text W_f14' and @node-type='feed_list_content_full']"))
			if (is.na(text)){
				text <- html_text(html_node(hs,xpath = "//div[@class='WB_text W_f14']"))
				link <- html_attr(html_node(hs,xpath = "//div[@class='WB_text W_f14']//a[@title]"),"href")
				text <- ifelse(is.na(link),text,paste(text,"***External link:",link))
			} else {
				link <- html_attr(html_node(hs,xpath = "//div[@class='WB_text W_f14' and @node-type='feed_list_content_full']//a[@title]"),"href")
				text <- ifelse(is.na(link),text,paste(text,"***External link:",link))
			}

		## Img
			img <- html_text(html_node(hs,xpath = "//div[@class='WB_media_wrap clearfix']//ul/@action-data"))
			img <- gsub('^[^\\=]+\\=[^\\=]+\\=[^\\=]+\\=','',img)
			if (nchar(img) > 4096 & !is.na(img)){
				img <- substr(img,1,4096)
			}			
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
				retweeted_status_user_id <- as.character(strsplit(rt_href,"/|\\?")[[1]][2])
				retweeted_status <- as.character(mid2id(strsplit(rt_href,"/|\\?")[[1]][3]))
			}
		## RT text
#			rt_text <- html_text(html_node(hs,xpath = "//div[@class='WB_feed_expand']//div[@class='WB_text']"),trim=T)
			rt_text <- ""
		## numbers
			num1 <- html_text(html_node(hs,xpath = "//div[@class='WB_feed_handle']"))
#			num2 <- strsplit(gsub('[:space:][:space:]','',trimws(gsub('[^0-9]',' ',num1))),' ')[[1]]
			num2 <- strsplit(gsub('[ ]+',',',trimws(gsub('[^0-9]',' ',num1))),',')[[1]]
			reposts_count <- num2[1]
			comments_count <- num2[2]
			attitudes_count <- num2[3]

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

rt_parse_wb_rds <- function(txt){

	html_txt <- read_html(txt)
	each_post <- html_nodes(html_txt,xpath = "//div[@action-type='feed_list_item']")
	all <- data.frame()
	if (length(each_post) != 0){

		for (i in 1:length(each_post)){
			hs_txt <- as.character(each_post[[i]])
			hs <- read_html(hs_txt)
		## Filter promotion
			prom <- html_text(html_node(hs,xpath = "//i[contains(@class,'promo')]"))
			if (!is.na(prom)){
				next
			}
		## Screen name
			screen_name <- html_text(html_node(hs,xpath = "//div[@class='WB_info']//a/@nick-name"))
		## Created_at
			created_at <- html_text(html_node(hs,xpath = "//div[@class='WB_from S_txt2']//a/@title"))
			created_at <- strptime(created_at,"%Y-%m-%d %H:%M")
		## Href
			href <- html_text(html_node(hs,xpath = "//div[@class='WB_from S_txt2']//a/@href"))
			user_id <- as.character(strsplit(href,"/|\\?")[[1]][2])
			id <- as.character(mid2id(strsplit(href,"/|\\?")[[1]][3]))
		## text
#			text <- html_text(html_node(hs,xpath = "//div[@class='WB_text W_f14']"))
			text <- ""
		## Img
			img <- html_text(html_node(hs,xpath = "//div[@class='WB_media_wrap clearfix']//ul/@action-data"))
			img <- gsub('^[^\\=]+\\=[^\\=]+\\=[^\\=]+\\=','',img)
			if (nchar(img) > 4096 & !is.na(img)){
				img <- substr(img,1,4096)
			}
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
				retweeted_status_user_id <- as.character(strsplit(rt_href,"/|\\?")[[1]][2])
				retweeted_status <- as.character(mid2id(strsplit(rt_href,"/|\\?")[[1]][3]))
			}
		## RT text
#			rt_text <- html_text(html_node(hs,xpath = "//div[@class='WB_feed_expand']//div[@class='WB_text']"),trim=T)
			rt_text <- html_text(html_node(hs,xpath = "//div[@class='WB_feed_expand']//div[@class='WB_text' and @node-type='feed_list_reason_full']"))
			if (is.na(rt_text)){
				rt_text <- html_text(html_node(hs,xpath = "//div[@class='WB_feed_expand']//div[@class='WB_text']"),trim=T)
				rt_link <- html_attr(html_node(hs,xpath = "//div[@class='WB_feed_expand']//div[@class='WB_text']//a[@title]"),"href")
				rt_text <- ifelse(is.na(rt_link),rt_text,paste(rt_text,"*** External link:",rt_link))
			} else {
				rt_link <- html_attr(html_node(hs,xpath = "//div[@class='WB_feed_expand']//div[@class='WB_text' and @node-type='feed_list_reason_full']//a[@title]"),"href")
				rt_text <- ifelse(is.na(rt_link),rt_text,paste(rt_text,"***External link:",rt_link))
			}

		## numbers
			num1 <- html_text(html_node(hs,xpath = "//div[@class='WB_feed_handle']"))
#			num2 <- strsplit(gsub('[:space:][:space:]','',trimws(gsub('[^0-9]',' ',num1))),' ')[[1]]
			num2 <- strsplit(gsub('[ ]+',',',trimws(gsub('[^0-9]',' ',num1))),',')[[1]]
			reposts_count <- num2[1]
			comments_count <- num2[2]
			attitudes_count <- num2[3]
		## RT number
			rt_num1 <- html_text(html_node(hs,xpath = "//div[@class='WB_handle W_fr']"))
			rt_num2 <- strsplit(gsub('[ ]+',',',trimws(gsub('[^0-9]',' ',rt_num1))),',')[[1]]
			rt_reposts_count <- rt_num2[1]
			rt_comments_count <- rt_num2[2]
			rt_attitudes_count <- rt_num2[3]

			if (!is.na(retweeted_status)){
			      w_item <- data.frame(id=retweeted_status,user_id=retweeted_status_user_id,screen_name=rt_screen_name,
					retweeted_status_user_id=NA,retweeted_status=NA,
					created_at=rt_created_at,href=rt_href,text=rt_text,original_pic = img, in_reply_to_screen_name=NA,
				  rt_created_at=NA, rt_href=NA,rt_text=NA,
					reposts_count=rt_reposts_count,comments_count=rt_comments_count,attitudes_count=rt_attitudes_count,stringsAsFactors = F)

				all <- rbind(all,w_item)
			}
		}
	}
	return(all)
}

unfold_fn <- function(){
	unfold_l <- remDr$findElements("xpath","//a[@action-type='fl_unfold' and @class='WB_text_opt']")
#	unfold_l <- remDr$findElements("xpath","//a[contains(text(),'展开全文')]")
	if (length(unfold_l) != 0){
		for (i in 1:length(unfold_l)){
			unfold_l[[i]]$clickElement()
			Sys.sleep(2)
		}
	}
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
	tryCatch({
		url <- paste0('https://api.weibo.com/2/statuses/show.json?source=',sample(token,1),'&id=',as.character(id))
		r <- fromJSON(getURL(url,ssl.verifypeer=TRUE))
		if ("error_code" %in% names(r)){
			return(r$error_code)	
		} else {
			return(99999)  # Nothing returned
		}
#		if ( r$error_code == 20112){
	}, error = function(e){
		return(99999)  # runtime error
	})
}

InsertDB <- function(df){
  tona <- function(x){
    x[x == ""] <- NA
    return(x)
  }
  tryCatch({
  	con <- dbConnect(dbDriver("PostgreSQL"), user=DB_UNAME, dbname=DB_NAME, host=HOSTIP)
	on.exit(dbDisconnect(con))  
  	dbGetQuery(con, "set client_encoding to 'utf-8'")
  }, error = function(e) {
	Sys.sleep(60)
	print("Retrying InsertDB connection .....")
	tryCatch({  
  		con <- dbConnect(dbDriver("PostgreSQL"), user=DB_UNAME, dbname=DB_NAME, host=HOSTIP)
		on.exit(dbDisconnect(con))
  		dbGetQuery(con, "set client_encoding to 'utf-8'")
	}, error = function(e){
		Sys.sleep(60)
		print(e)
		return(NA)
	   }
	)
     }
  )
	
#  con <- dbConnect(dbDriver("PostgreSQL"), user=DB_UNAME, dbname=DB_NAME, host=HOSTIP)
#  dbGetQuery(con, "set client_encoding to 'utf-8'")
  dbinserted_time <- rep(as.character(Sys.time()),nrow(df))
  strSQL <- paste(
    'insert into rp_sinaweibo (id,user_id,screen_name,retweeted_status_user_id,retweeted_status,created_at,text,original_pic,in_reply_to_screen_name,reposts_count,comments_count,attitudes_count,dbinserted,deleted_last_seen,myip) values',
    paste(sprintf("(%s,%s,'%s',%s,%s,'%s','%s','%s','%s',%s,%s,%s,'%s','%s','%s')",df$id,tona(df$user_id),gsub("'","''",df$screen_name),df$retweeted_status_user_id,df$retweeted_status,df$created_at,gsub("'","''",df$text),df$original_pic,gsub("'","''",df$in_reply_to_screen_name),tona(df$reposts_count),tona(df$comments_count),tona(df$attitudes_count),dbinserted_time,dbinserted_time,myip), collapse=', '),
    'on conflict (id) do update set permission_denied = FALSE, deleted = NULL, deleted_last_seen = EXCLUDED.deleted_last_seen, reposts_count = EXCLUDED.reposts_count, comments_count = EXCLUDED.comments_count, attitudes_count = EXCLUDED.attitudes_count', sep = ' '
  )
  strSQL <- gsub(",NA,",",NULL,",strSQL)
  strSQL <- gsub("\\(NA,","\\(NULL,",strSQL)
  strSQL <- gsub(",NA\\)",",NULL\\)",strSQL)
  strSQL <- gsub(",NA,",",NULL,",strSQL)
  tryCatch({
	  dbSendQuery(con, strSQL)

  }, error = function(e) {
	  Sys.sleep(10)
	  print("Retrying dbsendquery ......")
	  dbSendQuery(con, strSQL)
     }
  )
#  dbSendQuery(con, strSQL)
#  dbDisconnect(con)
}

Set_PD <- function(id){
  tryCatch({
  	con <- dbConnect(dbDriver("PostgreSQL"), user=DB_UNAME, dbname=DB_NAME, host=HOSTIP)
	on.exit(dbDisconnect(con))  
  	dbGetQuery(con, "set client_encoding to 'utf-8'")
        censored_time <- as.character(Sys.time())
        strSQL <- paste0("update rp_sinaweibo set (permission_denied,deleted) = (TRUE,'",censored_time,"') where id = ",id)
        dbSendQuery(con, strSQL)
  }, error = function(e) {
	Sys.sleep(60)
	print("Retrying Set_PD connection .....")
	print(e)
	return(NA)
#	tryCatch({  
#  		con <- dbConnect(dbDriver("PostgreSQL"), user=DB_UNAME, dbname=DB_NAME, host=HOSTIP)
#		on.exit(dbDisconnect(con))
#  		dbGetQuery(con, "set client_encoding to 'utf-8'")
#	}, error = function(e){
#		Sys.sleep(60)
#		print(e)
#		return(NA)
#	   }
#	)
     }
  )
	
#  con <- dbConnect(dbDriver("PostgreSQL"), user=DB_UNAME, dbname=DB_NAME, host=HOSTIP)
#  dbGetQuery(con, "set client_encoding to 'utf-8'")
#  dbDisconnect(con)
}

Set_DP <- function(id){
  tryCatch({
  	con <- dbConnect(dbDriver("PostgreSQL"), user=DB_UNAME, dbname=DB_NAME, host=HOSTIP)
	on.exit(dbDisconnect(con))  
  	dbGetQuery(con, "set client_encoding to 'utf-8'")
        deleted_time <- as.character(Sys.time())
        strSQL <- paste0("update rp_sinaweibo set (permission_denied,deleted) = (NULL,'",deleted_time,"') where id = ",id)
        dbSendQuery(con, strSQL)
  }, error = function(e) {
	Sys.sleep(60)
	print("Retrying Set_DP connection .....")
	print(e)
	return(NA)
     }
  )
	
#  con <- dbConnect(dbDriver("PostgreSQL"), user=DB_UNAME, dbname=DB_NAME, host=HOSTIP)
#  dbGetQuery(con, "set client_encoding to 'utf-8'")
#  dbDisconnect(con)
}

Set_EC <- function(id,ecode){
  tryCatch({
  	con <- dbConnect(dbDriver("PostgreSQL"), user=DB_UNAME, dbname=DB_NAME, host=HOSTIP)
	on.exit(dbDisconnect(con))  
  	dbGetQuery(con, "set client_encoding to 'utf-8'")
        current_time <- as.character(Sys.time())
        strSQL <- paste0("update rp_sinaweibo set (ecode,deleted_last_seen) = (",ifelse(is.na(ecode),"NULL",paste0("'",ecode,"'")),",'",current_time,"') where id = ",id)
        dbSendQuery(con, strSQL)
  }, error = function(e) {
	Sys.sleep(60)
	print("Retrying Set_EC connection .....")
	print(e)
	return(NA)
     }
  )
#  con <- dbConnect(dbDriver("PostgreSQL"), user=DB_UNAME, dbname=DB_NAME, host=HOSTIP)
#  dbGetQuery(con, "set client_encoding to 'utf-8'")
#  dbDisconnect(con)
}

chk_missing <- function(x){
	if (length(x) == 1){
		return(FALSE)
	}
	r <- rep(F,length(x))
	dx <- diff(x)
	begin <- length(dx) + 1
	for (i in 1:length(dx)){
		if (dx[i] == -1){
			begin <- i
			next
		} else if (dx[i] == 1){
			if (begin < i){
				for (j in (begin+1):i){
					r[j] <- TRUE
				}
			}
		}
	}
	return(r)
}

Send_alert <- function(e,Scap){
  tryCatch({
	  smtp <- server(host = "smtp.gmail.com",
			 port = 587,  # previously 465
			 username = Sender_username,
			 password = Sender_password,
			 reuse = FALSE)

	  Email_msg <- paste0(Sys.info()[[4]],":",e)
	  Email_subject <- paste0("Error alert from Weiboscope 3.0 - ",myip,"-",myvm)
	  html_body <- '<html><body><img src="cid:image"></body></html>'

	  email <- envelope(
	    to = Sender_username,
	    from = Sender_username,
	    subject = Email_subject,
	    text = Email_msg,
	    html = html_body
	  )

  	email <- attachment(email, path = Scap, cid = "image")
  
  	smtp(email, verbose = TRUE)
	  
	return(TRUE)
  }, error = function(e){
	return(FALSE)
  })
}

Send_alert_blacktea <- function(e,Scap){
  tryCatch({
    smtp <- server(host = "147.8.144.20",
                   port = 25,
                   username = New_Sender_username,
                   password = New_Sender_password,
                   reuse = FALSE)
    
    Email_msg <- paste0(Sys.info()[[4]],":",e)
    Email_subject <- paste0("Error alert from Weiboscope 3.0 - ",Sys.info()[[4]])
    html_body <- '<html><body><img src="cid:image"></body></html>'
    
    if (remDr$getCurrentUrl()[[1]] == "https://weibo.com/login.php"){
	    Email_subject <- paste0("LOGIN REQUIRED !! " ,Email_subject)
    }
	  
    email <- envelope(
      to = Receiver_username,
      from = New_Sender_username,
      subject = Email_subject,
      text = Email_msg,
      html = html_body
    )
    
    email <- attachment(email, path = Scap, cid = "image")
    
    smtp(email, verbose = TRUE)
    
    return(TRUE)
    
  }, error = function(e){
    return(FALSE)
  })
}

Scrolling4Posts <- function(){
		z <- NULL
		start_time <- Sys.time()
		d_time <- 0
		duration <- 300
		while (is.null(z) & (d_time < duration)){
			webElem <- remDr$findElement("css", "body")
			webElem$sendKeysToElement(list("\uE010"))
			Sys.sleep(20)
			z <- tryCatch({
				suppressMessages({
					c <- remDr$findElement("xpath","//span[@class='more_txt W_f14'] | //a[contains(text(),'点击重新载入')]")
				})
			},error=function(e){
				return(NULL)
			})
			d_time <- difftime(Sys.time(),start_time,units = "secs")
		}
		if (!is.null(z)){
			remDr$executeScript("arguments[0].click();", args=list(c))
#			c$clickElement()
		}
#		} else {
#			remDr$navigate("https://weibo.com")
#			Sys.sleep(30)
#			click <- remDr$findElement(using = "xpath","//a[@bpfilter='main']")
#			click$clickElement()
#			Sys.sleep(30)	
#		}
}

#remDr$navigate("https://weibo.com/login.php")

#cont <- readline("Press Return to continue \n")

find_version <- function(){
  v <- str_extract(list.dirs("C:/Program Files/Google/Chrome/Application", recursive = FALSE),'[0-9]*\\..*[0-9]$')
  v <- trimws(v[!is.na(v)])
  if (length(v)==0){
    return(NA)
  } else if (length(v)==1){
    return(v)
  } else {
    return(sort(v)[length(v)])
  }
}

get_chromedriver_version <- function(){
  trimws(str_extract(system(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop","Selenium"),"/chromedriver --version"),intern=TRUE),"[0-9]*\\.[\\.0-9]+"))
}

check_tt <- function(uid,cl){
	p <- 1
	max_page <- (cl %/% 10) + 1
	if (max_page > 15){
		max_page <- 15
	}	
	rs_data <- c()
	while(p<=max_page){
		url <- paste0("https://m.weibo.cn/api/container/getIndex?type=uid&value=",uid,"&containerid=107603",uid,"&page=",p)
		g <- RJSONIO::fromJSON(url,flatten=TRUE)
		Sys.sleep(0.25)
		if(g$ok == 1){
			d <- sapply(g$data$cards,function(x) {x$mblog$id})
			rs_data <- c(rs_data,d)
			p <- p + 1
		} else {
			break
		}
	}
	closeAllConnections()
	return(rs_data)
}

download_latest <- function(){
		browser_v <- strsplit(find_version(),'\\.')[[1]][1]
		cdriver_v <- strsplit(get_chromedriver_version(),'\\.')[[1]][1]
		if (browser_v != cdriver_v){
			latest_release <- getURL("https://chromedriver.storage.googleapis.com/LATEST_RELEASE")
			download.file(paste0("https://chromedriver.storage.googleapis.com/",latest_release,"/chromedriver_win32.zip"),paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop","Selenium"),"/chromedriver_win32.zip"))
			unzip(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop","Selenium"),"/chromedriver_win32.zip"),exdir=file.path(Sys.getenv("USERPROFILE"),"Desktop","Selenium"))
		}	
}

clearCache <- function(){
	remDr$navigate("chrome://settings/clearBrowserData")
	Sys.sleep(1)
	for (i in 1:12){
		remDr$findElement("xpath","//settings-ui")$sendKeysToElement(list("\uE004"))
		Sys.sleep(1)
	}
	Sys.sleep(3)
	remDr$buttondown()
	Sys.sleep(3)
	remDr$findElement("xpath","//settings-ui")$sendKeysToElement(list("\uE007"))
}

myswitch <- function(windowId){
  qpath <- sprintf("%s/session/%s/window", remDr$serverURL, remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowId))
}

Close_all_tabs <- function(){
	all_h <- remDr$getWindowHandles()
	if (length(all_h) > 1){
		for (i in 2:length(all_h)){
			myswitch(all_h[[i]])
			remDr$closeWindow()
#		remDr$navigate("https://www.hku.hk")
#		remDr$getCurrentWindowHandle()
		}
		myswitch(all_h[[1]])
	}
}

while (1) {
	## Sleep between 2-6am
	current_hr <- as.integer(format(Sys.time(),"%H"))
	if (current_hr >= 2 & current_hr <= 6){
		Sys.sleep(600)
		next
	}
	## Update source code at 7am everyday
	current_date <- format(Sys.time(),"%Y-%m-%d")
	if ((current_date != start_date) && (format(Sys.time(),"%H") == "07")) {
		for (x in 1:100){
			print(paste0("Code updated at ",current_date,":",Sys.time()))
		}
#		if (find_version() != get_chromedriver_version()){
#			latest_release <- getURL("https://chromedriver.storage.googleapis.com/LATEST_RELEASE")
#			download.file(paste0("https://chromedriver.storage.googleapis.com/",latest_release,"/chromedriver_win32.zip"),paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop","Selenium"),"/chromedriver_win32.zip"))
#			unzip(paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop","Selenium"),"/chromedriver_win32.zip"),exdir=file.path(Sys.getenv("USERPROFILE"),"Desktop","Selenium"))
#		}
		start_v <- 2
 		source("run_weiboscope.R")
	}
# Remove all db connections
	removedb <- lapply(dbListConnections(drv = dbDriver("PostgreSQL")), function(x) {dbDisconnect(conn = x)})
	closeAllConnections()
	
#while (Sys.time() < strptime("2021-02-12 10:00:00","%Y-%m-%d %H:%M:%S")) {
	tryCatch({
#		remDr$refresh()
		remDr$navigate("https://weibo.com")
		Sys.sleep(30)
		click <- remDr$findElement(using = "xpath","//a[@bpfilter='main']")
		click$clickElement()
		
		### Randomization of sleeping time and looping times		
		scrolling_times <- sample(3:5,1)
		i <- 0
		while (i < scrolling_times){
			Sys.sleep(abs(rnorm(1,60,20)))
			print(paste0("Scrolling loop: ",i,"/",scrolling_times))
			Scrolling4Posts()
			i <- i + 1
			### Random liking - 25%
			if (sample(1:4,1) == 2){
				all_likes <- remDr$findElements("xpath","//span[@node-type='like_status']")
				if (length(all_likes) != 0){
					all_likes[[sample(length(all_likes),1)]]$clickElement()
					print("Liking .......")
				}
			}
		}
		
#		Sys.sleep(40)
#		Scrolling4Posts()
#		Sys.sleep(65)
#		Scrolling4Posts()
#		Sys.sleep(40)
#		if (format(Sys.time(),"%H") > 2 && format(Sys.time(),"%H") < 5){
#		Scrolling4Posts() 
#		Scrolling4Posts()
#		Scrolling4Posts()
#		}
		
#		webElem <- remDr$findElement("css", "body")
#		webElem$sendKeysToElement(list("\uE010"))
#		Sys.sleep(30)
#		webElem <- remDr$findElement("css", "body")
#		webElem$sendKeysToElement(list("\uE010"))
#		Sys.sleep(30)
#		webElem <- remDr$findElement("css", "body")
#		webElem$sendKeysToElement(list("\uE010"))
#		Sys.sleep(30)
#		webElem <- remDr$findElement("css", "body")
#		webElem$sendKeysToElement(list("\uE010"))
#		Sys.sleep(30)
#		webElem <- remDr$findElement("css", "body")
#		webElem$sendKeysToElement(list("\uE010"))
#		Sys.sleep(30)
#		webElem <- remDr$findElement("css", "body")
#		webElem$sendKeysToElement(list("\uE010"))
#		Sys.sleep(30)
#		webElem <- remDr$findElement("css", "body")
#		webElem$sendKeysToElement(list("\uE010"))
#		Sys.sleep(30)
#		webElem <- remDr$findElement("css", "body")
#		webElem$sendKeysToElement(list("\uE010"))
#		Sys.sleep(5)
		unfold_fn()   # unfold all shortened texts
		whole_body <- remDr$findElement(using = "xpath","//body")
		text_html <- whole_body$getElementAttribute("innerHTML")[[1]]
		wb_df <- parse_wb_rds(text_html)
		### Added for retweeted weibos
		rt_wb_df <- rt_parse_wb_rds(text_html)
		wb_df <- rbind(wb_df,rt_wb_df)
		wb_df <- wb_df[!duplicated(wb_df$id),]
		wb_df <- wb_df[wb_df$user_id != "",] 
		wb_df <- wb_df[!is.na(wb_df$user_id),] 
		wb_df <- wb_df[nchar(wb_df$id) <= 16,]
		wb_df <- wb_df[nchar(wb_df$retweeted_status) <= 16 | is.na(wb_df$retweeted_status),]
		if (nrow(wb_df)!=0){
			InsertDB(wb_df)
		}
		if (nrow(all)==0){
			all <- wb_df
		} else {
			all <- rbind(all,wb_df[!(wb_df$id %in% all$id),])
		}
	}, error = function(e){
		print(e)
		print("First block error")
		Sys.sleep(300)
		wb_df <- data.frame()
		x <- as.character(e)
		remDr$screenshot(file = "scapture_file.jpg")
  		if (!Send_alert(x,"scapture_file.jpg")){
			print("Send email error")
		}
  		start_v <- 2
  		source("run_weiboscope.R")
	})
	
#	need_to_chk <- unique(all[all$created_at < (Sys.time() - (checking_time*60*60*2/3)),]$user_id)  ### checking only after 16 hour 
	threshold_chk <- 5
	need_to_chk <- unique(all$user_id[order(all$created_at)][1:ifelse(nrow(all)<threshold_chk,nrow(all),threshold_chk)])
	print(paste0("Need to check: ",length(need_to_chk)," [",Sys.time(),"]"))
	
	for (u in need_to_chk){
		u_posts <- c()
#		u_posts <- sort(unique(as.character(wb_df$id[wb_df$user_id == u])))
		ref  <- sort(unique(as.character(all$id[all$user_id == u])))
		if (length(ref) != 0){
			if (length(u_posts) == 0){
				r_missing <- rep(T,length(ref))
			} else {
				r_missing <- chk_missing(ref %in% u_posts)
			}
		} else {
			print("Not found user in ref")
			next
		}
		if (sum(r_missing) > 0){
			c_ref <- ref[r_missing]
			#### Check the existence of the "disappeared" posts from the user timeline
			tryCatch({
###
### New check via new API
###
#				remDr$navigate(paste0("https://weibo.com/u/",u,"?is_all=1"))
#				Sys.sleep(5)
#				webElem <- remDr$findElement("css", "body")
#				webElem$sendKeysToElement(list("\uE010"))
#				Sys.sleep(2)
#				webElem <- remDr$findElement("css", "body")
#				webElem$sendKeysToElement(list("\uE010"))
#				Sys.sleep(2)
#				u_id <- remDr$findElements("xpath", "//div[@class='WB_from S_txt2']//a[@name]")
				u_id <- check_tt(u,length(c_ref))
				if (length(u_id) == 0) {  ### Retry if no data
#					remDr$refresh()
					Sys.sleep(5)
					u_id <- check_tt(u,length(c_ref))
#					u_id <- remDr$findElements("xpath", "//div[@class='WB_from S_txt2']//a[@name]")
					if (length(u_id) == 0) {
						c_ref <- c()
					}
				} else {
#					chk_again <- sort(sapply(1:length(u_id), function(i){
#						u_id[[i]]$getElementAttribute("name")[[1]]
#					}))
#					c <- c_ref[!(c_ref %in% chk_again)]
					c_ref <- c_ref[!(c_ref %in% u_id)]				
				}
			}, error = function(e){
				print(paste0("Can't reach https://weibo.com/u/",u))
				c_ref <- c()
			})
			####
			if (length(c_ref) != 0){
				print(paste0("Links to be checked (",u,") :",length(c_ref)))
				for (cc in c_ref){
					if (!(cc %in% censored)){
#						print(paste0("Not found via link: ",cc))
						chk_result <- check_censored(cc)
						Set_EC(cc,chk_result)
						if (!is.na(chk_result)){
							if (chk_result == 20112){
								print(paste0("Permission denied: ",cc))
								censored <- c(censored,cc)
								Set_PD(cc)
							} else if (chk_result == 20101){
								print(paste0("target weibo does not exist: ",cc))
								Set_DP(cc)
							} 
						}	
					}
				}
			}
		}
	}
	tryCatch({	 
#		remDr$refresh()
		remDr$navigate("https://weibo.com")
		Sys.sleep(10)
		click <- remDr$findElement(using = "xpath","//a[@bpfilter='main']")
		click$clickElement()
		Sys.sleep(10)
		all <- all[all$created_at >= (Sys.time() - (checking_time*60*60)),]  # checking the past "checking_time" hours
		saveRDS(all,"all.rds")
		Close_all_tabs()
	}, error = function(e){
		print(e)
		print("Second block error")
		Sys.sleep(300)
		x <- as.character(e)
  		remDr$screenshot(file = "scapture_file.jpg")
  		if (!Send_alert(x,"scapture_file.jpg")){
			print("Send email error")
		}
  		start_v <- 2
  		source("run_weiboscope.R")
	})
}

#InsertDB(tm_wb)
#cookies <- remDr$getAllCookies()
#saveRDS(cookies, "cookies.rds")

#cookies <- readRDS("cookies.rds")
#for (i in 1:length(cookies)) {
#  remDr$addCookie(name = cookies[[i]][["name"]], value = #cookies[[i]][["value"]])
#}
