require(RSelenium)
require(stringr)
require(RPostgreSQL)
require(rvest)
require("RCurl")
require("RJSONIO")
require("emayili")

myip <- getURL("ifconfig.me/ip")

if (myip == ""){
	myip <- getURL("api.ipify.org")	
}

if (myip == ""){
	myip <- "0.0.0.0"	
}

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

	eCaps <- list(chromeOptions = list(
		args = c('start-maximized','enable-automation','--disable-gpu', '--no-sandbox','--disable-extensions','--dns-prefetch-disable','--disable-infobars','--disable-dev-shm-usage','--disable-browser-side-navigation')
	#	prefs = list("detach" = TRUE),
	#	detach = TRUE
	))

	remDr <- remoteDriver(
	  remoteServerAddr = "localhost",
	  port = 4444,
	  browserName = "chrome", extraCapabilities = eCaps)

	q <- remDr$open()
	q <- remDr$maxWindowSize()
	q <- remDr$setWindowSize(1920, 1080)
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

print(myip)

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
				retweeted_status_user_id <- as.character(strsplit(rt_href,"/|\\?")[[1]][2])
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
  con <- dbConnect(dbDriver("PostgreSQL"), user=DB_UNAME, dbname=DB_NAME, host=HOSTIP)
  dbGetQuery(con, "set client_encoding to 'utf-8'")
  dbinserted_time <- rep(as.character(Sys.time()),nrow(df))
  strSQL <- paste(
    'insert into rp_sinaweibo (id,user_id,screen_name,retweeted_status_user_id,retweeted_status,created_at,text,original_pic,in_reply_to_screen_name,reposts_count,comments_count,attitudes_count,dbinserted,myip) values',
    paste(sprintf("(%s,%s,'%s',%s,%s,'%s','%s','%s','%s',%s,%s,%s,'%s','%s')",df$id,tona(df$user_id),gsub("'","''",df$screen_name),df$retweeted_status_user_id,df$retweeted_status,df$created_at,gsub("'","''",df$text),df$original_pic,gsub("'","''",df$in_reply_to_screen_name),tona(df$reposts_count),tona(df$comments_count),tona(df$attitudes_count),dbinserted_time,myip), collapse=', '),
    'on conflict (id) do update set dbinserted = EXCLUDED.dbinserted, reposts_count = EXCLUDED.reposts_count, comments_count = EXCLUDED.comments_count, attitudes_count = EXCLUDED.attitudes_count', sep = ' '
  )
  strSQL <- gsub(",NA,",",NULL,",strSQL)
  strSQL <- gsub("\\(NA,","\\(NULL,",strSQL)
  strSQL <- gsub(",NA\\)",",NULL\\)",strSQL)
  strSQL <- gsub(",NA,",",NULL,",strSQL)
  dbSendQuery(con, strSQL)
  dbDisconnect(con)
}

Set_PD <- function(id){
  con <- dbConnect(dbDriver("PostgreSQL"), user=DB_UNAME, dbname=DB_NAME, host=HOSTIP)
  dbGetQuery(con, "set client_encoding to 'utf-8'")
  censored_time <- as.character(Sys.time())
  strSQL <- paste0("update rp_sinaweibo set (permission_denied,deleted) = (TRUE,'",censored_time,"') where id = ",id)
  dbSendQuery(con, strSQL)
  dbDisconnect(con)
}

Set_DP <- function(id){
  con <- dbConnect(dbDriver("PostgreSQL"), user=DB_UNAME, dbname=DB_NAME, host=HOSTIP)
  dbGetQuery(con, "set client_encoding to 'utf-8'")
  deleted_time <- as.character(Sys.time())
  strSQL <- paste0("update rp_sinaweibo set (permission_denied,deleted) = (NULL,'",deleted_time,"') where id = ",id)
  dbSendQuery(con, strSQL)
  dbDisconnect(con)
}

Set_EC <- function(id,ecode){
  con <- dbConnect(dbDriver("PostgreSQL"), user=DB_UNAME, dbname=DB_NAME, host=HOSTIP)
  dbGetQuery(con, "set client_encoding to 'utf-8'")
  current_time <- as.character(Sys.time())
  strSQL <- paste0("update rp_sinaweibo set (ecode,dbinserted) = (",ifelse(is.na(ecode),"NULL",paste0("'",ecode,"'")),",'",current_time,"') where id = ",id)
  dbSendQuery(con, strSQL)
  dbDisconnect(con)
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
			 port = 465,
			 username = Sender_username,
			 password = Sender_password,
			 reuse = FALSE)

	  Email_msg <- paste0(Sys.info()[[4]],":",e)
	  Email_subject <- paste0("Error alert from Weiboscope 3.0 - ",myip)
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
		while (is.null(z) & (d_time < 180)){
			webElem <- remDr$findElement("css", "body")
			webElem$sendKeysToElement(list("\uE010"))
			Sys.sleep(20)
			z <- tryCatch({c <- remDr$findElement("xpath","//span[@class='more_txt W_f14'] | //a[contains(text(),'点击重新载入')]")},error=function(e){return(NULL)})
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

while (1) {
#while (Sys.time() < strptime("2021-02-12 10:00:00","%Y-%m-%d %H:%M:%S")) {
	tryCatch({
#		remDr$refresh()
		remDr$navigate("https://weibo.com")
		Sys.sleep(30)
		click <- remDr$findElement(using = "xpath","//a[@bpfilter='main']")
		click$clickElement()
		Sys.sleep(30)
		Scrolling4Posts() 
		Scrolling4Posts()
		Scrolling4Posts()
		if (format(Sys.time(),"%H") > 2 && format(Sys.time(),"%H") < 5){
			Scrolling4Posts() 
			Scrolling4Posts()
			Scrolling4Posts()
		}
		
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
		Sys.sleep(30)
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
  		source("https://raw.githubusercontent.com/fukingwa/weiboscope3.0/main/weiboscope_scraper.R")
	})
	for (u in unique(wb_df$user_id)){
		u_posts <- sort(unique(as.character(wb_df$id[wb_df$user_id == u])))
		ref  <- sort(unique(as.character(all$id[all$user_id == u])))
		if (length(ref) != 0){
			r_missing <- chk_missing(ref %in% u_posts)
		} else {
			print("Not found user in ref")
			next
		}
		if (sum(r_missing) > 0){
			c <- ref[r_missing]
			#### Check the existence of the "disappeared" posts from the user timeline
			tryCatch({
				remDr$navigate(paste0("https://weibo.com/u/",u))
				Sys.sleep(5)
				webElem <- remDr$findElement("css", "body")
				webElem$sendKeysToElement(list("\uE010"))
				Sys.sleep(2)
				webElem <- remDr$findElement("css", "body")
				webElem$sendKeysToElement(list("\uE010"))
				Sys.sleep(2)
				u_id <- remDr$findElements("xpath", "//div[@class='WB_from S_txt2']//a[@name]")
				if (length(u_id) == 0) {  ### Retry if no data
					remDr$refresh()
					Sys.sleep(5)
					u_id <- remDr$findElements("xpath", "//div[@class='WB_from S_txt2']//a[@name]")
					if (length(u_id) == 0) {
						c <- c()
					}
				} else {
					chk_again <- sort(sapply(1:length(u_id), function(i){
						u_id[[i]]$getElementAttribute("name")[[1]]
					}))
					c <- c[!(c %in% chk_again)]
				}
			}, error = function(e){
				print(paste0("Can't reach https://weibo.com/u/",u))
				c <- c()
			})
			####
			if (length(c) != 0){
				for (cc in c){
					if (!(cc %in% censored)){
						print(paste0("Not found via link: ",cc))
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
		Sys.sleep(30)
		click <- remDr$findElement(using = "xpath","//a[@bpfilter='main']")
		click$clickElement()
		Sys.sleep(60)
		all <- all[all$created_at >= (Sys.time() - (12*60*60)),]  # Twelve hours
		saveRDS(all,"all.rds")
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
  		source("https://raw.githubusercontent.com/fukingwa/weiboscope3.0/main/weiboscope_scraper.R")
	})
}

#InsertDB(tm_wb)
#cookies <- remDr$getAllCookies()
#saveRDS(cookies, "cookies.rds")

#cookies <- readRDS("cookies.rds")
#for (i in 1:length(cookies)) {
#  remDr$addCookie(name = cookies[[i]][["name"]], value = #cookies[[i]][["value"]])
#}
