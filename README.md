# weiboscope3.0

This project aims to make Weiboscope (codename Weiboscope 3.0) become decentralized, distributed, and resilient. Anyone in anywhere can build site(s) to collect Weibo data.   

Steps to setup weiboscope3.0:

1) Install R

https://www.r-project.org/

2) Install the following R packages

Rscript -e 'install.packages(c("RSelenium","stringr","RPostgreSQL","rvest","RCurl","RJSONIO","emayili"), dependencies=T, repos="http://cloud.r-project.org")'

3) Install google-chrome and download chromedriver from the source https://chromedriver.chromium.org/downloads. Make sure both have same versions, i.e. say both version 88.

4) Install Java OpenJDK, download selenium standalone server from https://www.selenium.dev/downloads/, and Run 

java -Dwebdriver.chrome.driver=[PATH TO]chromedriver -jar [PATH to]selenium-server-standalone-3.141.59.jar -sessionTimeout 57868143

5) Setup .pgpass in the home directory, in which your PostgreSQL database username and password are stored

6) Create a new directory and put the envirnoment R image file WB.RData in there. The R image containss four variables:
DB_NAME = database name
DB_UNAME = database user name
HOSTIP = IP address of the database
token = a array of tokens used to check "permission_denied"

7) Run R in the command line/desktop and execute in the console: source("https://raw.githubusercontent.com/fukingwa/weiboscope3.0/main/weiboscope_scraper.R")

8) Login Weibo (need smartphone to scan QR code) in the pop window when you are asked. Press  any key to continue 

9) If the program is borken and stopped to the console but the window is still up, Follow Step 7 to execute the command and select 2) jump start to resume



