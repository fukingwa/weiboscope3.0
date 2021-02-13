# weiboscope3.0

Steps to setup weiboscope3.0:

1) Install r-base and r-base-deve

sudo apt-get install r-base r-base-dev or sudo yum install r-base r-base-dev

2) Install the following R packages

Rscript -e "install.packages(c("RSelenium","stringr","RPostgreSQL","rvest","RCurl","RJSONIO"),dependencies=T)"

3) Install google-chrome and download chromedriver from the source https://chromedriver.chromium.org/downloads. Make sure both have different versions.

4) Install java and download selenium standalone server from https://www.selenium.dev/downloads/

java -Dwebdriver.chrome.driver=[PATH TO]chromedriver -jar [PATH to]selenium-server-standalone-3.141.59.jar -sessionTimeout 57868143

5) Setup .pgpass in the home directory, in which your psql username and password are stored

6) Create a new directory and put the envirnoment R image file WB.RData in there

7) Run R and Run the command: source("https://raw.githubusercontent.com/fukingwa/weiboscope3.0/main/weiboscope_scraper.R")

8) Login Weibo (need smartphone to scan QR code) when you are asked. Click any key to continue 

9) If the program is broken but the window is still up, Run Step 6 and select 2) jump start to resume



