killall chrome
killall chromedriver
killall java
/home/fukingwa/Weibo/chrome.sh
read -n 1 -s
/home/fukingwa/Weibo/run_java.sh
read -n 1 -s
R -e "source('/home/fukingwa/Weibo/run_weiboscope.R')"
