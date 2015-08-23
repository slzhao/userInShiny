# TODO: Add comment
# 
# Author: zhaos
###############################################################################

library(shiny)
library(googleVis)
library(mailR)

#Texts
WEBTITLE="User in Shiny"

#Files
userFile<-"data/user.csv"
emailFile<-"data/email.txt"


Logged = FALSE
Group=""
userData<-read.csv(userFile,header=T,as.is=T)
GLOBALDATA <- reactiveValues(userData=userData)


#Emails
emailList<-readLines(emailFile)
sendEmailSign<-TRUE

SMTP_FROM="test <test@gmail.edu>"
SMTP_SETTINGS = list(host.name='mailtrap.io',
		port='2525',
		user.name='YourUserName',
		passwd='YourPassword')
SMTP_AUTHENTICATE=TRUE

sendEmail<-function(subject="New Assignment",body="New Assignment Content",to=emailList) {
	send.mail(from = SMTP_FROM,
			to = to,
			subject = subject,
			body = body,
			smtp = SMTP_SETTINGS,
			authenticate = SMTP_AUTHENTICATE,
			send = TRUE)
}

