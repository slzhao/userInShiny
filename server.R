# TODO: Add comment
# 
# Author: zhaos
###############################################################################


# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output,session) {
			source("www/Login.R",  local = TRUE)
			changePasswordButtonCount<-0
			delUserButtonCount<-0
			moveUserButtonCount<-0
			addUserButtonCount<-0
			editEmailButtonCount<-0
			
			changePasswordFun<-reactive({
						if (!is.null(input$changePasswordButton) && input$changePasswordButton==changePasswordButtonCount) {
							return(NULL)
						} else {
							userName<-isolate(input$userName)
							oldPassword<-isolate(input$oldPassword)
							newPassword1<-isolate(input$newPassword1)
							newPassword2<-isolate(input$newPassword2)
							userInd<-which(userData$user==userName)
							
							if (newPassword1!=newPassword2) {
								changePasswordButtonCount<<-input$changePasswordButton
								return("The new passwords should be same in two inputs.")
							}
							
							if (length(userInd)>0) {
								password<-userData$password[userInd]
								if (oldPassword==password) {
									userData$password[userInd]<<-newPassword1
									GLOBALDATA$userData<-userData
									write.csv(GLOBALDATA$userData,userFile,row.names=F)
									changePasswordButtonCount<<-input$changePasswordButton
									return("The password has been changed and saved.")
								} else {
									changePasswordButtonCount<<-input$changePasswordButton
									return("The old password doesn't match the password in database.")
								}
							}
						}
					})
			
			editEmailFun<-reactive({
						if (!is.null(input$editEmailButton) && input$editEmailButton==editEmailButtonCount && !is.null(input$sendEmailOrNot)) {
							return(NULL)
						} else {
							emailsDraft<-isolate(input$emails)
							
							emailList<<-strsplit(emailsDraft,";")[[1]]
							sendEmailSign<<-input$sendEmailOrNot
							writeLines(emailList,emailFile)
							editEmailButtonCount<<-input$editEmailButton
							return(paste0("The emails have been updated."))
						}
					})
			
			delUserFun<-reactive({
						if (!is.null(input$delUserButton) && input$delUserButton==delUserButtonCount) {
							return(NULL)
						} else {
							userName<-isolate(input$delUserName)
							userInd<-which(userData$user==userName)
							if (length(userInd)>0) {
								userData<<-userData[-userInd,]
								GLOBALDATA$userData<-userData
								write.csv(GLOBALDATA$userData,userFile,row.names=F)
							}
							delUserButtonCount<<-input$delUserButton
							return(paste0("User '",userName,"' has been deleted"))
						}
					})
			moveUserFun<-reactive({
						if (!is.null(input$moveUserButton) && input$moveUserButton==moveUserButtonCount) {
							return(NULL)
						} else {
							userName<-isolate(input$moveUserName)
							userInd<-which(userData$user==userName)
							if (length(userInd)>0) {
								userGroup<-isolate(input$moveUserGroup)
								userNote<-isolate(input$moveUserNote)
								if (userNote=="") {
									userNote<-userData$note[userInd]
								}
								temp<-c(userName,userGroup,userNote)
								userData[userInd,-2]<<-temp
								GLOBALDATA$userData<-userData
								write.csv(GLOBALDATA$userData,userFile,row.names=F)
								moveUserButtonCount<<-input$moveUserButton
								return(paste0("User '",userName,"' has been moved to '",userGroup,"' group"))
							}
							return(NULL)
						}
					})
			addUserFun<-reactive({
						if (!is.null(input$addUserButton) && input$addUserButton==addUserButtonCount) {
							return(NULL)
						} else {
							userName<-isolate(input$addUserName)
							userGroup<-isolate(input$addUserGroup)
							userPass1<-isolate(input$addUserPass1)
							userPass2<-isolate(input$addUserPass2)
							
							if (userPass1!=userPass2) {
								addUserButtonCount<<-input$addUserButton
								return("The passwords for added user should be same in two inputs.")
							}
							
							temp2<-c(userName,userPass1,userGroup,isolate(input$addUserNote))
							userData<<-rbind(userData,temp2)
							GLOBALDATA$userData<<-userData
							write.csv(GLOBALDATA$userData,userFile,row.names=F)
							addUserButtonCount<<-input$addUserButton
							return(paste0("User '",userName,"' has been added to '",userGroup,"' group"))
						}
					})
			
			observe({

						
						if (USER$Logged == TRUE) {
							output$tabSets <- renderUI({
										Tab1<-tabPanel("Tab1",uiOutput("tab1"))
										Tab2<-tabPanel("Tab2",uiOutput("tab2"))
										changePasswordTab<-tabPanel("Change Password",uiOutput("changePassword"))
										userAdminTab<-tabPanel("User Management",uiOutput("userAdmin"))
										emailAdminTab<-tabPanel("Email Management",uiOutput("emailAdmin"))
										
										if (USER$Group == "admin") {
											tabsetPanel(Tab1,Tab2,changePasswordTab,
													userAdminTab,emailAdminTab
											)
										} else {
											tabsetPanel(Tab1,changePasswordTab
											)
										}
									})
						}
						output$userAdmin <- renderUI({
								list(
									h4("Users list:"),
									htmlOutput("usersTableDisplay"),
									br(),
									h4("User Management:"),
									tags$div(class = "row-fluid",
										tags$div(class = "span6", uiOutput("usersAdd")),
										tags$div(class = "span6", uiOutput("usersMove")),
										tags$div(class = "span6", uiOutput("usersDel"))
										)
									)
								})
						output$emailAdmin <- renderUI({									
									list(
											uiOutput("editEmails")
									)
								})
						
						output$changePassword <- renderUI({
									wellPanel(
											passwdInput("oldPassword", "Input Old Password:"),
											passwdInput("newPassword1", "Input New Password:"),
											passwdInput("newPassword2", "Confirm New Password:"),
											br(),
											actionButton('changePasswordButton','Change Password'),
											br(),
											h4(textOutput("changePaawordResult"))
									)
								})
						
						output$editEmails <- renderUI({
									wellPanel(
											h4("Edit Emails"),
											checkboxInput(inputId="sendEmailOrNot", label="Send email", value = sendEmailSign),
											textInput(inputId = "emails",label = "Emails:(email1;email2;email3;...)",value=paste0(emailList,collapse=";")),
											tags$head(tags$style(type="text/css", "#emails {width: 800px}")),
											br(),
											actionButton('editEmailButton','Update Email'),
											br(),
											h4(textOutput("editEmailResult"))
									)
								})
						
						output$usersAdd <- renderUI({
										wellPanel(
												h4("Add User"),
												textInput(inputId = "addUserName",label = "Input New User Name:"),
												passwdInput("addUserPass1", "Input New User Password:"),
												passwdInput("addUserPass2", "Confirm New User Password:"),
												selectInput("addUserGroup", "Select New User Group:",list("user"="user","admin"="admin")),
												textInput(inputId = "addUserNote",label = "Note:"),
												br(),
												actionButton('addUserButton','Add User'),
												br(),
												h4(textOutput("addUserResult"))
										)
									#}
								})
						output$usersMove <- renderUI({
										wellPanel(
												h4("Move User"),
												selectInput("moveUserName","Select User Name to Move:",sapply(userData[,1],function(x) x,simplify=F)),
												selectInput("moveUserGroup", "Select User Group to Move:",list("user"="user","admin"="admin")),
												textInput(inputId = "moveUserNote",label = "Note:",value=""),
												br(),
												actionButton('moveUserButton','Move User'),
												br(),
												h4(textOutput("moveUserResult"))
										)
									#}
								})
						output$usersDel <- renderUI({
										wellPanel(
												h4("Delete User"),
												selectInput("delUserName","Select User Name to Delete:",sapply(userData[,1],function(x) x,simplify=F)),
												br(),
												actionButton('delUserButton','Delete User'),
												br(),
												h4(textOutput("delUserResult"))
										)
								})
					})
					
					output$changePaawordResult <- renderText({
								changePasswordFun()
							})
					output$delUserResult <- renderText({
								delUserFun()
							})
					output$moveUserResult <- renderText({
								moveUserFun()
							})
					output$addUserResult <- renderText({
								addUserFun()
							})
					output$editEmailResult <- renderText({
								editEmailFun()
							})
					
					output$usersTableDisplay <- renderGvis({
								gvisTable(GLOBALDATA$userData[,-2])        
							})
		})
