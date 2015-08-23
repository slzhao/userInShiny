#### Log in module ###
USER <- reactiveValues(Logged = Logged,Group=Group)

passwdInput <- function(inputId, label) {
  tagList(
    tags$label(label),
    tags$input(id = inputId, type="password", value="")
  )
}

output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {
	return(
			list(
					br(),
					wellPanel(
							textInput("userName", "User Name:"),
							passwdInput("passwd", "Password:"),
							br(),
							actionButton("Login", "Log in")
					)
					)
			)
  }
})

output$pass <- renderText({  
  if (USER$Logged == FALSE) {
    if (!is.null(input$Login)) {
   if (input$Login > 0) {
      Username <- isolate(input$userName)
      Password <- isolate(input$passwd)
	usernameInd <- which(GLOBALDATA$userData$user == Username)
	  if (length(usernameInd) > 0) {
		  if (GLOBALDATA$userData$password[usernameInd[1]]==Password) {
			  USER$Logged <- TRUE
			  USER$Group <- GLOBALDATA$userData$group[usernameInd[1]]
		  }
		  else  {
			  "Password failed!"
		  }
	  } else  {
		  "User name doesn't exist!"
	  }
    } 
    }
  }
})

