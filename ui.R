

shinyUI(bootstrapPage(
				# Add custom CSS & Javascript;
				tagList(
						tags$head(
								tags$link(rel="stylesheet", type="text/css",href="style.css"),
								tags$script(type="text/javascript", src = "md5.js"),
								tags$script(type="text/javascript", src = "passwdInputBinding.js")
						)
				),
				
				## Login module;
				h1(WEBTITLE,align="center"),
				div(class = "login",
						uiOutput("uiLogin"),
						textOutput("pass")
				),
				uiOutput("tabSets")
		))
