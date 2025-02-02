data_1 <-data.frame(x1 = c(7, 3, 2, 9, 0),          # Column 1 of data frame
                     x2 = c(4, 4, 1, 1, 8),          # Column 2 of data frame
                     x3 = c(5, 3, 9, 2, 4))  
y1 <-c(9, 8, 7, 6, 5)
data_new1<-cbind(data_1, y1)                       # cbind vector to data frame
data_new1
des_plus <- data.frame(des, lanceur = rep(c("Luc", "Kim"), each = 5))
###covid en france 
new_data_frame <- read.csv(file="https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7", header=T, sep=";")
new_data_frame$date<-as.Date(new_data_frame$jour, tryFormats = c("%d/%m/%Y"))
new_data_frame$date<- as.Date(new_data_frame$date)
head(new_data_frame$date)
str(new_data_frame)
new_data_frame <- subset(new_data_frame, select = c (dep,sexe,jour,hosp,rea,rad,dc))
colnames(new_data_frame)<-c("department","gender","date","nb_hospitalizations","nb_reanimations","nb_returned_home","nb_deaths")
new_data_frame$gender<-factor(new_data_frame$gender,labels=c("males & females","males","females"))
summary(new_data_frame)
head(new_data_frame)
install.packages("dplyr")
library(dplyr)
new_data_frame_2 <- new_data_frame %>% select(c(nb_hospitalizations,nb_reanimations,nb_deaths,department))
new_data <- new_data_frame %>% filter(department == "75")
new_data <-  new_data_frame %>%  filter(date=="2022-11-21" & gender!="males & females")  %>% select(c(nb_hospitalizations,department)) %>% group_by(department)  %>% summarize(sum_nb_hostpitalizations_2022_11_21=sum(nb_hospitalizations))
install.packages("ggplot2")
library(ggplot2)
new_data <- new_data_frame %>%  filter(gender=="males & females") %>% select(c(nb_deaths,date,department)) %>% group_by(date) %>%  summarize(sum_nb_deaths=sum(nb_deaths))
ggplot(data=new_data,aes(x=date,y=sum_nb_deaths))+ geom_line()
##
new_data <- new_data_frame %>%  filter(date=="2022-11-21" & gender!="males & females") %>% select(c(nb_deaths,gender)) %>% group_by(gender)  %>% summarize(sum_nb_deaths=sum(nb_deaths))
ggplot(data=new_data,aes(x=gender,y=sum_nb_deaths)) + geom_col(aes(fill=gender))+ theme_classic() +  ggtitle("Total number of deaths by gender as of 2022-11-21")+labs(fill = "Gender category",x="", y="Number of deaths")
##
new_data <- new_data_frame %>% filter(department=="25" & gender!="males & females")  %>% select(c(nb_hospitalizations,gender,date))
ggplot(data=new_data,aes(x=date,y=nb_hospitalizations,fill=gender)) + geom_point(size=1, shape=23)+ theme_classic() + labs(y="Number of deaths")+ ggtitle("Number of hospitalizations in Doubs departement over time by gender")+labs(fill = "Gender categories",x="Date", y="Number of hospitalizations")+theme(plot.title = element_text(hjust = 0.5))+scale_x_date(date_breaks = "1 month")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+scale_y_continuous(n.breaks=30)
getwd()
pop_data <- read.csv("pop2022.csv", sep=",")
str(pop_data)
new_data <- merge(new_data_frame, pop_data, by.x = "department", by.y = "department")
head(new_data)
data2022<-new_data_frame %>% filter(date > '2021-12-31')
new_data <- merge(data2022, pop_data, by.x = "department", by.y = "department")
new_data2<- new_data %>% filter(date=="2022-11-21" & gender=="males & females") %>% mutate(deaths_pop_ratio=(nb_deaths/pop)*100) %>% select(c(department,name_dep,deaths_pop_ratio))
new_data2<- new_data2  %>%  arrange(-deaths_pop_ratio)
head(new_data2,10)
ggplot(data=new_data2[0:10,], aes(x="", y=deaths_pop_ratio, fill=name_dep)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()
##Rshiny
install.packages("shiny")
library(shiny)
new_data_frame
#create the user interface (ui), this time we specify more things:
ui <- fluidPage(
  #we create a select box containing all the variables of the data frame new_data_frame. The name of this select box is variable.
  varSelectInput("variable", label = "Please choose a variable", new_data_frame),
  #we show the text output called summary that is created by our server function upon an action of the user
  verbatimTextOutput("summary"),
  #we show the table called table that is created by our server function upon an action of the user
  tableOutput("table")
)

#we store the server functions here - where we perform calculations based on the user's choices
server <- function(input, output, session) {
  
  #we define a reactive, something that reacts to the user's actions, when the user changes something. We select the column of the dataframe based on the value selected in the select box by the user.
  dataset <- reactive({
    new_data_frame  %>% select(c(!!input$variable))
  })
  
  #we define a first output that we want to show, it is built-in summary function of R applied to the variable of the dataframe selected by the user, which we have called variable. We use renderPrint() function from Shiny so that it turns it into a piece of text to show to the user.
  output$summary <- renderPrint({
    summary(dataset())
  })
  
  #we define a second output that we want to show, it is the first five rows of the dataframe, we call head() We use renderTable() function from shiny so that it turns it into table to show to the user.
  output$table <- renderTable({
    head(dataset())
  })
}

#we create the app
shinyApp(ui, server)

##
des_plus=data.frame(des, lanceur = rep(c("Luc", "Kim"), each = 5))
str(des_plus)
typeof(des_plus) 
des_plus['lanceur']=as.factor(des_plus['lanceur'])
str(des_plus)

###

