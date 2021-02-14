library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(usmap)


df <- read.csv("annual_generation_state.csv")

# Remove rows with
# Missing values (State)
# "GENERATION (Megawatthours)" with negative values
# "ENERGY SOURCE" with "Other", "Other Gases", "Other Biomass", "Pumped Storage"
df <- subset(df, STATE != "  ")
df <- subset(df, GENERATION..Megawatthours. >= 0)
df <- subset(df, ENERGY.SOURCE != "Other"         & ENERGY.SOURCE != "Other Gases" &
               ENERGY.SOURCE != "Other Biomass" & ENERGY.SOURCE != "Pumped Storage")
unique(df$ENERGY.SOURCE)

# Convert string numbers to numeric
t_gen <- as.numeric(gsub(",", "", df$GENERATION..Megawatthours.))

f_state <- toupper(df$STATE)
f_state <- factor(f_state)

# unique(f_state)
f_state <- as.factor(f_state)
f_type.of.producer <- as.factor(df$TYPE.OF.PRODUCER)
f_energy.source <- as.factor(df$ENERGY.SOURCE)
f_year <- as.factor(df$YEAR)

# Rename "ENERGY SOURCE" to be more compact
# levels(f_energy.source)
levels(f_energy.source)[3] <- "Hydro"
levels(f_energy.source)[7] <- "Solar"
levels(f_energy.source)[10] <- "Wood"
# levels(f_energy.source)

# Colorblind-friendly palette
c.pal <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000", "#000000")

dfplot <- data.frame(f_year, f_energy.source, t_gen)
dfplot.no.total <- subset(dfplot,f_energy.source != "Total")

sbc.amount.each.energy <- ggplot(dfplot.no.total, aes(x=f_year, y=t_gen, fill=f_energy.source)) +
                                  geom_bar(position="stack", stat="identity") +
                                  labs(x="Year", y="Amount (Megawatthours)", fill="Energy Source") +
                                  theme(text=element_text(size=12,  family="sans"), axis.text.x= element_text(angle=270))


sbc.percent.each.energy <- ggplot(dfplot.no.total, aes(x=f_year, y=t_gen, fill=f_energy.source)) +
                                    geom_bar(position="fill", stat="identity") +
                                    labs(x="Year", y="Total Production (%)", fill="Energy Source") +
                                    theme(text=element_text(size=12,  family="sans"), axis.text.x= element_text(angle=270))

lc.amount.each.energy <- ggplot(dfplot.no.total, aes(x=f_year, colour=f_energy.source)) +
                                  stat_summary(aes(y = t_gen,group = f_energy.source), geom="line") +
                                  labs(x="Year", y="Amount (Megawatthours)", colour="Energy Source") +
                                  theme(text=element_text(size=12,  family="sans"), axis.text.x= element_text(angle=270))

# Total gen by year by source (30years X 10sources matrix)
gen.sum.by.source <- matrix(nrow=length(levels(f_year)),ncol=length(levels(f_energy.source)))
rownames(gen.sum.by.source) <- levels(f_year)
colnames(gen.sum.by.source)   <- levels(f_energy.source)
for (i in 1:nrow(gen.sum.by.source)) {
  for (j in 1:ncol(gen.sum.by.source)) {
    gen.sum.by.source[i,j] <- sum(subset(dfplot.no.total, f_year==levels(f_year)[i] & f_energy.source==levels(f_energy.source)[j])$t_gen)
  }
  gen.sum.by.source[i,8] <- sum(gen.sum.by.source[i,])
}

percent.by.source <- matrix(nrow=length(levels(f_year)),ncol=length(levels(f_energy.source)))
rownames(percent.by.source) <- levels(f_year)
colnames(percent.by.source)   <- levels(f_energy.source)
for (i in 1:nrow(percent.by.source)) {
  for (j in 1:ncol(percent.by.source)) {
    if (j!=8) {
      percent.by.source[i,j] <- gen.sum.by.source[i,j] / gen.sum.by.source[i,8] * 100
    }
  }
}

df_gen.sum.by.source <- as.data.frame(gen.sum.by.source)

df_percent.by.source <- as.data.frame(percent.by.source)
df_percent.by.source$Year <- c(1990:2019)
# df_percent.by.source <- df_percent.by.source[,c(ncol(df_percent.by.source),1:(ncol(df_percent.by.source)-1))]
lc.percent.each.energy <- ggplot(df_percent.by.source, aes(x=Year,y=df_percent.by.source$Coal)) + 
                                  geom_line(aes(y=df_percent.by.source$Coal, color=c.pal[9])) +
                                  geom_line(aes(y=df_percent.by.source$Geothermal, color=c.pal[6])) +
                                  geom_line(aes(y=df_percent.by.source$Hydro, colour=c.pal[4])) +
                                  geom_line(aes(y=df_percent.by.source$`Natural Gas`,color=c.pal[3])) +
                                  geom_line(aes(y=df_percent.by.source$Nuclear,color=c.pal[1])) +
                                  geom_line(aes(y=df_percent.by.source$Petroleum,color=c.pal[8])) +
                                  geom_line(aes(y=df_percent.by.source$Solar,color=c.pal[7])) +
                                  geom_line(aes(y=df_percent.by.source$Wind,color=c.pal[2])) +
                                  geom_line(aes(y=df_percent.by.source$Wood,color=c.pal[5])) + 
                                  labs(x='Year', y='Percentage (%)', color='Energy Source') + 
                                  scale_color_discrete(labels = c(levels(f_energy.source))) + 
                                  theme(text=element_text(size=16,  family="sans"), axis.text.x= element_text(angle=270))
df_percent.by.source <- subset(df_percent.by.source, select=-Year)


state_list <- c(state.name,"Washington DC","Total US")
state_abbr_list <- c(state.abb,"DC","US")

dfplot.state <- data.frame(f_year, f_state, f_energy.source, t_gen)
dfplot.no.total.state <- subset(dfplot.state,f_energy.source != "Total")

temp.by.source <- matrix(nrow=length(levels(f_year)),ncol=length(levels(f_energy.source)))
rownames(temp.by.source) <- levels(f_year)
colnames(temp.by.source)   <- levels(f_energy.source)

# Each state energy generation and source by year
list_of_state_by_year <- list()
for (i in 1:50) {
  for (j in 1:nrow(temp.by.source)) {
    ttemp_df <- subset(dfplot.no.total.state, f_state==levels(f_state)[i] & f_year==levels(f_year)[j])
    ttemp_df <- aggregate(ttemp_df$t_gen,
                          by=list(f_year=ttemp_df$f_year, f_state=ttemp_df$f_state, f_energy.source=ttemp_df$f_energy.source), FUN=sum)
    if(j==1) {temp_df <- ttemp_df}
    else{temp_df <- rbind(temp_df,ttemp_df)}
  }
  list_of_state_by_year[i] <- list(temp_df)
}

# Convert to percentage of each energy source in state per year
temp.by.source_list <- list()
temp.percent.list <- list()
t <- c(1,2,3,4,5,6,7,9,10)
for (states in 1:50){
  temp.by.source <- matrix(nrow=length(levels(f_year)),ncol=length(levels(f_energy.source)))
  rownames(temp.by.source) <- levels(f_year)
  colnames(temp.by.source) <- levels(f_energy.source)
  
  for (i in 1:length(levels(f_year))) {
    for (j in 1:length(levels(f_energy.source))) {
      ss <- subset(list_of_state_by_year[[states]], f_year==levels(f_year)[i] & f_energy.source==levels(f_energy.source)[j])$x
       if(length(ss) >0) {temp.by.source[i,j] <- ss}  
    }
    temp.by.source[,8] <- rowSums(temp.by.source[,t], na.rm=TRUE)
  }
  temp.by.source_list[states] <- list(temp.by.source)
  
  temp.percent <- matrix(nrow=length(levels(f_year)),ncol=length(levels(f_energy.source)))
  rownames(temp.percent) <- levels(f_year)
  colnames(temp.percent) <- levels(f_energy.source)
  
  for (i in 1:length(levels(f_year))) {
    for (j in 1:length(levels(f_energy.source))) {
      temp.percent[i,j] <- temp.by.source[i,j] / temp.by.source[i,8] * 100
    }
  }
  temp.percent.list[states] <- list(temp.percent)
}

df_heat_percent <- data.frame()
for (i in 1:50) {
  temp_for_fips <- as.data.frame(temp.percent.list[[i]])
  temp_for_fips$fips <- levels(f_state)[i]
  temp_for_fips$year <- c(1990:2019)
  df_heat_percent <- rbind(df_heat_percent,temp_for_fips)
}



# Shiny Interface
ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Project 1"),
  
  # Sidebar start
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
                   sidebarMenu(
                     menuItem("About", tabName = "about"),
                     menuItem("Visualization", tabName = "visualization"),
                     menuItem("Comparison", tabName = "comparison")
                   )
  ),
  # Sidebar end
  
  # Body start
  dashboardBody(
    tabItems(
      # "About" Tab
      tabItem(tabName = "about",
              h2("About"),
              p("This project focuses on the visualization of data using the annual electrical power generation in the US dataset.
                The original data is available from",
                a("https://www.eia.gov/electricity/data/state/"),
                ".",
                style = "font-family : sans-serif"),
              p("The app is written by Eakasorn Suraminitkul and uses the data from 1990 until year 2019.",
                style = "font-family : sans-serif"),
      ),
      
      # "Visualization" Tab
      tabItem(tabName = "visualization",
              tabsetPanel(
                # "Overview" Sub-tab
                tabPanel("Overview",
                         h2("Electrical power generation in the US"),

                         fluidRow(
                           column(1,
                                  checkboxInput("c1", "Coal", FALSE),
                           ),
                           column(1,
                                  checkboxInput("c2", "Geothermal", FALSE),
                           ),
                           column(1,
                                  checkboxInput("c3", "Hydro", FALSE),
                           ),
                           column(1,
                                  checkboxInput("c4", "Natural Gas", FALSE),
                           ),
                           column(1,
                                  checkboxInput("c5", "Nuclear", FALSE),
                           ),
                           column(1,
                                  checkboxInput("c6", "Petroleum", FALSE),
                           ),
                           column(1,
                                  checkboxInput("c7", "Solar", FALSE),
                           ),
                           column(1,
                                  checkboxInput("c8", "Wind", FALSE),
                           ),
                           column(1,
                                  checkboxInput("c9", "Wood", FALSE),
                           ),
                           column(1,
                                  checkboxInput("c10", "All", TRUE),
                           )
                         ),

                         fluidRow(
                           column(6,
                                  fluidRow(
                                    box(title = "Amount of each energy source (1990-2019)", solidHeader = TRUE, status = "primary", width = 12,
                                        plotOutput("sbc.amount.each.energy", height = 275)
                                    )
                                  )
                           ),
                           column(6,
                                  fluidRow(
                                    box(title = "Percent of each energy source (1990-2019)", solidHeader = TRUE, status = "primary", width = 12,
                                        plotOutput("sbc.percent.each.energy", height = 275)
                                    )
                                  )
                           )
                         ),
                         
                         
                         fluidRow(
                           column(6,
                                  fluidRow(
                                    box(title = "Amount of each energy source (1990-2019)", solidHeader = TRUE, status = "primary", width = 12,
                                        plotOutput("lc.amount.each.energy", height = 275)
                                    )
                                  )
                           ),
                           column(6,
                                  fluidRow(
                                    box(title = "Percent of each energy source (1990-2019)", solidHeader = TRUE, status = "primary", width = 12,
                                        plotOutput("lc.percent.each.energy", height = 275)
                                    )
                                  )
                           )
                         )
                ),
                
                # "Table 1" Sub-tab
                tabPanel("Table (Amount)",
                         fluidRow(
                            box(title = "Amount of each energy source (1990-2019)", solidHeader = TRUE, status = "primary", width = 12,
                                DT::dataTableOutput("table.amount.each.energy"),style = "height:550px; overflow-y: scroll;overflow-x: scroll;"
                              )
                         )
                ),
                # "Table 2" Sub-tab
                tabPanel("Table (Percentage)",
                         fluidRow(
                             box(title = "Percent of each energy source (1990-2019)", solidHeader = TRUE, status = "primary", width = 12,
                                DT::dataTableOutput("table.percent.each.energy"),style = "height:550px; overflow-y: scroll;overflow-x: scroll;"
                              )
                        )
                )
                
              )
              

      ),
      
      # "Comparison" Tab
      tabItem(tabName = "comparison",
              tabsetPanel(
                # "Comparison Zone 1" Sub-tab
                tabPanel("Zone",
                        h2("Zone Comparison"),
                        
                        column(6,
                               selectInput("state1", "Select a state",       
                                           state_list, selected="Total US"),
                               fluidRow(
                                 box(title = "Amount of each energy source (1990-2019)", solidHeader = TRUE, status = "primary", width = 12,
                                     plotOutput("sbc.amount.p2s1", height = 250)
                                 )
                               ),
                               fluidRow(
                                 box(title = "Amount of each energy source (1990-2019)", solidHeader = TRUE, status = "primary", width = 12,
                                     plotOutput("lc.amount.p2s1", height = 250)
                                 )
                               )
                        ),

                        column(6,
                               selectInput("state2", "Select a state",       
                                           state_list, selected="Illinois"),
                               fluidRow(
                                 box(title = "Amount of each energy source (1990-2019)", solidHeader = TRUE, status = "primary", width = 12,
                                     plotOutput("sbc.amount.p2s2", height = 250)
                                 )
                               ),
                               fluidRow(
                                 box(title = "Amount of each energy source (1990-2019)", solidHeader = TRUE, status = "primary", width = 12,
                                     plotOutput("lc.amount.p2s2", height = 250)
                                 )
                               )
                               
                        )
                ),

                # "Comparison 2" Sub-tab
                tabPanel("Multiple",
                         h2("Multiple Conditions Comparison"),
                         
                         column(6,
                                fluidRow(
                                  column(2,selectInput("comp.tab2.p1", "Year",       
                                            c(1990:2019," "), selected=" ")),
                                  column(2,selectInput("comp.tab2.p2", "State",       
                                            c(state_abbr_list," "), selected=" ")),
                                  column(2,selectInput("comp.tab2.p3", "Energy source",       
                                            c(levels(f_energy.source)[1:7],levels(f_energy.source)[9:10]," "), selected=" "))
                                ),
                                fluidRow(
                                  box(title = "Amount of each energy source (1990-2019)", solidHeader = TRUE, status = "primary", width = 12,
                                      plotOutput("sbc.amount.p2ss1", height = 250)
                                  )
                                ),
                                fluidRow(
                                  box(title = "Amount of each energy source (1990-2019)", solidHeader = TRUE, status = "primary", width = 12,
                                      plotOutput("lc.amount.p2ss1", height = 250)
                                  )
                                )
                         ),

                         
                         column(6,
                                fluidRow(
                                  column(2,selectInput("comp.tab2.p4", "Year",       
                                                       c(1990:2019," "), selected=" ")),
                                  column(2,selectInput("comp.tab2.p5", "State",       
                                                       c(state_abbr_list," "), selected=" ")),
                                  column(2,selectInput("comp.tab2.p6", "Energy source",       
                                                       c(levels(f_energy.source)[1:7],levels(f_energy.source)[9:10]," "), selected=" "))
                                ),
                                fluidRow(
                                  box(title = "Amount of each energy source (1990-2019)", solidHeader = TRUE, status = "primary", width = 12,
                                      plotOutput("sbc.amount.p2ss2", height = 250)
                                  )
                                ),
                                fluidRow(
                                  box(title = "Amount of each energy source (1990-2019)", solidHeader = TRUE, status = "primary", width = 12,
                                      plotOutput("lc.amount.p2ss2", height = 250)
                                  )
                                )
                         )

                ),
                
                # "Comparison 3" Sub-tab
                tabPanel("Geographic",
                         h2("Geographic Conditions Comparison"),
                         
                         column(6,
                                fluidRow(
                                  column(2,selectInput("comp.tab3.p1", "Year",       
                                                       c(1990:2019), selected="1990")),
                                  column(2,selectInput("comp.tab3.p2", "Energy source",       
                                                       c(levels(f_energy.source)[1:7],levels(f_energy.source)[9:10]), selected="Coal"))
                                ),
                                fluidRow(
                                  box(title = "Amount of each energy source (1990-2019)", solidHeader = TRUE, status = "primary", width = 12,
                                      plotOutput("heat.amount1", height = 250)
                                      # DT::dataTableOutput("heat.amount1"),style = "height:550px; overflow-y: scroll;overflow-x: scroll;"
                                      
                                  )
                                ),
                                fluidRow(
                                  box(title = "Amount of each energy source (1990-2019)", solidHeader = TRUE, status = "primary", width = 12,
                                      # plotOutput("heat.percent1", height = 275)
                                      DT::dataTableOutput("heat.percent1"),style = "height:250; overflow-y: scroll;overflow-x: scroll;"
                                      # textOutput("heat.percent1")
                                  )
                                )
                         ),
                         
                         
                         column(6,
                                fluidRow(
                                  column(2,selectInput("comp.tab3.p3", "Year",       
                                                       c(1990:2019), selected="2019")),
                                  column(2,selectInput("comp.tab3.p4", "Energy source",       
                                                       c(levels(f_energy.source)[1:7],levels(f_energy.source)[9:10]), selected="Coal"))
                                ),
                                fluidRow(
                                  box(title = "Amount of each energy source (1990-2019)", solidHeader = TRUE, status = "primary", width = 12,
                                      plotOutput("heat.amount2", height = 250)
                                  )
                                ),
                                fluidRow(
                                  box(title = "Percentage of each energy source (1990-2019)", solidHeader = TRUE, status = "primary", width = 12,
                                      plotOutput("heat.percent2", height = 250)
                                  )
                                )
                         )
                         
                )
              )
              
              
      )
      
    )
  )
)

server <- function(input, output) {
  cclist <- c("Coal","Geothermal","Hydro","Natural Gas","Nuclear","Petroleum","Solar","Wind","Wood")
  cc1 <- renderText({c1 = ifelse(input$c1, cclist[1], "")})
  cc2 <- renderText({c2 = ifelse(input$c2, cclist[2], "")})
  cc3 <- renderText({c3 = ifelse(input$c3, cclist[3], "")})
  cc4 <- renderText({c4 = ifelse(input$c4, cclist[4], "")})
  cc5 <- renderText({c5 = ifelse(input$c5, cclist[5], "")})
  cc6 <- renderText({c6 = ifelse(input$c6, cclist[6], "")})
  cc7 <- renderText({c7 = ifelse(input$c7, cclist[7], "")})
  cc8 <- renderText({c8 = ifelse(input$c8, cclist[8], "")})
  cc9 <- renderText({c9 = ifelse(input$c9, cclist[9], "")})

  temp_list <- reactive(match(c(cc1(),cc2(),cc3(),cc4(),cc5(),cc6(),cc7(),cc8(),cc9()), cclist) [!is.na(match(c(cc1(),cc2(),cc3(),cc4(),cc5(),cc6(),cc7(),cc8(),cc9()), cclist))])

  # For part 1
  output$sbc.amount.each.energy <- renderPlot({
    if(input$c10==TRUE) {sbc.amount.each.energy}
    else{
      temp <- subset(dfplot.no.total,f_energy.source==cc1()|f_energy.source==cc2()|f_energy.source==cc3()|
               f_energy.source==cc4()|f_energy.source==cc5()|f_energy.source==cc6()|
               f_energy.source==cc7()|f_energy.source==cc8()|f_energy.source==cc9())
  
      ggplot(temp, aes(x=f_year, y=t_gen, fill=f_energy.source)) +
        geom_bar(position="stack", stat="identity") +
        labs(x="Year", y="Amount (Megawatthours)", fill="Energy Source") +
        theme(text=element_text(size=12,  family="sans"), axis.text.x= element_text(angle=270))
    }
  })
    
  output$sbc.percent.each.energy <- renderPlot({
    if(input$c10==TRUE) {sbc.percent.each.energy}
    else{
      temp <- subset(dfplot.no.total,f_energy.source==cc1()|f_energy.source==cc2()|f_energy.source==cc3()|
               f_energy.source==cc4()|f_energy.source==cc5()|f_energy.source==cc6()|
               f_energy.source==cc7()|f_energy.source==cc8()|f_energy.source==cc9())
      
      ggplot(temp, aes(x=f_year, y=t_gen, fill=f_energy.source)) +
        geom_bar(position="fill", stat="identity") +
        labs(x="Year", y="Total Production (%)", fill="Energy Source") +
        theme(text=element_text(size=12,  family="sans"), axis.text.x= element_text(angle=270))
    }
  })

  output$lc.amount.each.energy <- renderPlot({
    if(input$c10==TRUE) {lc.amount.each.energy}
    else{
      temp <- subset(dfplot.no.total,f_energy.source==cc1()|f_energy.source==cc2()|f_energy.source==cc3()|
                       f_energy.source==cc4()|f_energy.source==cc5()|f_energy.source==cc6()|
                       f_energy.source==cc7()|f_energy.source==cc8()|f_energy.source==cc9())
      
      ggplot(temp, aes(x=f_year, colour=f_energy.source)) +
      stat_summary(aes(y = t_gen,group = f_energy.source), geom="line") +
      labs(x="Year", y="Amount (Megawatthours)", colour="Energy Source") +
      theme(text=element_text(size=12,  family="sans"), axis.text.x= element_text(angle=270))
    }
  })
  output$lc.percent.each.energy <- renderPlot({
    if(input$c10==TRUE) {lc.percent.each.energy}
    else{
      temp <- subset(dfplot.no.total,f_energy.source==cc1()|f_energy.source==cc2()|f_energy.source==cc3()|
                       f_energy.source==cc4()|f_energy.source==cc5()|f_energy.source==cc6()|
                       f_energy.source==cc7()|f_energy.source==cc8()|f_energy.source==cc9())
      
      df_percent.by.source$Year <- c(1990:2019)
      
      ggplot(temp, aes(x=Year,y=df_percent.by.source$Coal)) 
        if(input$c1==TRUE) {+ geom_line(aes(y=df_percent.by.source$Coal, color=c.pal[9]))}
        if(input$c1==TRUE) {+ geom_line(aes(y=df_percent.by.source$Geothermal, color=c.pal[6]))}
        if(input$c1==TRUE) {+ geom_line(aes(y=df_percent.by.source$Hydro, colour=c.pal[4]))}
        if(input$c1==TRUE) {+ geom_line(aes(y=df_percent.by.source$`Natural Gas`,color=c.pal[3]))}
        if(input$c1==TRUE) {+ geom_line(aes(y=df_percent.by.source$Nuclear,color=c.pal[1]))}
        if(input$c1==TRUE) {+ geom_line(aes(y=df_percent.by.source$Petroleum,color=c.pal[8]))}
        if(input$c1==TRUE) {+ geom_line(aes(y=df_percent.by.source$Solar,color=c.pal[7]))}
        if(input$c1==TRUE) {+ geom_line(aes(y=df_percent.by.source$Wind,color=c.pal[2]))}
        if(input$c1==TRUE) {+ geom_line(aes(y=df_percent.by.source$Wood,color=c.pal[5]))} +
      labs(x='Year', y='Percentage (%)', color='Energy Source') + 
      scale_color_discrete(labels = c(levels(f_energy.source))) + 
      theme(text=element_text(size=16,  family="sans"), axis.text.x= element_text(angle=270))
      
      df_percent.by.source <- subset(df_percent.by.source, select=-Year)
    }
  })
  
  output$table.amount.each.energy <- renderDataTable({
    if(input$c10==TRUE) {df_gen.sum.by.source}
    else{
      subset(df_gen.sum.by.source,select=temp_list())
    }
  })
  output$table.percent.each.energy <- renderDataTable({
    if(input$c10==TRUE) {df_percent.by.source}
    else{
      subset(df_percent.by.source, select=temp_list())
    }
  })
  
  
  # For part 2
  output$sbc.amount.p2s1 <- renderPlot({
    if (input$state1=='Total US') {sbc.amount.each.energy}
    else({
        if (input$state1=='Washington DC') {
          temp <- reactive(subset(dfplot.no.total.state, f_state=='DC'))
        }
        else{
          temp <- reactive({
            subset(dfplot.no.total.state, f_state==renderText({state.abb[match(input$state1,state.name)]})())
          })
        }
  
        ggplot(as.data.frame(temp()), aes(x=f_year, y=t_gen, fill=f_energy.source)) +
          geom_bar(position="stack", stat="identity") +
          labs(x="Year", y="Amount (Megawatthours)", fill="Energy Source") +
          theme(text=element_text(size=12,  family="sans"), axis.text.x= element_text(angle=270))
    })
  })
  output$sbc.amount.p2s2 <- renderPlot({
    if (input$state2=='Total US') {sbc.amount.each.energy}
    else({
      if (input$state2=='Washington DC') {
        temp <- reactive(subset(dfplot.no.total.state, f_state=='DC'))
      }
      else{
        temp <- reactive({
          subset(dfplot.no.total.state, f_state==renderText({state.abb[match(input$state2,state.name)]})())
        })
      }
      
      ggplot(as.data.frame(temp()), aes(x=f_year, y=t_gen, fill=f_energy.source)) +
        geom_bar(position="stack", stat="identity") +
        labs(x="Year", y="Amount (Megawatthours)", fill="Energy Source") +
        theme(text=element_text(size=12,  family="sans"), axis.text.x= element_text(angle=270))
    })
  })
  
  output$lc.amount.p2s1 <- renderPlot({
    if (input$state1=='Total US') {lc.amount.each.energy}
    else({
      if (input$state1=='Washington DC') {
        temp <- reactive(subset(dfplot.no.total.state, f_state=='DC'))
      }
      else{
        temp <- reactive({
          subset(dfplot.no.total.state, f_state==renderText({state.abb[match(input$state1,state.name)]})())
        })
      }
      
      ggplot(as.data.frame(temp()), aes(x=f_year, colour=f_energy.source)) +
        stat_summary(aes(y = t_gen,group = f_energy.source), geom="line") +
        labs(x="Year", y="Amount (Megawatthours)", colour="Energy Source") +
        theme(text=element_text(size=12,  family="sans"), axis.text.x= element_text(angle=270))
    })
  })
  
  output$lc.amount.p2s2 <- renderPlot({
    if (input$state2=='Total US') {lc.amount.each.energy}
    else({
      if (input$state2=='Washington DC') {
        temp <- reactive(subset(dfplot.no.total.state, f_state=='DC'))
      }
      else{
        temp <- reactive({
          subset(dfplot.no.total.state, f_state==renderText({state.abb[match(input$state2,state.name)]})())
        })
      }
      
      ggplot(as.data.frame(temp()), aes(x=f_year, colour=f_energy.source)) +
        stat_summary(aes(y = t_gen,group = f_energy.source), geom="line") +
        labs(x="Year", y="Amount (Megawatthours)", colour="Energy Source") +
        theme(text=element_text(size=12,  family="sans"), axis.text.x= element_text(angle=270))
    })
  })
  
  output$sbc.amount.p2ss1 <- renderPlot({
    # Year Condition 1
    if (input$comp.tab2.p1==' ') {temp1 <- reactive(dfplot.no.total.state)}
    else{temp1 <- reactive(subset(dfplot.no.total.state, f_year==renderText(input$comp.tab2.p1)()))}
    
    # State Condition 2
    if (input$comp.tab2.p2=='DC') {
      temp2 <- reactive(subset(temp1(), f_state=='DC'))
    }
    else{
      if (input$comp.tab2.p2=='US'|input$comp.tab2.p2==' ') {
        temp2 <- reactive(subset(temp1(), f_state=='US-TOTAL'))
      }
      else{
        temp2 <- reactive({
          subset(temp1(), f_state==renderText(input$comp.tab2.p2)())
        })
      }
    }
    
    # Energy Source Condition 3
    if (input$comp.tab2.p3==' ') {temp3 <- reactive(temp2())}
    else{
      temp3 <- reactive({
        subset(temp2(), f_energy.source==renderText(input$comp.tab2.p3)())
      })
    }
    
    ggplot(as.data.frame(temp3()), aes(x=f_year, y=t_gen, fill=f_energy.source)) +
      geom_bar(position="stack", stat="identity") +
      labs(x="Year", y="Amount (Megawatthours)", fill="Energy Source") +
      theme(text=element_text(size=12,  family="sans"), axis.text.x= element_text(angle=270))
  })
    
  output$sbc.amount.p2ss2 <- renderPlot({
    # Year Condition 1
    if (input$comp.tab2.p4==' ') {temp1 <- reactive(dfplot.no.total.state)}
    else{temp1 <- reactive(subset(dfplot.no.total.state, f_year==renderText(input$comp.tab2.p4)()))}
    
    # State Condition 2
    if (input$comp.tab2.p5=='DC') {
      temp2 <- reactive(subset(temp1(), f_state=='DC'))
    }
    else{
      if (input$comp.tab2.p5=='US'|input$comp.tab2.p5==' ') {
        temp2 <- reactive(subset(temp1(), f_state=='US-TOTAL'))
      }
      else{
        temp2 <- reactive({
          subset(temp1(), f_state==renderText(input$comp.tab2.p5)())
        })
      }
    }
    
    # Energy Source Condition 3
    if (input$comp.tab2.p6==' ') {temp3 <- reactive(temp2())}
    else{
      temp3 <- reactive({
        subset(temp2(), f_energy.source==renderText(input$comp.tab2.p6)())
      })
    }
    
    ggplot(as.data.frame(temp3()), aes(x=f_year, y=t_gen, fill=f_energy.source)) +
      geom_bar(position="stack", stat="identity") +
      labs(x="Year", y="Amount (Megawatthours)", fill="Energy Source") +
      theme(text=element_text(size=12,  family="sans"), axis.text.x= element_text(angle=270))
  })
  
  output$lc.amount.p2ss1 <- renderPlot({
    # Year Condition 1
    if (input$comp.tab2.p1==' ') {temp1 <- reactive(dfplot.no.total.state)}
    else{temp1 <- reactive(subset(dfplot.no.total.state, f_year==renderText(input$comp.tab2.p1)()))}
    
    # State Condition 2
    if (input$comp.tab2.p2=='DC') {
      temp2 <- reactive(subset(temp1(), f_state=='DC'))
    }
    else{
      if (input$comp.tab2.p2=='US'|input$comp.tab2.p2==' ') {
        temp2 <- reactive(subset(temp1(), f_state=='US-TOTAL'))
      }
      else{
        temp2 <- reactive({
          subset(temp1(), f_state==renderText(input$comp.tab2.p2)())
        })
      }
    }
    
    # Energy Source Condition 3
    if (input$comp.tab2.p3==' ') {temp3 <- reactive(temp2())}
    else{
      temp3 <- reactive({
        subset(temp2(), f_energy.source==renderText(input$comp.tab2.p3)())
      })
    }
    
    ggplot(as.data.frame(temp3()), aes(x=f_year, colour=f_energy.source)) +
      stat_summary(aes(y = t_gen,group = f_energy.source), geom="line") +
      labs(x="Year", y="Amount (Megawatthours)", colour="Energy Source") +
      theme(text=element_text(size=12,  family="sans"), axis.text.x= element_text(angle=270))
  })
  
  output$lc.amount.p2ss2 <- renderPlot({
    # Year Condition 1
    if (input$comp.tab2.p4==' ') {temp1 <- reactive(dfplot.no.total.state)}
    else{temp1 <- reactive(subset(dfplot.no.total.state, f_year==renderText(input$comp.tab2.p4)()))}
    
    # State Condition 2
    if (input$comp.tab2.p5=='DC') {
      temp2 <- reactive(subset(temp1(), f_state=='DC'))
    }
    else{
      if (input$comp.tab2.p5=='US'|input$comp.tab2.p5==' ') {
        temp2 <- reactive(subset(temp1(), f_state=='US-TOTAL'))
      }
      else{
        temp2 <- reactive({
          subset(temp1(), f_state==renderText(input$comp.tab2.p5)())
        })
      }
    }
    
    # Energy Source Condition 3
    if (input$comp.tab2.p6==' ') {temp3 <- reactive(temp2())}
    else{
      temp3 <- reactive({
        subset(temp2(), f_energy.source==renderText(input$comp.tab2.p6)())
      })
    }
    
    ggplot(as.data.frame(temp3()), aes(x=f_year, colour=f_energy.source)) +
      stat_summary(aes(y = t_gen,group = f_energy.source), geom="line") +
      labs(x="Year", y="Amount (Megawatthours)", colour="Energy Source") +
      theme(text=element_text(size=12,  family="sans"), axis.text.x= element_text(angle=270))
  })
  
  
  # For part 3
  # comp.tab3.p2
  output$heat.percent1 <- renderDataTable({
    temp <- subset(df_heat_percent, year==renderText(input$comp.tab3.p1)())
    temp <- temp[,c('fips', renderText(input$comp.tab3.p2)())]
    colnames(temp) <- c('fips','value')
    temp <- as.data.frame(temp)
  })

  output$heat.percent2 <- renderPlot({
    temp <- subset(df_heat_percent, year==renderText(input$comp.tab3.p3)())
    temp <- temp[,c('fips', renderText(input$comp.tab3.p4)())]
    colnames(temp) <- c('fips','value')
    temp <- as.data.frame(temp)
    
    plot_usmap(temp)
  })
  
  # output$value <- renderText({ input$somevalue })
}


shinyApp(ui = ui, server = server)
