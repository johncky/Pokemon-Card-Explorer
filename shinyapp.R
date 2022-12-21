library(shiny)
library(tidyverse)
library(shinycssloaders)
library(RSQLite)

# Compute top card sets by trading volume
top_volume_set = function(card_db, year=NULL, month=NULL, day=NULL, top=10){
  data = card_db
  if (!is.null(year))
    data  = data %>% filter(Year==year)
  if (!is.null(month))
    data  = data %>% filter(Month==month)
  if (!is.null(day))
    data  = data %>% filter(Day==day)
  data = data %>% group_by(setName) %>% count() %>% arrange(desc(n)) %>% head(top) %>%collect()
  return(data)
}

# compute top cards by trading volume
top_volume_card = function(card_db, year=NULL, month=NULL, day=NULL, top=10){
  data = card_db
  if (!is.null(year))
    data  = data %>% filter(Year==year)
  if (!is.null(month))
    data  = data %>% filter(Month==month)
  if (!is.null(day))
    data  = data %>% filter(Day==day)
  data = data %>% group_by(cardName) %>% count() %>% arrange(desc(n)) %>% head(top) %>%collect()
  return(data)
}

# compute top cards by average price
top_price_card = function(card_db, year=NULL, month=NULL, day=NULL, top=10){
  data = card_db
  if (!is.null(year))
    data  = data %>% filter(Year==year)
  if (!is.null(month))
    data  = data %>% filter(Month==month)
  if (!is.null(day))
    data  = data %>% filter(Day==day)
  data = data %>% group_by(cardName) %>% summarize(avgPrice=mean(Price, na.rm=T),.groups='keep') %>% arrange(desc(avgPrice)) %>% head(top) %>%collect()
  return(data)
}

# compute top sets by average price
top_price_set = function(card_db, year=NULL, month=NULL, day=NULL, top=10){
  data = card_db
  if (!is.null(year))
    data  = data %>% filter(Year==year)
  if (!is.null(month))
    data  = data %>% filter(Month==month)
  if (!is.null(day))
    data  = data %>% filter(Day==day)
  data = data %>% group_by(setName) %>% summarize(avgPrice=mean(Price, na.rm=T),.groups='keep') %>% arrange(desc(avgPrice)) %>% head(top) %>%collect()
  return(data)
}

# obtain data for plotting price trend
avg_price_trend = function(card_db, lag=7){
  all_cards=card_db %>% group_by(SaleDateStr) %>% summarize(avgPrice=mean(Price, na.rm=T)) %>% collect()%>% arrange(SaleDateStr)
  return(all_cards)
  
}

# plot average pokemon card price trend
avg_price_plot = function(avg_price_trend_data){
  p = avg_price_trend_data %>% mutate(Date=as.POSIXct(SaleDateStr, format="%Y-%m-%d", tz="UTC"))%>%ggplot() + geom_line(aes(x=Date, y=avgPrice))+
    scale_y_log10() + labs(title='Avg Pokemon Card Price', y='Avg Price ($)')+theme(plot.title = element_text(hjust = 0.5))+theme_minimal()
  return(p)
}

# obtain data for plotting volume trend
avg_volume_trend = function(card_db, lag=7){
  all_cards=card_db %>% group_by(SaleDateStr) %>% count() %>% collect()%>% arrange(SaleDateStr) %>% ungroup() 
  return(all_cards)
}

# plot Pokemon cards average volume trend
avg_volume_plot = function(avg_volume_trend){
  p = avg_volume_trend %>% mutate(Date=as.POSIXct(SaleDateStr, format="%Y-%m-%d", tz="UTC"))%>%ggplot() + geom_line(aes(x=Date, y=n))+labs(title='Avg Pokemon Card Traded Volume', y='Trades')+theme(plot.title = element_text(hjust = 0.5))+theme_minimal()
  return(p)
}

# obtain image link for cards given card names
retrieve_card_img = function(card_db, card_names){
  return(card_db %>% filter(cardName %in% card_names) %>% distinct(img) %>% collect() %>%pull(img) %>% as.vector() )
}

# obtain set icons given set names
retrieve_set_img = function(card_db, set_names){
  return(card_db %>% filter(setName %in% set_names) %>% distinct(setIcon) %>% collect() %>%pull(setIcon) %>% as.vector() )
}

# obtain data from pokemon.sqlite databse 
card_search = function(card_db, set=NULL, holo=F, first_ed=F, RF=F, shining=F){
  data = card_db 
  if (!is.null(set)){
    data = data %>% filter(setName==set)
  }
  if (holo){
    data = data %>% filter(Holo==holo)
  }
  if (first_ed){
    data = data %>% filter(`FirstEdition`==first_ed)
  }
  if (RF){
    data = data %>% filter(`ReverseFoil`==RF)
  }
  if (shining){
    data = data %>% filter(Shining==shining)
  }
  data = data %>% group_by(setName, cardName) %>% summarise(Trades=n(), 
                                                            `Avg price`=mean(Price, na.rm=T),
                                                            `Min price` = min(Price, na.rm=T),
                                                            `Max price` = max(Price, na.rm=T),.groups='keep') %>% collect()%>%
    rename(Set=setName, Card=cardName)
  return(data)
}

# compute a pie chart of trading volume by card sets
volume_pie_chart_by_sets = function(card_db, year=NULL, month=NULL, day=NULL){
  data = card_db
  if (!is.null(year))
    data  = data %>% filter(Year==year)
  if (!is.null(month))
    data  = data %>% filter(Month==month)
  if (!is.null(day))
    data  = data %>% filter(Day==day)
  data = data %>% group_by(setName) %>% summarise(total_volume = n()) %>% arrange(desc(total_volume)) %>% collect()
  data_remain = data[-c(1:5),] %>% summarise(setName="other",total_volume=sum(total_volume))
  data = rbind(data[1:5,],data_remain)%>%rename(`Card Set` = setName, `Trade Volume` = total_volume)
  
  p = ggplot(data, aes(x = "", y = `Trade Volume`, fill = `Card Set`)) +
    geom_bar(width = 1, size = 1, stat = "identity") + 
    coord_polar("y", start=0)+
    theme_minimal()+
    theme(
      axis.title.y = element_blank(),
      axis.text.x=element_blank(),
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank(),
      plot.title=element_text(size=14, face="bold"),
      legend.position="bottom"
    )+
    scale_fill_brewer(palette="Blues") +
    guides(color = guide_legend(override.aes = list(size = 6)))+
    labs(title="Numebr of cards traded by sets")
  return(p)
}

# compute a pie chart of traded car values by card sets
value_pie_chart_by_sets = function(card_db, year=NULL, month=NULL, day=NULL){
  data = card_db
  if (!is.null(year))
    data  = data %>% filter(Year==year)
  if (!is.null(month))
    data  = data %>% filter(Month==month)
  if (!is.null(day))
    data  = data %>% filter(Day==day)
  data = data %>% group_by(setName) %>% summarise(total_value = sum(Price)) %>% arrange(desc(total_value)) %>% collect()
  data_remain = data[-c(1:5),] %>% summarise(setName="other",total_value=sum(total_value))
  data = rbind(data[1:5,],data_remain) %>% rename(`Card Set` = setName, `Traded Value`=total_value)
  p = ggplot(data, aes(x = "", y = `Traded Value`, fill = `Card Set`)) +
    geom_bar(width = 1, size = 1, stat = "identity") + 
    coord_polar("y", start=0)+
    theme_minimal()+
    theme(
      axis.title.y = element_blank(),
      axis.text.x=element_blank(),
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank(),
      plot.title=element_text(size=14, face="bold"),
      legend.position="bottom"
    )+
    scale_fill_brewer(palette="Blues") +
    guides(color = guide_legend(override.aes = list(size = 6)))+
    labs(title="Total traded value by sets")
  return(p)
}

# density plot of card prices by card sets
density_plot_by_set = function(card_db, year=NULL, month=NULL, day=NULL){
  data = card_db
  if (!is.null(year))
    data  = data %>% filter(Year==year)
  if (!is.null(month))
    data  = data %>% filter(Month==month)
  if (!is.null(day))
    data  = data %>% filter(Day==day)
  top_sets = data %>% group_by(setName) %>% summarise(total_volume = n()) %>% arrange(desc(total_volume)) %>% head(5)%>% collect() %>% pull(setName)
  top_data = data %>% filter(setName %in% top_sets) %>% select(setName, Price)%>%collect()
  data_rest = data %>% filter(!(setName %in% top_sets)) %>% select(setName, Price)%>%collect()
  data_rest$setName = 'others'
  data = rbind(top_data, data_rest) %>% mutate(logPrice=log(Price)) %>% select(c(setName, logPrice)) %>%
    mutate(`Set` = setName, `Log(Price)`=logPrice)
  p = data %>% ggplot() + geom_density(aes(`Log(Price)`, fill=`Set`), alpha=0.5)+
    theme_minimal()+
    theme(
      panel.border = element_blank(),
      panel.grid=element_blank(),
      plot.title=element_text(size=14, face="bold"),
      legend.position="bottom"
    )+labs(title='Density of log(Price) by sets')
  
  return(p)
  
}

# trend plot of cards filter by a specific pokemon
trend_plot_by_pokemon = function(card_db, pokemon_name, max_cards=5){
  top_vol_cards =  card_db %>% filter(pokemon==pokemon_name) %>% group_by(cardName) %>% summarise(vol = n())%>%
    arrange(desc(vol))%>%head(max_cards)%>%collect()%>%pull(cardName)
  
  data = card_db %>% filter(pokemon==pokemon_name) %>% filter(cardName %in% top_vol_cards)%>%
    group_by(SaleDateStr, cardName) %>% summarise(avgPrice=mean(Price)) %>% collect()
  p = data %>% mutate(temp_date = as.POSIXct(SaleDateStr, format="%Y-%m-%d", tz="UTC")) %>%
    ggplot() +
    geom_line(aes(x=temp_date, y=avgPrice, color=cardName))+
    scale_y_log10()+
    labs(y='Avg Price ($)', x='Date')+
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5),
          legend.position="bottom")
  return(p)
}

# price density plots of different cards for a specific pokemon
boxplot_by_pokemon = function(card_db, pokemon_name, max_cards=5){
  top_vol_cards =  card_db %>% filter(pokemon==pokemon_name) %>% group_by(cardName) %>% summarise(vol = n())%>%
    arrange(desc(vol))%>%head(max_cards)%>%collect()%>%pull(cardName)
  
  non_pokemon_label = paste0("Non-'", pokemon_name, "' Cards")
  data = card_db %>% mutate(pokemon = case_when(pokemon!=pokemon_name ~ non_pokemon_label,
                                                TRUE ~ cardName)) %>% filter(pokemon==non_pokemon_label | pokemon %in% top_vol_cards) %>% collect()
  p = data %>% ggplot() + geom_density(aes(log(Price), fill=pokemon), alpha=0.3) + 
    theme(plot.title = element_text(hjust = 0.5),legend.position="bottom")
  return(p)
}

# compute prices of pokemon cards for a specific pokemon
poke_table = function(card_db, pokemon_name){
  data = card_db %>% filter(pokemon == pokemon_name) %>% group_by(cardName) %>%
    summarise(avgPrice=round(mean(Price),1),
              medianPrice=  round(median(Price),1),
              minPrice = round(min(Price),1),
              maxPrice = round(max(Price),1),
              `Grade 1-7 Population` = sum(Grade<8),
              `Grade 8-10 Population` = sum(Grade>=8))%>%ungroup() %>% collect()
  return(data)
}

# create links for redirecting to card page
create_links = function(cardNames, linkLabel){
  cardNames_link = purrr::map_chr(
    seq_len(length(cardNames)),
    function(i) {
      actionLink(paste0(linkLabel,i), cardNames[i],) %>% as.character()
    }
  )
  return(cardNames_link)
}

# create image gallery
create_img_gallery = function(card_img_links){
  ui = fluidRow(purrr::map(
    seq_len(length(card_img_links)),
    function(j) {list(column(width=3,offet=0,img(src=card_img_links[j], height="100%", width="100%", align="right")))}
  ))
  return(ui)
}
# Database connection
con = DBI::dbConnect(RSQLite::SQLite(), "./data/pokemon.sqlite")
card_db = tbl(con, "card")

# find min & max year
max_year = card_db %>%  select(Year) %>% distinct() %>% collect() %>% pull(Year) %>% .[1] %>% as.numeric()
min_year= card_db %>%  select(Year) %>% distinct() %>% collect() %>% pull(Year) %>%as.numeric()%>% sort() %>% .[1] 
# all distinct pair of sets and cards
all_set_card_pair = card_db %>% distinct(setName, cardName) %>% collect()
all_sets = all_set_card_pair %>% distinct(setName)%>% pull(setName)
all_cards = all_set_card_pair %>% distinct(cardName) %>% pull(cardName)
# all first generation pokemon names
all_pokemons = card_db %>% distinct(pokemon) %>% pull() %>% sort()

# Initialize plots that would not be updated
price_trend_plot = avg_price_plot(avg_price_trend(card_db, lag=7))
volume_trend_plot = avg_volume_plot(avg_volume_trend(card_db, lag=7))

shinyApp(
  ui = fluidPage(
    tags$style("#select ~ .selectize-control .selectize-input {max-height: 80px;max-width: 180px}"),
    titlePanel("Pokemon Card Explorer"),
    
    mainPanel(width=10,
              tabsetPanel(id='tabset',
                          # Includes hottest cards and their prices
                          tabPanel("Hottest", 
                                   fluidRow(column(width=3,selectInput(inputId = "market_year", 
                                                                       label = "Year",
                                                                       choices = c('All', max_year:min_year))),
                                            
                                            column(width=3,selectInput(inputId = "market_month",
                                                                       label = "Month", choices = c('All', 1:12)))),
                                   
                                   fluidRow(column(width=3,
                                                   tableOutput('market_top_price_set')%>% withSpinner(type=8)),
                                            column(width=3,
                                                   tableOutput('market_top_price_card')%>% withSpinner(type=8)),
                                            column(width=3,
                                                   tableOutput('market_top_volume_card')%>% withSpinner(type=8)),
                                            column(width=3,
                                                   tableOutput('market_top_volume_set')%>% withSpinner(type=8))),
                                   
                                   h4('Hottest Cards'),
                                   
                                   div(style = 'overflow-x: scroll;',uiOutput('market_hottest')%>% withSpinner(type=8)),
                                   
                                   h4('Most Expensive Cards'),
                                   
                                   div(style = 'overflow-x: scroll;',uiOutput('market_exp')%>% withSpinner(type=8))),
                          # Include analytical plots of card sets
                          tabPanel("Set Analysis", 
                                   h4('Trend'),
                                   
                                   plotOutput('market_trend_price',width = "80%",height = "200px")%>% withSpinner(type=8),
                                   
                                   plotOutput('market_trend_volume',width = "80%",height = "200px")%>% withSpinner(type=8),
                                   fluidRow(column(width=3,selectInput(inputId = "prop_year",
                                                                       label = "Year", 
                                                                       choices = c('All', max_year:min_year))),
                                            
                                            column(width=3,selectInput(inputId = "prop_month",
                                                                       label = "Month",
                                                                       choices = c('All', 1:12)))),
                                   
                                   h4('Visualization'),
                                   fluidRow(column(width=5,
                                                   plotOutput("volume_pie",width = "80%")%>% withSpinner(type=8)),
                                            column(width=5,
                                                   plotOutput("value_pie",width = "80%")%>% withSpinner(type=8))),
                                   
                                   p(''),
                                   
                                   plotOutput('set_density',width = "80%")%>% withSpinner(type=8),
                                   
                                   p('')),
                          # incldue analytical plots of pokemon
                          tabPanel("Pokemon Analysis",
                                   fluidRow(column(width=3,selectInput(inputId = 'poke_set',
                                                                       label='Pokemon Select',
                                                                       choices=all_pokemons)),
                                            column(width=3, actionButton(inputId = 'pokemon_update', 'Update',
                                                                         style = 'margin-top:25px'))),
                                   div(style = 'height: 250px; overflow: scroll; font-size: 95%',
                                       align = "left",
                                       tableOutput("poke_table")%>% withSpinner(type=8)),
                                   uiOutput('poke_title_gallery'),
                                   div(style = 'overflow-x: scroll;',uiOutput('poke_gallery')%>% withSpinner(type=8)),
                                   uiOutput('poke_title_trend'),
                                   plotOutput("poke_trend",width = "100%",height = "300px") ,
                                   plotOutput("poke_box",width = "100%",height = "300px")),
                          # to show detailed trades, trend, prices of a card
                          tabPanel("Card", 
                                   fluidRow(column(width=3,selectInput(inputId = 'set_select',
                                                                       label='Set', choices=c('All', all_sets))),
                                            column(width=3,selectInput(inputId = 'card_select',
                                                                       label='Card', choices=all_cards)),
                                            column(width=3, actionButton(inputId = 'card_update', 
                                                                         label='Update',style = 'margin-top:25px')),),
                                   fluidRow(column(width=4, uiOutput("card_img")%>% withSpinner(type=8)),
                                            column(width=3,  div(style = 'font-size: 95%',tableOutput("card_info"))),
                                            column(width=4, div(style = 'font-size: 95%',tableOutput("card_table"))),),
                                   plotOutput("card_price",width = "80%",height = "180px")%>% withSpinner(type=8),
                                   uiOutput('card_trades_title'),
                                   tableOutput('card_trades')%>% withSpinner(type=8)),
                          
                          # to search for cards and redirect to "Card" tab
                          tabPanel("Search Card", 
                                   shiny::sidebarLayout(
                                     sidebarPanel(
                                       selectInput(inputId = "data_set", label = "Set", choices = c('All',all_sets)),
                                       checkboxInput(inputId = "data_holo", label = "Holo",value = F),
                                       checkboxInput(inputId = "data_1st", label = "1st Edition",value = F),
                                       checkboxInput(inputId = "data_reverse_foil", label = "Reverse Foil",value = F),
                                       checkboxInput(inputId = "data_shining", label = "Shining",value = F),
                                       actionButton(inputId = 'data_update', label='Update'),
                                       p(''),
                                       textInput(inputId='data_filter', label='Dynamic card name filter'),
                                       p(''),
                                       sliderInput(inputId = 'data_max_row', label = 'Max results (dynamic)',min=10, max=300, value = 100),
                                       
                                     ),
                                     mainPanel(
                                       div(style = 'height: 1000px; overflow: scroll; font-size: 95%', align = "left", tableOutput('data_table')%>% withSpinner(type=8))
                                     )
                                   )),
              ), style='width: 1000px; height: 1000px'
    )
  ),
  
  server = function(input, output, session) {
    state = reactiveValues(
      observers = list(), # observers for links in "Search Card" tab
      frontpage_observerse = list(), # observers for links in "Hottest" tab
      pokemon_page_observers = list(), # observers for links in "Pokemon Analysis" tab
      market_year = NULL, # year selected for "Hottest" tab
      market_month=NULL, # month selected for "Hottest" tab
      card_search_table=data.frame(), # for storing search results in "Search Card" tab
      card_output_tbl = data.frame(), # for showing filtered results in "Search Card" tab
      card_page_selected = NULL, # which pokemon to show in "Pokemon Anlayais" tab
    )
    
    # initialize Ui to empty
    output$market_trend_price = renderPlot(price_trend_plot)
    output$market_trend_volume = renderPlot(volume_trend_plot)
    output$card_img = renderUI({'No cards selected'})
    output$card_table = renderUI({})
    output$card_price = renderUI({})
    output$card_table = renderUI({})
    output$card_trades = renderUI({})
    output$data_table = renderUI({'No Results'})
    output$poke_table = renderUI({'No pokemon selected'})
    output$poke_gallery = renderUI({})
    output$poke_box = renderUI({})
    output$poke_trend = renderUI({})
    poke_title_gallery = renderUI({})
    
    
    # Update 'state$card_page_selected' when update button is click
    observe({
      state$card_page_selected = input$card_select
    }) %>% bindEvent(input$card_update)
    
    # Update "Card" page content to that card 'state$card_page_selected'
    observe({
      card = state$card_page_selected
      # retrieve data for this card
      data = card_db %>% filter(cardName==card) %>% select(c(setName,cardName, SaleDateStr, Grade,
                                                             Price, AuctionID,SellerID, PSACertNumber,
                                                             SaleType, img, Holo, `FirstEdition`)) %>% collect()
      data = data %>% mutate(SaleDateStr = as.Date(SaleDateStr))
      img = data %>% distinct(img) %>% pull(img) %>% .[1] # retrieve image for this card
      
      # table to output on "Card" tab
      tbl = data %>% group_by(Grade) %>% summarise(Number=n(), `Average Price`=round(mean(Price),2))
      info = c(data$cardName[1],data$setName[1], round(mean(data$Holo),1), round(mean(data$`FirstEdition`),2))
      info = data.frame(info =  c('Name', 'Set', '% Holo', '% 1st Edition'), value=info)
      output$card_img = renderUI(img(src=img, height="90%", width="90%", align="right"))
      output$card_table = renderTable(tbl)
      output$card_info = renderTable(info)
      
      # Price trend plot on "Card" tab
      p = data %>% group_by(SaleDateStr)%>%summarise(Price=mean(Price))%>%ggplot()+
        geom_line(aes(x=SaleDateStr, y=Price), color='blue')+
        scale_y_log10()+ 
        theme_minimal()+
        theme(
          panel.border = element_blank(),
          panel.grid=element_blank(),
          plot.title=element_text(size=14, face="bold"),
          legend.position="bottom"
        )+labs(x='Date', title='Price Trend')
      output$card_price= renderPlot(p)
      
      # Table for recent trades on "Card" tab
      output$card_trades = renderTable(data %>% head(10)%>% select(SaleDateStr, Grade, Price, AuctionID, PSACertNumber, SellerID, SaleType)%>%mutate(SaleDateStr=as.character(SaleDateStr))%>%rename(Date=SaleDateStr))
      output$card_trades_title = renderUI({'Recent trades'})
      
    }) %>% bindEvent(state$card_page_selected)
    
    
    # Update "Set Analysis" tab pie chart when SelectInput year & month are changed
    observe({
      pie_year = input$prop_year
      pie_month = input$prop_month
      if (pie_year=='All')
        pie_year = NULL
      if (pie_month=='All'){
        pie_month = NULL
      }else{
        pie_month =str_sub(as.character(as.numeric(pie_month)+100), start=-2)
      }
      output$volume_pie = renderPlot(volume_pie_chart_by_sets(card_db = card_db,
                                                              year = pie_year,
                                                              month=pie_month))
      output$value_pie = renderPlot(value_pie_chart_by_sets(card_db = card_db,
                                                            year = pie_year,
                                                            month=pie_month))
      output$set_density = renderPlot(density_plot_by_set(card_db = card_db,
                                                          year = pie_year,
                                                          month=pie_month))
    })
    
    # Update "Pokemon Analysis" tab to a specfici pokemon when update button is clicked
    observe({
      pokemon_name = input$poke_set
      # price trend plot
      output$poke_trend = renderPlot(trend_plot_by_pokemon(card_db = card_db,
                                                           pokemon_name = pokemon_name))
      # density plot
      output$poke_box = renderPlot(boxplot_by_pokemon(card_db = card_db,
                                                      pokemon_name = pokemon_name))
      # card prices table
      tbl = poke_table(card_db, pokemon_name)
      cardNames = tbl$cardName
      
      # create links to redirect to "Card" page when clicked
      tbl$cardName = create_links(cardNames, linkLabel = 'link_c')
      output$poke_table = renderTable(tbl, sanitize.text.function = function(x){x})
      
      # destroy observers
      purrr::walk(
        state$pokemon_page_observers,
        ~ .x$destroy()
      )
      
      # Create new observers to links just created
      state$pokemon_page_observers = purrr::map(seq_len(length(cardNames)), function(i) {
        label = paste0("link_c",i)
        observeEvent(input[[label]], ignoreInit = TRUE, {
          # to do when click
          updateTabsetPanel(session, inputId = 'tabset', selected = 'Card')
          updateSelectInput(inputId = 'set_select', selected = 'All')
          updateSelectInput(inputId = 'card_select', selected =cardNames[i])
          state$card_page_selected = cardNames[i]
        })})
      
      
      # Update Gallery
      imgs = retrieve_card_img(card_db, cardNames)
      output$poke_gallery = renderUI(create_img_gallery(imgs[1:min(4, length(imgs))]))
      
      # Update titles
      output$poke_title_trend = renderUI(h4('Price trend'))
      output$poke_title_gallery = renderUI(h4('Gallery'))
      
    }) %>% bindEvent(input$pokemon_update)
    
    # Update "Card" tab card selection choices to cards in the set. When new set is selected, this changes
    # selection in cards.
    observe({
      if (input$set_select=='All'){
        new_choices = all_cards
      }else{
        new_choices = all_set_card_pair %>% filter(setName==input$set_select)%>%pull(cardName)
      }
      updateSelectInput(inputId = 'card_select', choices=new_choices)
    })%>% bindEvent(input$set_select)    
    
    # "Search Card" tab filter, takes 'state$card_search_table', filter it and update
    # 'state$card_output_tbl' which will be rendered
    output_data_change = reactive({list(input$data_filter ,state$card_search_table,input$data_max_row)})
    observe({
      tbl = state$card_search_table
      # if empty table or filter is empty, render empty table
      if (dim(tbl)[1]==0){
        state$card_output_tbl = data.frame()
      }else{
        
        if (input$data_filter != ''){
          tbl = tbl %>% filter(str_detect(Card, input$data_filter))
        }
        if (dim(tbl)[1]==0){
          state$card_output_tbl = data.frame()
        }else{
          if (dim(tbl)[1] > input$data_max_row){
            # for controlling maximum results to show
            tbl = tbl %>% head(input$data_max_row)
          }
          state$card_output_tbl = tbl
        }
      }
    }) %>% bindEvent(output_data_change())
    
    
    # Update "Card Search" output table by rendering 'state$card_output_tbl' 
    observe({
      
      # destroy observers the links
      purrr::walk(
        state$observers,
        ~ .x$destroy()
      )
      
      # grab table to render
      tbl = state$card_output_tbl
      if (dim(tbl)[1]==0){
        output$data_table = renderUI({'No Results'})
      }else{
        
        card_names = tbl %>% pull(Card)
        
        # create links for card names, which will redirect to that card on "Card" tab
        tbl$Card = create_links(card_names, 'link')
        
        # observers for the links
        state$observers = purrr::map(seq_len(length(card_names)), function(i) {
          label = paste0("link",i)
          observeEvent(input[[label]], ignoreInit = TRUE, {
            # to do when click
            updateTabsetPanel(session, inputId = 'tabset', selected = 'Card')
            updateSelectInput(inputId = 'set_select', selected = 'All')
            updateSelectInput(inputId = 'card_select', selected = card_names[i])
            state$card_page_selected = card_names[i]
          })})
        
        output$data_table = renderTable(tbl, sanitize.text.function = function(x){x})
      }
    }) %>% bindEvent(state$card_output_tbl)
    
    # "Card Search" tab "Update" button
    # change 'state$card_search_table" when button is clicked, which will then be filtered
    # to generate 'state$card_output_table", which will be rendered
    observeEvent(input$data_update, {
      # if set is All, get results for all card sets
      set = input$data_set
      if (set=='All'){
        set = NULL
      }
      # retreive results
      data = card_search(card_db, set=set,
                         holo=input$data_holo,
                         first_ed=input$data_1st,
                         RF=input$data_reverse_foil,
                         shining=input$data_shining)
      state$card_search_table = data
    })
    
    
    # "Hottest" tab inputs, observe any changes in selectInput
    market_inputs_to_listen = reactive({
      list(input$market_year, input$market_month)})
    
    # Update "Hottest" tab on input changes
    observeEvent(market_inputs_to_listen(), {
      year = input$market_year
      month = input$market_month
      # If 'All', grab all results
      if (year=='All'){
        state$market_year = NULL
      }else{
        state$market_year = year
      }
      if (month=='All'){
        state$market_month = NULL
      }else{
        state$market_month = as.character(as.numeric(month)+100) %>% str_sub(.,-2)
      }
      
      # destroy old observers for links in "Hottest" page
      purrr::walk(
        state$frontpage_observers,
        ~ .x$destroy()
      )
      
      # grab data for top price, top volume sets & cards 
      set_top_price = top_price_set(card_db, year=state$market_year, month=state$market_month, top=5)%>%
        rename('Top Set'=setName, Price=avgPrice) %>% mutate(Price=  as.integer(Price))
      card_top_price = top_price_card(card_db, year=state$market_year, month=state$market_month, top=5)%>%
        rename(`Top Card`=cardName, Price=avgPrice)%>% mutate(Price=  as.integer(Price))
      set_top_volume = top_volume_set(card_db, year=state$market_year, month=state$market_month, top=5)%>%
        rename('Top Set'=setName, Volume=n)
      card_top_volume = top_volume_card(card_db, year=state$market_year, month=state$market_month, top=5)%>%
        rename(`Top Card`=cardName, Volume=n)
      
      # create action links for top cards
      card_names_a = card_top_price$`Top Card`
      card_name_ui_a = create_links(card_names_a, 'link_a')
      card_top_price$`Top Card` = card_name_ui_a
      
      # create action links for top cards
      card_names_b =  card_top_volume$`Top Card`
      card_name_ui_b = create_links(card_names_b, 'link_b')
      card_top_volume$`Top Card` = card_name_ui_b
      
      # set new observers for the newly created links
      state$frontpage_observers = purrr::map(seq_len(length(card_names_a)), function(i) {
        label = paste0("link_a",i)
        observeEvent(input[[label]], ignoreInit = TRUE, {
          # to do when click
          updateTabsetPanel(session, inputId = 'tabset', selected = 'Card')
          updateSelectInput(inputId = 'set_select', selected = 'All')
          updateSelectInput(inputId = 'card_select', selected = card_names_a[i])
          state$card_page_selected = card_names_a[i]
        })})
      
      # set new observers for the newly created links
      state$frontpage_observers = list(state$frontpage_observers, 
                                       purrr::map(seq_len(length(card_names_b)), function(i) {
                                         label = paste0("link_b",i)
                                         observeEvent(input[[label]], ignoreInit = TRUE, {
                                           # to do when click
                                           updateTabsetPanel(session, inputId = 'tabset', selected = 'Card')
                                           updateSelectInput(inputId = 'set_select', selected = 'All')
                                           updateSelectInput(inputId = 'card_select', selected = card_names_b[i])
                                           state$card_page_selected = card_names_b[i]
                                         })}))
      
      # render table
      output$market_top_price_set = renderTable(set_top_price)
      output$market_top_price_card = renderTable(card_top_price, sanitize.text.function = function(x){x})
      output$market_top_volume_set = renderTable(set_top_volume)
      output$market_top_volume_card = renderTable(card_top_volume, sanitize.text.function = function(x){x})
      
      top_cards = card_names_b%>% .[1:min(4, length(.))]
      
      # obtain links & render image gallery for "Hottest Cards"
      card_img_link = retrieve_card_img(card_db, top_cards) %>% .[1:min(4, length(top_cards))]
      output$market_hottest = renderUI(create_img_gallery(card_img_link))
      top_cards_exp = card_names_a %>% .[1:min(4, length(.))]
      exp_card_link = retrieve_card_img(card_db, top_cards_exp) 
      exp_card_ui =  fluidRow(purrr::map(
        seq_len(length(exp_card_link)),
        function(j) {list(column(width=3,offet=0,img(src=exp_card_link[j], height="100%", width="100%", align="right")))}
      ))
      output$market_exp = renderUI(exp_card_ui)
      
    })
    
    
  }
)
