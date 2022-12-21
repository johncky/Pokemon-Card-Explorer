library(tidyverse)
library(shiny)
library(RSQLite)
library(rvest)

# extract data for each set, contains card name & card link for each card in the set
extract_set_info = function(set_api){
  res = tryCatch(httr::GET(set_api) %>% read_html(), error = function(e) NA)
  if (is.na(res) ){
    print('Error occured, retrying in 2s.')
    Sys.sleep(2)
    closeAllConnections()
    res = tryCatch(httr::GET(set_api) %>% read_html(), error = function(e) NA)
  }
  if (is.na(res)){
    print('Error occured again, aborting.')
    return(NA)
  }
  
  card_links = res %>% html_nodes('a') %>% html_attr("href") %>% paste0('https://www.pokemonprice.com', .)
  card_names = res %>% html_nodes('a') %>% html_text()
  closeAllConnections()
  return(list(cardName=card_names,
              cardLink=card_links))
}

# extract data for each card, contains data for the card
extract_card_info = function(card_link){
  paste0('Extracting Card:', card_link)
  
  res = tryCatch(httr::GET(card_link) %>% read_html(),error = function(e) NA)
  if (is.na(res) ){
    print('Error occured, retrying in 2s.')
    Sys.sleep(2)
    closeAllConnections()
    res = tryCatch(httr::GET(card_link) %>% read_html(),error = function(e) NA)
  }
  if (is.na(res)){
    print('Error occured again, aborting.')
    return(NA)
  }
  
  data = res %>% html_table(fill=T)
  if (length(data)==0){
    return(NA)
  }
  data =  data %>% .[[1]] %>% tibble()
  card_img = res %>% html_nodes('.singlecardimg') %>% html_attr('src')
  data = data %>% mutate(img = card_img)
  closeAllConnections()
  return(data %>% as.list())
}


# scrape all pokemon cards
scrape_pokecard = function(base_url= 'https://www.pokemonprice.com/Sets'){
  html = base_url %>% read_html()
  set_link = html %>% html_nodes(".pokemonsets") %>% html_nodes("a") %>% html_attr('href') %>% paste0('https://www.pokemonprice.com', .)
  
  set_name = set_link %>% strsplit("/+") %>% sapply( function(x) x[[length(x)]])
  
  set_icons = html %>% html_nodes(".pokemonsets") %>% html_nodes("a") %>% html_nodes("img") %>% html_attr('src') %>% paste0('https://www.pokemonprice.com', .)
  
  set_api = set_link %>% strsplit("/+") %>% sapply( function(x) x[length(x) - 1]) %>% paste0('https://www.pokemonprice.com/api/cardsforset/', .)
  
  set_cards = lapply(set_api, function(x){
    #print(paste0('Extracting Set:', x))
    return(extract_set_info(x))
  })
  
  data = tibble(setName=set_name %>% str_replace_all('-', ' '),
                setIcon = set_icons,
                setLink=set_api,
                setCards=set_cards)
  data = data %>% unnest_wider(setCards)  %>% unnest_longer(col=c(cardName, cardLink))
  
  all_cardData = list()
  chunks = split(data, (as.numeric(rownames(data))-1) %/% 200)
  for (i in 1:length(chunks)){
    # for (i in 1:2){
    print(paste0('Extracting chunk:', i))
    chunk = chunks[[i]]
    all_cardData = c(all_cardData, lapply(chunk %>% pull(cardLink), extract_card_info))
  }
  
  
  data = data %>% mutate(cardData = all_cardData) %>% unnest_longer(cardData,indices_to = 'cardDataType')
  data = data[!is.na(data$cardDataType), ]
  data = data %>% pivot_wider(names_from = cardDataType, values_from = cardData)
  names(data) = gsub("\\s+", "", names(data))
  
  data = data %>% unnest_longer(col=SaleDate:img) %>% mutate(
    SaleDate = as.Date(SaleDate, format =  "%Y-%m-%d"),
    Price =  as.double(str_replace_all(Price, "[$,]", "")))
  
  # create categorical variables 
  categories = c('Holo', 'GX', 'EX', 'Charizard', 'Pikachu', 'Blastoise', 'Energy',
                 'Shining','Vmax', 'V', 'Raichu', 'Dark', 'Venusaur', 'Mewtwo', 'Gyarados', 'Zapdos', 'Charmander')
  for (cat in categories){
    data[[cat]] = str_detect(data$cardName, cat)
  }
  
  data[['FirstEdition']] = str_detect(data$cardName, '1st Edition')
  data[['ReverseFoil']] = str_detect(data$cardName, 'Reverse Foil')
  
  data[['Year']] = data$SaleDate %>% format("%Y")
  data[['Month']] = data$SaleDate %>% format("%m")
  data[['Day']] = data$SaleDate %>% format("%d")
  data[['SaleDateStr']] = data$SaleDate %>% format("%Y-%m-%d")
  
  #Add gen1 pokemon names
  pokemon_names = read.csv("data/FirstGenPokemon.csv") %>% pull(Name)
  card_pokemon = rep(NA, dim(data)[1])
  for(i in pokemon_names){
    index = grepl(i, data$cardName)
    card_pokemon[which(index==TRUE)] = i
  }
  data = data %>% mutate(pokemon=card_pokemon) %>% replace_na(list(pokemon="other"))
  
  # save to database
  dir.create('/data')
  con = DBI::dbConnect(RSQLite::SQLite(), "./data/pokemon.sqlite")
  dbWriteTable(con, 'card', data, overwrite=T)
  dbDisconnect(con)
  return(data)
}

scrape_pokecard()

