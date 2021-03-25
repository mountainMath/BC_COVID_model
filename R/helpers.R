extract_table_data <- function(node){
  h <- node %>% rvest::html_nodes("thead th") %>% rvest::html_text()
  rows <- node %>% rvest::html_nodes("tbody tr")
  data <- rows %>% lapply(function(d) d %>% 
                            rvest::html_nodes("td") %>% 
                            rvest::html_text() %>% 
                            t() %>% 
                            as.data.frame) %>%
    bind_rows() %>%
    setNames(h)
  
  data
}

ratio_to_share <- function(r)1/(1+1/r)
share_to_ratio <- function(s)1/(1/s-1)

get_voc_share <- function(initial_voc_share = 0.09,initial_date = as.Date("2020-02-27"),voc_advantage=0.08) {
  tibble(Date=seq(as.Date("2021-01-01"),as.Date("2021-06-01"),by="days")) %>%
    mutate(Week=strftime(Date,"%U")) %>%
    mutate(week=MMWRweek::MMWRweek(.$Date)$MMWRweek) %>%
    mutate(d=difftime(Date,initial_date,units="day") %>% as.integer()) %>%
    mutate(ratio=share_to_ratio(initial_voc_share)*exp(d*voc_advantage)) %>%
    mutate(share=ratio_to_share(ratio))
}

add_prediced_linear <- function(d,...){
  dd <- d %>% filter(!is.na(log_ratio))
  m.lm<-lm(log_ratio ~ Date, data=dd,...)
  d %>%
    left_join(tibble(p=predict(m.lm,new_data=dd$Date),Date=dd$Date),by="Date")
}

get_canada_vaccine_projections <- function(){
  url <- "https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection/prevention-risks/covid-19-vaccine-treatment/vaccine-rollout.html"
  
  tables <- xml2::read_html(url) %>% rvest::html_nodes("table")
  
  data_list <- tables %>% lapply(extract_table_data)
  
  
  bind_rows(
    data_list[[2]] %>% mutate(type="BioNTech"),
    data_list[[3]] %>% mutate(type="Moderna"),
    data_list[[4]] %>% mutate(type="AstraZeneca") #%>%
    #rename(`Distribution location`=`Distribution     location`) %>%
    #mutate(`Distribution location`=gsub(" +"," ",`Distribution location`))
  ) %>%
    pivot_longer(-one_of("Distribution location","type")) %>%
    mutate(week=as.Date(paste0(strsplit(name,"-") %>% map(last) %>% unlist," 2021"),format=c("%d %b %Y"))) %>%
    mutate(value=as.numeric(gsub(",","",value)))
  
}

get_ontario_voc_data <- function(){
  canada_sheet <- googlesheets4::sheets_get("https://docs.google.com/spreadsheets/d/1ZRcpDA0tn6ToxXcY9mNjr4feQJ_B3jQuHfLetOspSlA/edit#gid=0")
  
  googlesheets4::read_sheet(canada_sheet,"Ontario shares") %>%
    mutate(Date=as.Date(Date)) %>%
    mutate(share=as.numeric(Share)) %>%
    mutate(ratio=share_to_ratio(share)) %>%
    mutate(log_ratio=log(ratio))  
}

get_dk_data <- function(vintage="2021-03-17") {
  xml2::read_html(paste0("https://www.covid19genomics.dk/",vintage,"_data-overview.html")) %>% 
    rvest::html_nodes(xpath='//div[@id="b.1.1.7"]') %>% 
    rvest::html_nodes("a") %>%
    last() %>%
    rvest::html_attr("href") %>%
    gsub("data:text/csv;base64,","",.) %>%
    RCurl::base64Decode() %>%
    read_delim(delim=";",col_types = cols(.default="c")) %>%
    mutate(Year=substr(Week,1,4) %>% as.integer,
           week=substr(Week,7,8) %>% as.integer) %>%
    mutate(Date=MMWRweek::MMWRweek2Date(Year,week)) %>%
    mutate(percent=gsub(",",".",percent)) %>%
    mutate_at(c("yes","total","percent"),as.numeric) %>%
    select(Week,Date,Region,yes,total,percent) %>%
    mutate(ratio=yes/(total-yes)) %>%
    mutate(log_ratio=log(ratio+0.00001)) %>%
    mutate(day=difftime(Date,min(Date),units="day") %>% as.integer)  %>% 
    filter(Region=="Whole Denmark") %>%
    mutate(Region=="Denmark") %>%
    select(Date,Region,ratio, log_ratio) %>%
    mutate(share=ratio_to_share(ratio))
}

get_ch_data <- function(){
  
  ch_data1 <- read_csv("https://raw.githubusercontent.com/cevo-public/Quantification-of-the-spread-of-a-SARS-CoV-2-variant/main/data/data_variants_per_day_and_grossregion_viollier.csv")
  ch_data2 <- read_csv("https://raw.githubusercontent.com/cevo-public/Quantification-of-the-spread-of-a-SARS-CoV-2-variant/main/data/data_variants_per_day_risch.csv")
  
  bind_rows(ch_data1,ch_data2) %>%
    select(-region) %>%
    rename(Date=date) %>%
    group_by(Date) %>%
    summarise_all(sum) 
}

add_stl_trend_m <- function(c){
  #print(length(c))
  cc <- c %>%
    log() %>%
    ts(frequency = 7,start = as.numeric(format(Sys.Date(), "%j"))) %>% 
    stl(s.window=21,t.window=14) 
  
  as_tibble(cc$time.series) %>%
    mutate_all(exp)
}

