shipFun <- function(name = name, table = data$raw){

  # create basic table
  location <- table %>%
    as.data.frame() %>%
    filter(shipname == name) %>%
    mutate(datetime = as_datetime(datetime)) %>%
    arrange(desc(datetime)) %>%
    mutate(lon2 = lead(lon),
           lat2 = lead(lat)) %>%
    # ? not sure but i think the information is doubled sometimes, to verify with the documentation
    group_by(port) %>% 
    rowwise() %>%
    mutate(distance = as.numeric(distm(c(lon, lat), c(lon2, lat2), fun = distHaversine))) %>%
    ungroup()  %>%
    as.data.frame() %>%
    mutate(max_distance = ifelse(distance == max(distance, na.rm = T), 1, 0))
  
  if(sum(location$max_distance, na.rm = T) > 1){
    location <- location %>% 
      group_by(max_distance) %>% 
      mutate(max_distance = ifelse(max_distance == 1 & 
                                     datetime == max(datetime), 
                                   1, 0)) %>% 
      ungroup()
  }
  
  # keep only the ports that include the longest sailed distance
  ports <- unique(location$port)
  
  if(length(ports) > 1){
    
    port_ <- location %>% 
      filter(max_distance == 1) %>% 
      pull(port)
    
    location <- location %>% 
      filter(port == port_)
  }
  
  # split into 2 tables in order to use cumsum function later
  datetime_ <- location %>% 
    filter(max_distance == 1) %>% 
    pull(datetime)
  
  location_1 <- location %>% 
    filter(datetime >= datetime_)
  
  location_2 <- location %>% 
    filter(datetime < datetime_)
  
  # no positive information means that we dont have the info about start/end place
  if(sum(location_1$is_parked) > 0){
    
    location_1 <- location_1 %>% 
      arrange(datetime) %>% 
      mutate(parked_cs = cumsum(is_parked)) %>% 
      filter(parked_cs <= 1) %>% 
      select(-parked_cs)
    
  }
  
  if(sum(location_2$is_parked) > 0){
    
    location_2 <- location_2 %>% 
      arrange(desc(datetime)) %>% 
      mutate(parked_cs = cumsum(is_parked)) %>% 
      filter(parked_cs <= 1) %>% 
      select(-parked_cs)
  
  }
  
  # joining back 
  result <- rbind(location_1, 
                  location_2) %>% 
    arrange(desc(datetime)) %>% 
    mutate(distance = ifelse(datetime == min(datetime), 
                             0, distance),
           sail = ifelse(datetime == max(datetime), "Sail end.",
                         ifelse(datetime == min(datetime), "Sail start.", NA))) %>% 
    mutate(
      label = paste(
        "<p>",
        "Ship name:",
        shipname,
        "</p>",
        "<p>",
        "Ship type:",
        ship_type,
        "</p>",
        "<p>",
        "Destination:",
        destination,
        "</p>",
        "<p>",
        "Actual location:",
        sail,
        "</p>"
      )
    )
  
  # summary stats
  stats <- data.frame(
    td = sum(result$distance),
    tt = round(difftime(max(result$datetime), min(result$datetime), units = "hours"),2),
    ms = result %>% 
      filter(is_parked != 1) %>% 
      summarise(ms = round(mean(speed, na.rm = T),2),
                ms = ifelse(is.na(ms), 0, ms))
  ) %>% 
    rename(`Total distance [meters]` = td,
           `Total time [hours]` = tt,
           `Mean speed` = ms)
  
  
  return(list(result = result, 
              stats = stats))
  
}
