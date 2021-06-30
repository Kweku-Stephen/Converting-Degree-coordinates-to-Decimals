require(magrittr)

#Data Import and justa little cleaning
data <- readxl::read_excel(
  "C:/Users/Qet/Downloads/Profiling_All_location_Stephen_Convert_to_Decimals (1).xlsx"
) |> 
  sapply(
    \(vector = "") gsub("\\*", "°", vector) |> strsplit("\"")
  ) |> as.data.frame()



#Conversion function to convert from degree minutes to decimals in the form
# eg. N9°23'17.3256''
dc_dg <- function(x = ""){
  
  #if(class(x) == "list") unlist(x) -> x
  
  #Checking for elements with N, S, W, E
  ifelse (
    x %in% grep("^(N|S|W|E)", x, value = T),
    {
      unlist(strsplit(x, "^(N|S|W|E)")) -> s
      s = data.frame(s = s[!s == ""])
      
      s = tidyr::separate(s, s, c("Dg","mn"), sep = "°") |>
        tidyr::separate(mn, c("mn","s"), sep = "'") |>
        sapply(as.numeric)
      
      if(x %in% grep("^(N|S)", x, value = TRUE)) {
        s = (s[1]) + (s[2]/60) + (s[3]/3600)
      } else {
        s = paste0("-", (s[1]) + (s[2]/60) + (s[3]/3600))
      }
      
      s
    },
    
    x
  ) 
  
}


#Adding the computed results "lat" ,"long" to the data
data %<>% within(
  #data,
  {
    lat = sapply(data[ ,5], dc_dg)
    long = sapply(data[ ,6], dc_dg)
  }
) |> 
  sapply(as.character) 

#writing to disk
write.csv(data, file = "data.csv", row.names = F)
