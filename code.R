#This was specifically designed for an uncertain group of elements to run the computation on
#So instead of the main function of this body of code to accept a vector of length > 1, it only
#accepts a vector of length 1 since some elements within the data fro the conversion may not
# may havebeen recorded or entered incorrectly ie. deviates from the this structure " N9°23'17.3256'' "

require(magrittr)

#Data Import and justa little cleaning
data <- readxl::read_excel(
  "C:/Users/Qet/Downloads/Profiling_All_location_Stephen_Convert_to_Decimals (1).xlsx"
) |> 
  sapply(
    \(vector = "") gsub("\\*", "°", vector) |> # instead of the degree symbol "°", the data comes with asterix in its stead, this pipe replaces that with the degree symbol
      strsplit("\"") #This extra pipe of "stringsplit" removes "\" from some elements of the data
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
system.time(data %<>% within(
  #data,
  {
    lat = sapply(data[ ,5], dc_dg) #applying the function dc_dg on the latitude column or in this case vector of the data
    long = sapply(data[ ,6], dc_dg) #applying the function dc_dg on the longitude column or in this case vector of the data
  }
)) |> 
  sapply(as.character) 

#writing to disk
write.csv(data, file = "data.csv", row.names = F)





#Parallelizing the same Compuation to test speed
Sys.setenv("_R_USE_PIPEBIND_" = "true")

require(parallel)

cl = makeCluster(2, type = "PSOCK")

system.time(data <- clusterApply(
  cl,
  list(
    lat = data[ ,5], # Lattitude column/vector
    long = data[ ,6] # Longitude column/vector
  ),
  sapply,
  dc_dg
) |> . =>
  do.call(
    cbind,
    .
  ) |> . =>
  within(
    data,
    .
  ))

stopCluster(cl)

