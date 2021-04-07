library(httr)

getDepartement_by_long_lat <- function(long, lat){
  url <- paste("https://api-adresse.data.gouv.fr/reverse/?lon=",long, "&lat=",lat, sep = "")
  result <- fromJSON(url)
  return(result)
}
getDepartement_by_long_lat(2.37,48.357)


# récupération des long et lat pour le géocodage inversé de masse
caracteristiques.2018 %>% select(lat,long) %>% readr::write_delim(file = "data/file2018.csv", delim = ",")

#curl -X POST -F data=@path/to/file.csv https://api-adresse.data.gouv.fr/reverse/csv/
  
getGeocodage <- function(file){
    tmp <- tempfile()
    h <- new_handle()
    handle_setform(h,
                   data=form_file("data/file2018.csv", "@path/file2018.csv"))
    curl_download("https://api-adresse.data.gouv.fr/reverse/csv/",tmp, handle = h)
    readLines(tmp)
    
  }
getDepartement_by_long_lat(2.37,48.357)

#REF
# https://pasq.fr/geocodage-inverse-grace-a-la-ban
# http://gis.19327.n8.nabble.com/Geocodage-inverse-d-un-csv-avec-https-adresse-data-gouv-fr-api-td5941260.html