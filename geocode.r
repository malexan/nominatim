#' Nominatim
#'
#' geocodes a location using OpenStreetMap.
#'
#' @param location a character string or list specifying a location of interest.
#' OpenStreetMap Nominatim can't deal directly with letters in house number in Russia (e.g. "14А, 
#' Новгородская, Псков"). To geocode such addresses you have to pass list, where first 
#' element is house number (with letters: e.g. "14А") and second element is the rest of 
#' the address ("Новгородская, Псков").
#' @param output verbose level
#' @param messaging turn messaging on/off
#' @return depends (at least a data.frame with variables lon and lat)
#' @author Alexander Matrunich \email{alexander@@matrunich.com}
#' @details Based on David Kahle's geocode.R from ggmap
#' @seealso \url{http://wiki.openstreetmap.org/wiki/Nominatim}
#' @export
#' @examples



# Номера с литерами надо обрабатывать - если литера есть в номере, то проверять все результаты геокодинга с поиском подходящего. Или все подряд проверять по совпадению строки до первой запятой
nominatim <- function(location, output = c('latlon','latlona','more','all'),
  messaging = FALSE) {

  require(RCurl)
  require(XML)
  require(stringr)
  require(plyr)

  house <- FALSE
  if (is.list(location)) {
    house <- TRUE
	  house_number <- location[[1]]
    # Extract first digits from house number
    house_number_fdig <- str_extract(house_number, '^[0-9]*')
    location <- str_c(house_number_fdig, location[[2]], sep = ',')
	}
  
  # Forming url
  location <- str_replace_all(location, ' ', '+')
  location <- URLencode(location)
  url_string <- str_c('http://nominatim.openstreetmap.org/search?format=json&addressdetails=1&q=', location)
    
  # Geocode
  if(messaging) message(str_c('contacting ', url_string, '...'), appendLF = F)
  # gc <- getURL(url_string)
  gc <- fromJSON(paste(readLines(url(url_string)), collapse = ''))
  if(messaging) message(' done.')
  return(gc)
  
  # Extract place with required house number
	gc <- xmlRoot(xmlTreeParse(gc, getDTD=F, useInternalNodes = T))
  # return(xmlToList(gc))

  if(house) {
    gc <- getNodeSet(gc, str_c('//place[house_number/text() = "', house_number, '"]'))
    if(output == 'all') return(xmlToList(gc[[1]]))
  }
  if(!house) {
    return(xmlToList(gc))
  }
  
  #format geocoded data
  
  
  return(gc)
  
}
