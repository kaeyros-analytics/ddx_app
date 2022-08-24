
file_path <- paste(path_data, "/testdata.rds", sep="")
file_path <- paste(path_data, "/us-states.js", sep="")
#file_us_state = paste(file_path, "/us_states.geojson", sep = '')
states <- geojsonio::geojson_read(file_path, what = "sp")
class(file_path)

filess = paste(getwd(), "/us_states.txt", sep = '')
states <- geojsonio::geojson_read(file, what = "sp")
file.exists(url_path)

# INPUT IS A URL (beginning from http..)

url_path = "https://raw.githubusercontent.com/mlampros/DataSets/master/california.geojson"
url_path = "https://leafletjs.com/examples/choropleth/us-states.js"

url_js = geojsonR::FROM_GeoJson(url_file_string = url_path)

write(url_path, "test.json")
library("rjson")
json_data <- rjson::fromJSON(file= filess)
