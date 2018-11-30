maxDistKm = function(df) {
  gpsAvailable = df %>% filter(!is.na(latitude), !is.na(longitude), latitude >= 0, latitude <= 360, longitude >= 0, longitude <= 360)
  # Add check wether gps are available -> if not enter NA with warning message
  x = max(distm(cbind(gpsAvailable$longitude,gpsAvailable$latitude))) / 1000
  data.frame("max" = x)
}