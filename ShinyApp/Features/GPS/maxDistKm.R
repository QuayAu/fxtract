maxDistKm = function(df) {
  gpsAvailable = df %>% filter(!is.na(latitude), !is.na(longitude), latitude >= 0, latitude <= 360, longitude >= 0, longitude <= 360)
  if (nrow(gpsAvailable) == 0) return(data.frame("max" = NA))
  x = max(distm(cbind(gpsAvailable$longitude,gpsAvailable$latitude))) / 1000
  data.frame("max" = x)
}