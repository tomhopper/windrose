windrose
========

An R package for creating rose plots from wind data that show both direction and speed. Based on code provided by Andy Clifton on StackExchange (http://stackoverflow.com/a/17266781/393354).

Install with

```
library(devtools)
install_github("tomhopper/windrose")
```

To create a wind rose plot:

```
data(wind_data)
wind_rose <- windrose(wind_data, spd = Wind_Speed_meter_per_second, dir = Wind_Direction_deg)
plot(wind_rose)
```

TO DO
=====

* Fix countmax
* Clean up output of summary()
