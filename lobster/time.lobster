#!/usr/bin/env lobster

// Use with the vector returned from the built-in date_time()
// enum date_time:
//     dt_epoch    // unix epox (secs since 1/1/1970 0:00:00 UTC)
//     dt_year     // 1900 is 0
//     dt_month    // Jan is 0
//     dt_day      // First day of the month is 0
//     dt_yearday  // Jan 1 is 0
//     dt_weekday  // Sun is 0
//     dt_hour     // Midnight is 0
//     dt_min
//     dt_sec
print(date_time(true))

// format date time
print(date_time_string(true))

// custom format date time
print(date_time_string_format("%Y-%m-%dT%H:%M:%S%z",true))

