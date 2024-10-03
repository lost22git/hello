module [
    isLeapYear,
    unixToDays,
    unixToDaySeconds,
    daysToYMD,
    secondsToHMS,
    utcToRFC3339,
]

import pf.Utc

sPerMin = 60
sPerHour = 60 * sPerMin
sPerDay = 24 * sPerHour
msPerS = 1000
nsPerMs = 1_000_000
nsPerS = nsPerMs * msPerS

daysPer400Years = 365 * 400 + 97
daysPer100Years = 365 * 100 + 24
daysPer4Years = 365 * 4 + 1

daysPerMonth = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
daysPerMonthLeap = [0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

## check year if leap year or not
isLeapYear : U16 -> Bool
isLeapYear = \year -> (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)

## convert days to record {days, year, month, day, dayOfYear}
daysToYMD : Int _ -> { days : Int _, year : U16, month : U8, day : U8, dayOfYear : U16 }
daysToYMD = \daysTotal ->
    num400 = daysTotal // daysPer400Years
    daysRest = daysTotal - num400 * daysPer400Years

    num100 = daysRest // daysPer100Years |> \x -> if Num.compare x 4 == EQ then 3 else x
    daysRest2 = daysRest - num100 * daysPer100Years

    num4 = daysRest2 // daysPer4Years
    daysRest3 = daysRest2 - num4 * daysPer4Years

    numyears = daysRest3 // 365 |> \x -> if Num.compare x 4 == EQ then 3 else x
    daysRest4 = daysRest3 - numyears * 365

    year = num400 * 400 + num100 * 100 + num4 * 4 + numyears + 1970
    dayOfYear = daysRest4 + 1

    { days, month } =
        List.range { start: At 1, end: At 12 }
        |> List.walkUntil
            {
                days: daysRest4,
                month: 1,
                dpm: if (Num.compare numyears 3 == EQ) && ((Num.compare num100 3 == EQ) || !(Num.compare num4 24 == EQ)) then daysPerMonthLeap else daysPerMonth,
            }
            \state, monthIndex ->
                when List.get state.dpm monthIndex is
                    Err _ -> crash "Unreachable branch: failed to get month days by month index: $(Num.toStr monthIndex)"
                    Ok d -> if d > state.days then Break state else Continue { state & days: (state.days - d), month: state.month + 1 }

    { days: daysTotal, year: Num.toU16 year, month: Num.toU8 month, day: Num.toU8 days, dayOfYear: Num.toU16 dayOfYear }

## convert seconds to record {seconds, hour, minute, second}
secondsToHMS : Int _ -> { seconds : Int _, hour : U8, minute : U8, second : U8 }
secondsToHMS = \seconds -> {
    seconds: seconds,
    hour: (seconds // sPerHour) |> Num.toU8,
    minute: (seconds % sPerHour // sPerMin) |> Num.toU8,
    second: (seconds % sPerMin) |> Num.toU8,
}

## convert unix timestamp (seconds) to days
unixToDays = \unixS -> unixS // sPerDay

## convert unix timestamp (seconds) to seconds of day
unixToDaySeconds = \unixS -> unixS % sPerDay

## convert [Utc] to RFC3339 format [Str]
utcToRFC3339 : Utc.Utc -> Str
utcToRFC3339 = \utc ->
    unixS = utc |> Utc.toMillisSinceEpoch |> \x -> x // msPerS
    { year, month, day } = unixS |> unixToDays |> daysToYMD
    { hour, minute, second } = unixS |> unixToDaySeconds |> secondsToHMS
    date = "$(Num.toStr year)-$(Num.toStr month)-$(Num.toStr day)"
    time = "$(Num.toStr hour):$(Num.toStr minute):$(Num.toStr second)"
    "$(date)T$(time)Z"

##########
## Test ##
##########
expect isLeapYear 2000
expect isLeapYear 2008
expect isLeapYear 2020
expect isLeapYear 2024
expect !(isLeapYear 2100)
