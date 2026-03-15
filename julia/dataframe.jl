#!/usr/bin/env julia

using DataFrames
using DataFramesMeta

data = Dict(
    "姓名" => ["Alice", "Bob", "Charlie"],
    "年龄" => [25, 30, 35]
)
df = DataFrame(data)

show(df)

println()

# get columns
show(df.姓名)
show(df.年龄)

# add columns
df.城市 = ["New York", "Los Angeles", "Chicago"]

show(df)

# filter rows
young_people = df[df.年龄 .< 30, :]
show(young_people)

# update
df.年龄[df.姓名 .== "Bob"] .= 31

show(df)

# query
show(@select (@subset df :年龄 .> 30) :姓名 :年龄 :城市)

# remove columns
select!(df, Not(:城市))
show(df)
