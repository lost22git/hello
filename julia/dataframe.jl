#!/usr/bin/env julia

#== 
DataFrames: https://dataframes.juliadata.org/stable/
DataFramesMeta: https://juliadata.org/DataFramesMeta.jl/stable/
==#

using DataFrames
using DataFramesMeta
using Chain
using Statistics

data = Dict(
    "姓名" => ["Alice", "Bob", "Charlie", "Douglas"],
    "年龄" => [25, 30, 35, 44]
)
df = DataFrame(data)
show(df)

# get columns
show(df.姓名)
show(df.年龄)

# filter rows
young_people = df[df.年龄 .< 30, :]
show(young_people)

# add columns
df.城市 = ["New York", "Los Angeles", "New York", "Los Angeles"]
show(df)

# update columns
df.年龄[df.姓名 .== "Bob"] .= 31
show(df)

# query
show(
    @chain df begin
        @by :城市 :mean_年龄 = mean(:年龄)
        @orderby -:mean_年龄
        @subset :mean_年龄 .> 10
    end
)

# remove columns
select!(df, Not(:城市))
show(df)
