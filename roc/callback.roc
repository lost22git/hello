#!/usr/bin/env -S roc test
module []

parseField = \f -> f

parseBook = \_ ->
    parseField \name ->
        parseField \price ->
            { name: name, price: price }

parseBookSugar = \_ ->
    name <- parseField
    price <- parseField
    { name: name, price: price }

expect (((parseBook {}) "the roc book") 22.2) == { name: "the roc book", price: 22.2 }
expect (((parseBookSugar {}) "the roc book") 22.2) == { name: "the roc book", price: 22.2 }

