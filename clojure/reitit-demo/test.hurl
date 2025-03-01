#!/usr/bin/env hurl

## get book list
## curl -sSL -X GET http://{{HOST}}:{{PORT}}/book
GET http://{{HOST}}:{{PORT}}/book
HTTP 200
[Asserts]
header "Content-Type" contains "json"
jsonpath "$" count == 2

## get book by id
## curl -sSL -X GET http://{{HOST}}:{{PORT}}/book/1
GET http://{{HOST}}:{{PORT}}/book/1
HTTP 200
[Asserts]
header "Content-Type" contains "json"
jsonpath "$.id" == 1
jsonpath "$.title" == "Programming Clojure"

## add a new book
## curl -sSL -X POST http://{{HOST}}:{{PORT}}/book -H 'content-type:application/json' -d '{"title":"Getting Clojure"}'
POST http://{{HOST}}:{{PORT}}/book
{
  "title": "Getting Clojure"
}
HTTP 200
[Asserts]
header "Content-Type" contains "json"
jsonpath "$.id" != null
jsonpath "$.title" == "Getting Clojure"
jsonpath "$.create-at" != null
[Captures]
new-book-id: jsonpath "$.id"

## delete book by id
## curl -sSL -X DELETE http://{{HOST}}:{{PORT}}/book/{id}
DELETE http://{{HOST}}:{{PORT}}/book/{{new-book-id}}
HTTP 200
[Asserts]
header "Content-Type" contains "json"
jsonpath "$.id" == {{new-book-id}}
jsonpath "$.title" == "Getting Clojure"
jsonpath "$.create-at" != null

## edit book by id
## curl -sSL -X PUT http://{{HOST}}:{{PORT}}/book/2 -H 'content-type:application/json' -d '{"title":"Clojure in Action"}'
PUT http://{{HOST}}:{{PORT}}/book/2
{
  "title": "Clojure in Action"
}
HTTP 200
[Asserts]
header "Content-Type" contains "json"
jsonpath "$.id" == 2
jsonpath "$.title" == "Clojure in Action"
jsonpath "$.update-at" != null

## bug error
## curl -sSL -X GET http://{{HOST}}:{{PORT}}/bug
GET http://{{HOST}}:{{PORT}}/bug
HTTP 500
[Asserts]
header "Content-Type" contains "json"
jsonpath "$.time" != null

## coercion error
## curl -ssL -X GET http://{{HOST}}:{{PORT}}/book/sss
GET http://{{HOST}}:{{PORT}}/book/sss
HTTP 500
[Asserts]
header "Content-Type" contains "json"
jsonpath "$.time" != null

GET http://{{HOST}}:{{PORT}}/openapi.json
HTTP 200
[Asserts]
header "Content-Type" contains "json"
