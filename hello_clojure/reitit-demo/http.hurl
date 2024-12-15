#!/usr/bin/env hurl

# get book list
# curl -sSL -X GET http://localhost:8000/book
GET http://localhost:8000/book
HTTP 200
[Asserts]
header "Content-Type" contains "json"
jsonpath "$" count == 2

# get book by id
# curl -sSL -X GET http://localhost:8000/book/1
GET http://localhost:8000/book/1
HTTP 200
[Asserts]
header "Content-Type" contains "json"
jsonpath "$.id" == 1
jsonpath "$.title" == "Programming Clojure"

# add a new book
# curl -sSL -X POST http://localhost:8000/book -H 'content-type:application/json' -d '{"title":"Getting Clojure"}'
POST http://localhost:8000/book
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

# delete book by id
# curl -sSL -X DELETE http://localhost:8000/book/{id}
DELETE http://localhost:8000/book/{{new-book-id}}
HTTP 200
[Asserts]
header "Content-Type" contains "json"
jsonpath "$.id" == {{new-book-id}}
jsonpath "$.title" == "Getting Clojure"
jsonpath "$.create-at" != null

# edit book by id
# curl -sSL -X PUT http://localhost:8000/book/2 -H 'content-type:application/json' -d '{"title":"Clojure in Action"}'
PUT http://localhost:8000/book/2
{
  "title": "Clojure in Action"
}
HTTP 200
[Asserts]
header "Content-Type" contains "json"
jsonpath "$.id" == 2
jsonpath "$.title" == "Clojure in Action"
jsonpath "$.update-at" != null

