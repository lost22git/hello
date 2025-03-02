#!/usr/bin/env hurl

## openapi.json
GET http://{{HOST}}:{{PORT}}/swagger-ui/openapi.json
HTTP 200
[Asserts]
header "Content-Type" contains "json"

## get user list
GET http://{{HOST}}:{{PORT}}/users
HTTP 200
[Asserts]
header "Content-Type" contains "json"
jsonpath "$" count == 2

## get user by id
GET http://{{HOST}}:{{PORT}}/users/1
HTTP 200
[Asserts]
header "Content-Type" contains "json"
jsonpath "$.id" == 1
jsonpath "$.name" == "Alex"

## add a new user
POST http://{{HOST}}:{{PORT}}/users
{
  "name": "Rich"
}
HTTP 200
[Asserts]
header "Content-Type" contains "json"
jsonpath "$.id" != null
jsonpath "$.name" == "Rich"
jsonpath "$.create-at" != null
[Captures]
new-user-id: jsonpath "$.id"

## delete user by id
DELETE http://{{HOST}}:{{PORT}}/users/{{new-user-id}}
HTTP 200
[Asserts]
header "Content-Type" contains "json"
jsonpath "$.id" == {{new-user-id}}
jsonpath "$.name" == "Rich"
jsonpath "$.create-at" != null

## edit user by id
PUT http://{{HOST}}:{{PORT}}/users/2
{
  "name": "Bill"
}
HTTP 200
[Asserts]
header "Content-Type" contains "json"
jsonpath "$.id" == 2
jsonpath "$.name" == "Bill"
jsonpath "$.update-at" != null

## bug error
GET http://{{HOST}}:{{PORT}}/bug
HTTP 500
[Asserts]
header "Content-Type" contains "json"
jsonpath "$.time" != null

## coercion error
GET http://{{HOST}}:{{PORT}}/users/sss
HTTP 500
[Asserts]
header "Content-Type" contains "json"
jsonpath "$.time" != null
