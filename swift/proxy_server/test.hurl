# proxy
GET http://localhost:8080/proxy?target=https://httpbin.org/ip
HTTP 200
[Asserts]
header "Content-Type" contains "json"
jsonpath "$.origin" != null

POST http://localhost:8080/proxy?target=https://httpbin.org/post
{
  "name": "vapor"
}
HTTP 200
[Asserts]
header "Content-Type" contains "json"
jsonpath "$.data" != null
