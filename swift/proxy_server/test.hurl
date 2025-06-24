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

POST http://localhost:8080/proxy?target=https://api.imgur.com/3/image?client_id=546c25a59c58ad7
[Multipart]
image: file,棋魂.jpg;
HTTP 200
[Asserts]
header "Content-Type" contains "json"
jsonpath "$.data.link" != null
