# proxy
GET http://localhost:8000/proxy?target=https://httpbin.org/ip
HTTP 200
[Asserts]
header "Content-Type" contains "json"
jsonpath "$.origin" != null

