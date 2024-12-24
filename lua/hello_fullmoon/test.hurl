
# user add
POST http://localhost:8080/user
[FormParams]
username: lost
password: lost
HTTP 200
[Asserts]
header "Content-Type" contains "json"
jsonpath "$.code" == 0
jsonpath "$.msg" == "OK"
jsonpath "$.data.id" != null

# user list
GET http://localhost:8080/user
HTTP 200
[Asserts]
header "Content-Type" contains "json"
jsonpath "$.code" == 0
jsonpath "$.msg" == "OK"
jsonpath "$.data" count == 2

# login
POST http://localhost:8080/login
[FormParams]
username: lost
password: lost
HTTP 200
[Asserts]
header "Content-Type" contains "json"
jsonpath "$.code" == 0
jsonpath "$.msg" == "OK"
jsonpath "$.data.id" != null

# proxy
GET http://localhost:8080/proxy?target=https://httpbin.org/status/444
HTTP 444
