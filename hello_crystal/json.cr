#!/usr/bin/env crystal

def __(t : String)
  puts ""
  puts "------\e[1;33m " + t + " \e[m" + ("-" * (50 - 8 - t.size))
  puts ""
end

require "json"

__ "JSON <=> Object"

p! 1.to_json
p! 1_f32.to_json
p! "你好".to_json
p! [1, 2].to_json
p! StaticArray[1, 2].to_json
p! ({1, 2}).to_json
p! ({first: "你好", last: "世界"}).to_json
p! ({"first" => "你好", "last" => "世界"}).to_json

@[Flags]
enum Color
  Red
  Green
  Blue
end

p! (Color::Red | Color::Blue).to_json

require "uri"
require "uuid"
require "uuid/json"

class URIConverter
  def self.from_json(parser : JSON::PullParser)
    URI.parse parser.read_string
  end

  def self.to_json(value : URI, builder : JSON::Builder)
    builder.string value.to_s
  end
end

record BookId, value : UUID

class BookIdConverter
  def self.from_json(parser : JSON::PullParser)
    BookId.new UUID.new parser.read_string
  end

  def self.to_json(id : BookId, builder : JSON::Builder)
    builder.string id.value.to_s
  end
end

class Book
  include JSON::Serializable

  @[JSON::Field(converter: BookIdConverter)]
  property id : BookId

  property name : String

  property price : Float64

  property color : Color

  property tags : Array(String) = [] of String

  property published_time : Time = Time.utc

  @[JSON::Field(converter: URIConverter)]
  property link : URI? = nil

  def initialize(@id, @name, @price, @color, @tags = [] of String, @published_time = Time.utc, @link = nil)
  end
end

pp Book.from_json Book.new(
  id: BookId.new(UUID.random),
  name: "《史记》",
  price: 66.6,
  color: Color::Red,
  tags: ["历史", "中国"],
  published_time: Time.parse_iso8601("2222-02-02T02:02:02+0800"),
  link: URI.parse("https://book.com/1"),
).to_json

pp Book.from_json <<-JSON
{
  "id": "6bce63d9-7b7f-4493-9530-6fa1d4c1b285",
  "name": "《史记》",
  "price": 66.6,
  "color": [
    "red"
  ]
}
JSON

__ "JSON::parse and JSON::build"

# ```json
# {
#  "a": {
#    "b": [
#      {
#        "ck": "cv",
#        "ci": [
#          1, 2, 3
#        ]
#      }
#    ]
#  }
# }
#
# ```

any = JSON.parse(
  JSON.build do |j|
    j.object {
      j.field "a" {
        j.object {
          j.field "b" {
            j.array {
              j.object {
                j.field "ck", "cv"
                j.field "ci" {
                  j.array {
                    j.number 1
                    j.number 2
                    j.number 3
                  }
                }
              }
            }
          }
        }
      }
    }
  end
)

p! any.dig("a", "b", 0, "ck").as_s
p! any.dig?("a", "b", 0, "ckk").try &.as_s || ""

any.dig("a", "b", 0, "ci").as_a.push JSON::Any.new 4
p! any.dig("a", "b", 0, "ci")
