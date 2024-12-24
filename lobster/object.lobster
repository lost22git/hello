#!/usr/bin/env lobster

class RssItemEnclosure:
  url:string
  length:int
  type:string

class RssItem:
  title:string
  link:string
  description:string=""
  author:string=""
  enclosure:RssItemEnclosure?=nil
  guid:string=""
  pubDate:string=""

// check RssItem if has enclosure
def has_enclosure(_ :: RssItem):
  return enclosure !=  nil

class Rss:
  title:string
  link:string
  description:string=""
  copyright:string=""
  lastBuildDate:string=""
  pubDate:string=""
  ttl:int=-1
  image:string=""
  items:[RssItem]=[]

// check Rss if has items
def has_items(_ :: Rss):
  return length(items) > 0

// clear items of Rss
def clear_items(_ :: Rss):
  items = []

// TODO
// def from_string(s:string):
//   let title = ""
//   let link = ""
//   return Rss{title: title, link:link}



/////////////
// Testing //
/////////////

let rss = Rss{
  title:"my podcast",
  link:"http://mypodcast.xyz"
}

print("rss has items: {rss.has_items()}")

let rss_item = RssItem {
  title:"my podcast",
  link:"http://mypodcast.xyz"
}

print("rss item has enclosure: {rss_item.has_enclosure()}")
print(rss_item)

