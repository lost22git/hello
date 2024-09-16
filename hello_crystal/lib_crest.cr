#!/usr/bin/env -S DEBUG=1 crun

# ---
# crest:
#   github: mamantoha/crest
# ...

require "crest"
require "spec"

describe "Lib Crest Test" do
  it "raise error if status code not ok" do
    Crest.get "https://httpbin.org/status/444"
  rescue e : Crest::RequestFailed
    e.response.status.code.should eq 444
  end

  it "handle error" do
    resp = Crest.get "https://httpbin.org/status/444", handle_errors: false
    resp.status.code.should eq 444
  end

  it "proxy" do
    proxy = ENV["HTTP_PROXY"]? || "http://localhost:55556"
    p_addr, p_port = proxy.split("//")[1].split(":")

    resp = Crest.get "https://github.com/topics/crystal", p_addr: p_addr, p_port: p_port.to_i
    resp.status.code.should eq 200
  rescue e : Crest::RequestFailed
    p! e.response
  end

  it "logging" do
    Crest.get "https://httpbin.org/status/444", logging: true
  rescue e : Crest::RequestFailed
    e.response.status.code.should eq 444
  end
end
