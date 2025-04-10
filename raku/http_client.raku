#!/usr/bin/env raku 

# NOTE: 
# Segment Fault on termux android 9
# 
# `zef install --/test Cro::HTTP`

=begin comment

use Cro::HTTP::Client;

my $resp = await Cro::HTTP::Client.get: "https://httpbin.org/ip";
my $body = await $resp.body;
say $body;

=end comment




# NOTE: 
# Segment Fault on termux android 9
#
# `zef install --/test HTTP::Tiny`
# `zef install --/test IO::Socket::SSL`

=begin comment

use HTTP::Tiny;

my $resp = HTTP::Tiny.new.get: 
  "https://httpbin.org/ip";

if $resp<success> {
  say $resp<content>.decode;
} else {
  note "ERROR: ", $resp<reason>;
  note "ERROR: ", $resp<content>.decode;
}

=end comment
