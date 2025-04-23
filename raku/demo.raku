#!/usr/bin/env raku 

=begin Grammars

grammar Parser {
  rule TOP { I <love> <lang> }
  token love { '<3' | love }
  token lang { < Raku Perl Janet > }
}

class ParserAction {
  method lang ($/) { make $/.uc }
}

my $match = Parser.parse: 'I <3 Raku';
say $match<lang>;

my $m = Parser.parse('I <3 Janet', actions => ParserAction);
say $m<lang>;
say $m<lang>.made;    # run ParserAction.lang

=end Grammars


=begin Rational-Number

say 0.1 + 0.2 == 0.3;
say (1/13 + 3/7 + 3/8);
say (1/13 + 3/7 + 3/8).raku;

=end Rational-Number


=begin Lazy-Evaluation

my @primes = ^∞ .grep: *.is-prime;
say "1001ˢᵗ prime is @primes[1000]";

# .say for '50TB.file.txt'.IO.words;

=end Lazy-Evaluation


=begin Reduce-Operator

say [+] 1, 2, 3;
say [*] 1, 2, 3;
say ([/] 1, 2, 3).raku;

=end Reduce-Operator


=begin Type

say True.WHAT;
say 1.WHAT;
say (1.1).WHAT;
say 'h'.WHAT;
say "h".WHAT;
# Array
my Int @array = [1,2];
say @array.WHAT;
say @array.VAR.of;
# Hash
my %hash = (a=>1, b=>2);
say %hash.WHAT;
# Range
say (1..10).WHAT;
say (1..^10).WHAT;
# Any
my $unassign;
say $unassign.WHAT;
my $nil = Nil;
say $nil.WHAT;

=end Type


=begin Given

my $var = 42;

given $var {
    when 0..50 { say 'Less than or equal to 50'}
    when Int { say "is an Int" }
    when 42  { say 42 }
    default  { say "huh?" }
}
say "=" x $var; # print === 
given $var {
    when 0..50 { say 'Less than or equal to 50';proceed}
    when Int { say "is an Int";proceed}
    when 42  { say 42 }
    default  { say "huh?" }
}

=end Given


=begin Sub-Routine

# https://docs.raku.org/language/signatures
    
sub fib(Int $n-->Int) {
   if $n <= 1 {return 1}
    fib($n-1) + fib($n-2)
}

say fib(11);


sub test-named(Str :$name, UInt :$age=10 --> Str) {
   "{$name}'s age is $age"
}

say test-named(name=>"King");
say test-named(name=>"King", age=>11);
my $capture = \(name=>"King", age=>12); # \ => Capture.new
my $signature = :(Str :$name, UInt :$age); # : => Signature
say $signature.^name;
say "capture matches signature: { $capture ~~ $signature }"; 
say "capture matches signature: { $capture ~~ &test-named.signature }"; 
say test-named(|$capture);

=end Sub-Routine


=begin Multi-Dispatch

multi fib-multi(0-->1){}
multi fib-multi(1-->1){}
multi fib-multi(Int $n where $n>=2){
   fib-multi($n-1) + fib-multi($n-2)
}

say fib-multi(11);
  
=end Multi-Dispatch


=begin Multi-Assignment

my ($x, $y, $z) = 11, 22, 33;
dd $x;
dd $y;
dd $z;

=end Multi-Assignment


=begin Regex
=end Regex


=begin Feed-Operator

(1..10)
==> map({ $_ ** 2 })
==> say();

=end Feed-Operator


=begin Hash

my %ansi-color = :Red(31), :Green(32), :Yellow(33), :Blue(34);
say %ansi-color;
say "ansi-color count: { +%ansi-color }";

=end Hash


=begin Time

my $st = now;
sleep 0.5;
say "Elapsed: {now - $st}s";
say DateTime.now;
say DateTime.now.utc;
say DateTime.now.posix;
say DateTime.now.Instant;
say DateTime.now.Instant.to-posix;
say DateTime.new(Instant.from-posix(0));
say Instant.from-posix(0).DateTime;
say DateTime.new(Instant.from-posix(DateTime.now.posix));
say "2022-02-02T02:02:02+0202".DateTime;
say DateTime.new("2022-02-02T02:02:02+0202");

=end Time


=begin Class

class Rectangle {
  # `.` -> public
  has Int $.length = 1; 
  has Int $.width = 1;

  # `!` -> private
  has Bool $!debug = True;

  # 'is rw' -> writable
  has Str $.color is rw = "Red";

  method area(--> Int) {
    self!pri() if $!debug;
    return $!length * $!width;
  }

  method !pri() {
    say "private method, can't call from outside."
  }
}

my $rect = Rectangle.new(length => 2, width => 2);
$rect.color = "Blue";
say $rect;
say $rect.area();

say $rect.^name; # get class name
say $rect.^attributes; # get class attributes
say $rect.^methods; # get class methods
say $rect.^methods(:all); # get class all methods

=end Class


=begin Prompt
   
while prompt "\e[1mINPUT YOU NUMBER: \e[m" -> Int $n {
  say "\e[37;42mYOUR INPUT NUMBER IS: $n\e[m";
}
CATCH {
  die "\e[37;41mYOUR INPUT IS NOT A NUMBER !!!\e[m";
}

=end Prompt


=begin Repeat

say "blue" x 4;
say "blue" xx 4;

say <blue red> x 4;
say <blue red> xx 4;

=end Repeat


=begin IO

.say for "./demo.raku".IO.lines;
say "lines:", "./demo.raku".IO.lines.elems;
say "words:", "./demo.raku".IO.words.elems;

=end IO


=begin String

say "raku".lc;
say "raku".uc;
say "raku".chars;
say "raku".codes;
say "raku".encode.bytes;
say "raku;;perl".split(";");
say <raku perl>.join(";");

=end String


=begin Default

my Int $nil;
say $nil // 10;

=end Default

=begin Junction

use Test;

# all
is True, (<raku perl ruby>.all ~~ /r/).Bool, "all";
is True, (("raku" & "perl" & "ruby") ~~ /r/).Bool, "all: &";

# any
is True, (<raku perl ruby>.any ~~ /p/).Bool, "any";
is True, (("raku" | "perl" | "ruby") ~~ /p/).Bool, "any: |";

# one
is True, (<raku perl ruby>.one ~~ /p/).Bool, "one";
is True, (("raku" ^ "perl" ^ "ruby") ~~ /p/).Bool, "one: ^";


# none
is True, (<raku perl ruby>.none ~~ /o/).Bool, "none";
is True, (("raku" & "perl" & "ruby") !~~ /o/).Bool, "none: & !~~";

  
=end Junction

=begin Hyper-Batch

sub time-it($msg, &f) {
  my $st = now;
  f();
  say "Elapsed={now - $st}s, $msg";
}

for [1, 2, 4, 8, 16, 32] -> $i {
  my $batch = $i * 1024; 
  time-it "batch=$batch", -> {
    (^∞ ).hyper(batch=>$batch).grep(*.is-prime)[99999];
  }
}

=end Hyper-Batch

=begin Zip

say <a b c d e> Z <1 2 3 4>;
say <a b c d e> Z~ <1 2 3 4>;
say (1..10) Z+ (1..10);
say (1..10) Z- (1..10);
say (1..10) Z* (1..10);
say (1..10) Z/ (1..10);
say (1..10) Z% (1..10);
say (1..10) Z%% (1..10);

for <a b c d e> Z (1..4) -> [$k, $v] {
  say "$k => $v";
}

=end Zip


=begin Concurrency

# https://docs.raku.org/language/concurrency

# promise
my $p = Promise.in(1).then({
  say "saying after 1s";
  1
});
my $p2 = Promise.in(2).then({
  say "saying after 2s";
  2
});
say "await promises result are ", await $p,$p2;


# channel
my $ch = Channel.new;
start {
  say now.DateTime;
  sleep 1;
  $ch.send(now);
}
say $ch.receive.DateTime;


# supply (aka. reactive-stream in other languages)
# https://docs.raku.org/type/Supply
# https://github.com/Raku/CCR/blob/main/Remaster/Jonathan%20Worthington/Raku-Supplies-Reactive-Programming.md

my $supplier = Supplier.new;
my $supply = $supplier.Supply;
$supply.tap: { say "1|", DateTime.now, " : ", $_},  # => on-next
done=> { say "1|done" },                            # => on-complete
quit => { say "1|quit with error: $_" };            # => on-error

my $supply2 = $supply.grep(* > 2);  # => filter 
$supply2.tap: { say "2|", DateTime.now, " : ", $_},
done=> { say "2|done" },
quit => { say "2|quit with error: $_"};

for (1..4) -> $i {
  $supplier.emit($i);
  $supplier.quit("Got 3") if $i == 3;
  $supplier.done() if $i == 4;
  sleep 1;
}


# react 
react {
  whenever Supply.interval(1) -> $v {
    say "1|", DateTime.now, " : ", $v;
    done() if $v == 4;
  }
  
  whenever Supply.interval(2) -> $v {
    say "2|", DateTime.now, " : ", $v;
    done() if $v == 4;
  }
}

say "outside of react block";


# Supply -> Channel
my $ch = Supply.interval(1).Channel;
my $ch2 = Supply.interval(2).Channel;

react {
  whenever $ch -> $v {
    say "1|", DateTime.now, " : ", $v;
    done() if $v == 4;
  }

  whenever $ch2 -> $v {
    say "2|", DateTime.now, " : ", $v;
    done() if $v == 4;
  }
}

=end Concurrency

=begin Error

# https://docs.raku.org/language/exceptions

use Test;

# try default
is 1, try +"xx" // 1;

# with try
with try +"xx" {
  is 1, 0;
} else {
  is 1, 1;
}

# try catch

try {
  CATCH {
    default { say "error: ", $_ }
  }

  my $v = +"11x";
  say "v => ", $v;
}

say "outside of try";

=end Error


