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


=begin Async-Parallelism

start { sleep 1.5; print 'hi' }
await Supply.from-list(< A B C D E F >).throttle: 2, {
  sleep 0.5;
  .print
}

say "";

=end Async-Parallelism


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
my @array = [1,2];
say @array.WHAT;
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

sub fib(Int $n-->Int) {
   if $n <= 1 {return 1}
    fib($n-1) + fib($n-2)
}

say fib(11);

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


