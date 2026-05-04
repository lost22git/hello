with Ada.Text_IO;
use Ada.Text_IO;

procedure Fib is
  function fib(n: Natural) return Natural is
  begin
    if n < 2 then
      return 1;
    else
      return fib(n-1) + fib(n-2);
    end if;
  end fib;
  N: constant Natural := 40;
begin
  Put_Line(fib(N)'Img);
end Fib;
