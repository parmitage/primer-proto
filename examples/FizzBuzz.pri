Map(fn (x) if x mod 3 == 0 and x mod 5 == 0 then Show("fizzbuzz")
           else if x mod 3 == 0 then Show("fizz")
           else if x mod 5 == 0 then Show("buzz")
           else Show(x), 1..100);