let
  many = x: { recurseForDerivations = true;
              a = x; b = x; c = x; d = x; e = x;
              f = x; g = x; h = x; i = x; j = x;
            };
  pkg = builtins.abort "I'm afraid I can't do that, Dave";
in many (many (many (many (many (many pkg)))))
