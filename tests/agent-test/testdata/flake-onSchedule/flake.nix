{
  inputs = { };
  outputs = { self, ... }: {
    herculesCI.onSchedule.update = {
      when = {
        minute = 37;
        hour = 12;
        dayOfWeek = [ "Mon" "tue" "wed" "THU" "fri" "sat" "sun" ];
        dayOfMonth = [ 1 2 3 4 5 6 7 ];
      };
      outputs = {
        data-pkg = derivation {
          name = "data-pkg-1.0";
          builder = "foo";
          system = "x86_64-linux";
        };
        inherit (self) defaultPackage;
      };
    };

  };
}
