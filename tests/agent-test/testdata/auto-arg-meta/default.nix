{ identity ? null }:
assert identity.name == baseNameOf identity;
import identity {
  hello = derivation {
    name = "pkg-${identity.rev}-${toString (identity.x.y.z * 2)}-${if identity.yes && identity.no then "bad" else "ok"}";
    builder = "foo";
    system = "x86_64-linux";
  };
}
