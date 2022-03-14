{ path, runCommand, ... }:
runCommand "${path.name or "tarball"}.tar.gz" { inherit path; } ''
  tar -C $path --transform='flags=r;s|^|nixpkgs/|' -c . | gzip -1 >$out
''
