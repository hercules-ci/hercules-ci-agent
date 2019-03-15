{ nixpkgs ? null }:

{ inherit (import nixpkgs {}) hello;
}
