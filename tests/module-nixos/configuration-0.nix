{ config, lib, ... }:

{
  # to please nixos
  boot.loader.grub.enable = false;
  fileSystems."/".device = "/dev/null";
}
