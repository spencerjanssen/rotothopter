let simple_vbox =
    {config, pkgs, ... }:
    { deployment.targetEnv = "virtualbox";
      deployment.virtualbox.memorySize = 512; # megabytes
    };
in
{
  webserver = simple_vbox;
  appserver = simple_vbox;
  dbserver = simple_vbox;
}
