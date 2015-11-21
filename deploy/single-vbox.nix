let simple_vbox =
    {config, pkgs, ...}:
    {
        deployment.targetEnv = "virtualbox";
        deployment.virtualbox.memorySize = 512;
    };
in
{
    webserver = simple_vbox;
}
