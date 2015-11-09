let
    rotorepo = (import <nixpkgs> {}).fetchgit {
      url = /home/sjanssen/rotothopter;
      rev = "181756b3a021f6a39677522ddb7ec470d9fd4abc";
      sha256 = "1wgmzns2d59k9hdi6402q35mdba7k3n98csw35g2hnnviz1d52qi";
      branchName = "sqlify";
    };
    rotostatic = (import "${rotorepo}/release.nix" {  }).rotothopter_static;
in
{
  network.description = "Web server";

  dbserver =
    { config, pkgs, ... }:
    { services.postgresql.enable = true;
      services.postgresql.enableTCPIP = true;
      services.postgresql.initialScript = ./sqlsettings.sql;
      services.postgresql.authentication =
''
host all all 0.0.0.0/0 md5
'';
      networking.firewall.allowedTCPPorts = [ 5432 ];
    };
  webserver =
    { config, pkgs, ... }:
    { services.nginx.enable = true;
      services.nginx.httpConfig =
        ''
          upstream backends {
            server appserver:3000;
          }

          server {
            listen 80;

            location ~ ^/draft/[0-9]+/watch {
              proxy_pass http://backends;
              proxy_read_timeout 450;
              chunked_transfer_encoding off;
              proxy_buffering off;
            }

            location /static/ {
              alias ${rotostatic}/share/x86_64-linux-ghc-7.10.2/rotothopter-0.0.1/static/;
            }

            location / {
              proxy_pass http://backends;
            }
          }
        '';
      networking.firewall.allowedTCPPorts = [ 80 ];
      environment.systemPackages = [rotostatic];
    };
  appserver =
    { config, pkgs, ... }:
    {
      systemd.services.rotothopter = {
        path = [ rotostatic ];
        after = [ "network.target" "local-fs.target" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
            Type = "simple";
            User = "rotothopter";
            ExecStart = ''${rotostatic}/bin/rotothopter'';
            WorkingDirectory = "/var/rotothopter";
        };
        preStart =
        ''
        mkdir -p /var/rotothopter/config
        '';

        environment = {
            STATIC_DIR = ''${rotostatic}/share/x86_64-linux-ghc-7.10.2/rotothopter-0.0.1/static/'';
            PORT = "3000";
            APPROOT = "http://192.168.56.102:8000";
            PGUSER = "roto";
            PGPASS = "roto";
            PGHOST = "dbserver";
            PGDATABASE = "rotothopter";
            ALLOW_DUMMY_AUTH = "true";
        };
      };
      users.extraUsers.rotothopter = {
        home = "/var/rotothopter";
        createHome = true;
      };
      networking.firewall.allowedTCPPorts = [ 3000 ];
    };
}
