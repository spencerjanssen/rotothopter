{ rotothopterRev, rotothopterHash, wowRev, wowHash }:
let
    rotorepo = (import <nixpkgs> {}).fetchgit {
      url = https://github.com/spencerjanssen/rotothopter.git;
      rev = rotothopterRev;
      sha256 = rotothopterHash;
      branchName = "master";
    };
    rotostatic = (import "${rotorepo}/release.nix" {  }).rotothopter_static;
    secrets = (import ./prod-secrets.nix);
    wowrepo = (import <nixpkgs> {}).fetchgit {
        url = /home/sjanssen/randy_soundboard;
        rev = wowRev;
        sha256 = wowHash;
    };
    wow = (import "${wowrepo}/release.nix" {}).randy_soundboard;
in
{
    network.description = "Rotothopter Single AWS";

    webserver =
        { config, pkgs, ... }:
        {
            # postgres stuff
            services.postgresql.enable = true;
            services.postgresql.enableTCPIP = false;
            services.postgresql.initialScript = ./sqlsettings.sql;

            # nginx stuff
            services.nginx.enable = true;
            services.nginx.httpConfig =
            ''
            upstream backends {
                server 127.0.0.1:3000;
            }

            server {
                listen 80 default_server;
                server_name www.rotothopter.com;
                location ~ ^/draft/[0-9]+/watch {
                    proxy_pass http://backends;
                    proxy_read_timeout 450;
                    chunked_transfer_encoding off;
                    proxy_buffering off;
                }
                location /static/ {
                    alias ${rotostatic}/share/x86_64-linux-ghc-8.0.2/rotothopter-0.0.1/static/;
                }

                location / {
                    proxy_pass http://backends;
                }
                location /wow {
                    alias ${wow}/;
                }
            }

            server {
                server_name rotothopter.com rotothopter.ignorelist.com;
                return 302 http://www.rotothopter.com$request_uri;
            }
            '';
            networking.firewall.allowedTCPPorts = [ 80 ];
            environment.systemPackages = [rotostatic];

            # app stuff
            systemd.services.rotothopter = {
                path = [ rotostatic ];
                after = [ "network.target" "local-fs.target" ];
                requires = [ "postgresql.service" ];
                wantedBy = [ "multi-user.target" "nginx.service" ];
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
                    STATIC_DIR = ''${rotostatic}/share/x86_64-linux-ghc-8.0.2/rotothopter-0.0.1/static/'';
                    PORT = "3000";
                    APPROOT = "http://www.rotothopter.com";
                    PGUSER = "roto";
                    PGPASS = "roto";
                    PGHOST = "127.0.0.1";
                    PGDATABASE = "rotothopter";
                    ALLOW_DUMMY_AUTH = "false";
                    GMAIL_PASSWORD = secrets.gmail_password;
                    OUTGOING_ADDRESS = secrets.outgoing_address;
                    SES_REGION = secrets.ses_region;
                    SES_ACCESS = secrets.ses.access;
                    SES_SECRET = secrets.ses.SECRET;
                    GOOGLE_CLIENT_ID = secrets.google_client_id;
                    GOOGLE_CLIENT_PASSWORD = secrets.google_client_password;
                };
            };
            users.extraUsers.rotothopter = {
                home = "/var/rotothopter";
                createHome = true;
            };
        };
}
