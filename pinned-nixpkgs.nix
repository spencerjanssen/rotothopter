let hostpkgs = import <nixpkgs> {};
in import (hostpkgs.fetchgit {
    url = "http://github.com/NixOS/nixpkgs-channels";
    rev = "01a8de97eb2aab98e6e9a7330bf99f4fe4844d2a";
    sha256 = "003sig35c7062dq0ivi8axbngy78q7j0amwz6nph4nv6n4nxz1qg";
    }) {}
