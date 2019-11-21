let
    tarball = fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/f40b8471e745a44929bcc93e6ac76498ebd66fce.tar.gz";
        sha256 = "0r7nshgi93sh2ig5hmrhk3fvnj4496rs8qq108cyqkf3ja0dwj75";
    };
    config = {
    };
in
    {}: import tarball {inherit config;}
