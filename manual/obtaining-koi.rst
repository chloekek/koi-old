Obtaining Koi
=============

To obtain Koi, download the version you want from GitHub_.
Make sure Nix_ is installed,
then run the following command
from the repository root::

    nix run -ic make

The *-i* flag to *nix run* will clear the environment,
so this build should be relatively reproducible.

.. todo:: Document where to find the build output.

.. _GitHub: https://github.com/chloekek/koi
.. _Nix: https://nixos.org/nix
