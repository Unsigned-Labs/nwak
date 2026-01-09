# Nostr Web Army Knife

## A toolkit for debugging all things nostr as a webpage

![2025-03-16-213105_1454x948_scrot](https://github.com/user-attachments/assets/1f7b9bcb-9b2d-4139-9d11-4b324e4916fe)

Written in [Scala](https://scala-lang.org/) with [Calico](https://www.armanbilge.com/calico/) and [Snow](https://github.com/fiatjaf/snow).

Check it out at https://nwak.unsigned.in/

### About This Fork

This is a fork of [fiatjaf/nwak](https://github.com/fiatjaf/nwak) with a dark theme and enhanced UI. Original version available at https://nwak.nostr.technology/

### Building

Here is one way you can build and host a local copy of `nwak`:
1. Install [Nix](https://nixos.org) the package manager, and make sure [flakes](https://wiki.nixos.org/wiki/Flakes) are enabled
2. Checkout this repo and `cd` into it
3. `nix develop` will get you into a dev environment with all the things (`sbt`)
4. `sbt fullLinkJS/esBuild` will build the all important `bundle.js` file
5. `python3 -m http.server 8743` will serve up the HTML/JS app at http://localhost:8743

### Developing Locally

After step 3 of the above, run `just` if you have it installed.

That should set up a process that will continuously recompile the JavaScript while also serving the webpage at http://localhost:8743
