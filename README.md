# tomschafer.com

My personal website, which can be viewed at [tomschafer.com](https://tomschafer.com/).

## Build

Pre-requisites:

1. [Install Nix](https://nixos.org/download/) if you don't already have it on your machine
1. Make a copy of `.env.template` called `.env`, and add values to the `Build` section

Run:

```
nix develop --command make build
```

## Deploy

Pre-requisites:

1. Complete all pre-requisites in the above `Build` section
1. Update your `.env` file by adding values to the `Deploy` section

Run:

```
nix develop --command make deploy
```
