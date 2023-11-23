# tomschafer.com

My personal website, which can be viewed at [tomschafer.com](https://tomschafer.com/).

## Building

Pre-requisites:

1. Install `ghc`, used for compiling Haskell (see [here](https://www.haskell.org/downloads/))
1. Install the `pandoc` CLI (see [here](https://github.com/jgm/pandoc))
1. Make a copy of `.env.template` called `.env`, and add values to the `Build` section

To build:

1. Run `./build.sh`

## Deploying

Pre-requisites:

1. Complete all pre-requisites in the above `Building` section
1. Install the Netlify CLI (see [here](https://docs.netlify.com/cli/get-started/#installation))
1. Update your `.env` file by adding values to the `Deploy` section

To deploy:

1. Run `./deploy.sh` (note that this also runs the build script before deploying)
