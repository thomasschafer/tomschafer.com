---
title: Scooter v0.4
pubDate: 2025-03-24
image:
  url: images/scooter-in-editor.png
  alt: Preview of Scooter running in a terminal window
description: Scooter v0.4 is out, including new fields for glob matching files, editor integration and performance improvements
---

# What's new?

## Include and exclude files with glob matching

TODO: text
TODO: video

## Open editor from search results list with o by @thomasschafer in #80
## Add config option to override editor command by @thomasschafer in #84

TODO: text
TODO: video

## Improve scrolling by @thomasschafer in #85

Previously the selected item was locked to the centre of the screen which didn't look quite right. Scrolling looks a lot more natural now!

![Scooter preview](gifs/scooter-0-4-scroll.gif)

## 3x faster replacements

Replacements now happen in parallel, resulting in >3x faster replacements which is especially nice in big repos. I'm sure there is a lot more room to improve things here, so watch this space, and if you have ideas I'd love contributions - while it's already pretty quick, I want to make Scooter the fastest way to find and replace (without sacrificing usability or safety),and we're only just getting started.


# Contributions are welcome!

Whether you are an expert in building TUIs with Rust or have never used the language before, I'm really grateful for any contributions to Scooter, whether that's creating issues for bugs,requesting new features or creating PRs to implement new ideas and improve performance. If you want a new feature then just create an issue first so that we can chat about it! I'm also always so grateful for anyone adding Scooter to package managers.
