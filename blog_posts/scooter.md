---
title: Scooter
pubDate: 2024-11-13
image:
  url: images/scooter-preview.png
  alt: Preview of Scooter running in a terminal window
description: An interactive find and replace terminal UI application I've been working on.
---

For the past couple of months I've been working on [a small but useful TUI application](https://github.com/thomasschafer/scooter), written in Rust, and I'd like to talk a little about it.

My editor of choice is [Helix](https://github.com/helix-editor/helix/), which is a brilliant modal text editor built for the terminal - kind of like Neovim, but even faster and with a bunch of great features like powerful multicursor. The one thing to note about Helix, though, is that it doesn't have a lot of built-in features, and there are no plugins at the time of writing ([they're coming though!](https://github.com/helix-editor/helix/discussions/3806)). Notice that I didn't describe this feature minimalism as a downside: I've really enjoyed having my editor stripped back to the basics and being forced to implement all of the additional features I want myself, either in my own fork of Helix, through CLI tools or, for find-and-replace, by building my own TUI app.

This is where Scooter comes in, which is my application that allows you to search for some text, either using regex or a fixed string, and add a pattern to dictate which files should be searched through. You can then select which instances found should be replaced. If any of the files change between you making a selection and performing the replacement, Scooter won't blindly perform a replacement and mess up your files: these instances will be skipped and you'll see them listed at the end.

You can take a look at it [here](https://github.com/thomasschafer/scooter). Feel free to create an issue if you notice anything strange, and if you like it please consider dropping a star!
