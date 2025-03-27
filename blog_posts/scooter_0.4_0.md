---
title: Scooter v0.4
pubDate: 2025-03-24
image:
  url: images/scooter-in-editor.png
  alt: Preview of Scooter running in a terminal window
description: Scooter v0.4 is out, including new fields for glob matching files, editor integration and performance improvements
---

[Scooter](https://github.com/thomasschafer/scooter) is a fast, user-friendly find and replace tool built for people who love the terminal. We've recently released v0.4: here is a rundown of what's new.

## Include and exclude files with glob matching

You can now use glob patterns to both include and exclude files, in a similar way to the find and replace in VSCode. For instance, if you wanted to include all matches with the `.rs` extension except for those in the `xtask` directory, you could do this:

![Scooter with file includes and excludes](images/scooter-include-exclude.png)

## Open editor from search results list

From the search results page you can now open your editor at the selected file and line with `o`:

![Scooter scrolling preview](gifs/scooter-editor-open.gif)

By default this will use your `$EDITOR` environment variable, but you can [override this using your config file](https://github.com/thomasschafer/scooter?tab=readme-ov-file#editor_open-section).

You can combine this with some editor configuration to get a nice integration - for instance, I use Helix and Tmux, and I have the following in my Helix configuration to open Scooter in a floating window:

```toml
[keys.select.ret]
s = ":sh tmux popup -xC -yC -w90% -h90% -E scooter"
```

I also have this in my Scooter config:

```toml
[editor_open]
command = 'tmux send-keys -t "$TMUX_PANE" ":open %file:%line" Enter'
exit = true
```

This allows me to open Scooter in a floating pane in my editor with `<enter> s`, and then open a search result back in the editor (closing the floating window) with `o`:

![Scooter Helix integration](gifs/scooter-helix-integration.gif)

## 3x faster replacements

Replacements now happen in parallel, resulting in over 3x faster replacements which is especially nice in big repos. I'm sure there is a lot more room to improve things here, so watch this space, and if you have ideas I'd love contributions.

## Improved scrolling

Previously the selected item was locked to the centre of the screen, which didn't look quite right. Scrolling looks a lot more natural now!

![Scooter scrolling preview](gifs/scooter-0-4-scroll.gif)

# Installation

Check out the installation guide [here](https://github.com/thomasschafer/scooter?tab=readme-ov-file#installation) to get started with Scooter - I'd love to know what you think.

## Contributions are welcome!

No matter your experience level I'm always really grateful for any contributions to Scooter, whether that's creating issues for bugs, requesting features or creating PRs to implement new ideas and improve performance. If you want a new feature then just create an issue first so that we can chat about it! I'm also always grateful for anyone adding Scooter to package managers.
