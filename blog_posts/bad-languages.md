---
title: "Bad languages"
pubDate: 2024-05-05
image:
  url: images/coding-in-coffee-shop.png
  alt: A young woman coding in a coffee shop
description: TODO
---

Bertrand Russell said

> *Language serves not only to express thought, but to make possible thoughts which could not exist without it.*

While this was intended to describe the kind of languages we converse in, I think this applies equally to programming languages. A good programming language can help us develop and express (and make a machine execute) novel ideas elegantly and simply. A bad language does the opposite, obscuring beauty behind mounds of debris, bounding our creativity and tiring our fingers as we perform yet another null pointer check.

To demonstrate this, I'm going to talk about a couple of languages I've worked with - in particular, Go and Ruby - and some issues I've encountered with them. This post makes no assertions about the objective merits of these languages: a language can be wonderfully productive in spite of its quirks. Indeed, while languages such as Haskell allow for the creation of beautiful abstrations to express complex transformations of data, there is no doubt that it cannot match Go and Ruby for expressing the idea of actually making money for a business.

# Go

Go is elegantly summed up by its creator, Rob Pike, as follows:

> *Our programmers are Googlers, they’re not researchers. They’re typically, fairly young, fresh out of school, probably learned Java, maybe learned C or C++, probably learned Python. They’re not capable of understanding a brilliant language but we want to use them to build good software. So, the language that we give them has to be easy for them to understand and easy to adopt.*

This seems to be particularly condescending to Google employees, so perhaps it was with this in mind that he strapped an explosive to the underside of many data types routinely used in Go, to ensure that programmers are still forced to use some small part of their brain. I am, of course, talking about nil pointers here. If a variable in Haskell might be nil, the type checker will force you to handle this possibility:

```haskell
case mightBeNothing of
  | Just something -> ... -- Use the variable
  | Nothing -> ... -- Handle this case gracefully
```

In Go, you are allowed to simply stick a fork inside the power outlet and work out whether the plug socket is live:

```go
var mightBeNil *Foo = something()

mightBeNil.Method()
```

Of course, this will work fine if `something` returns a pointer to a `Foo` that exists, and it will give you a beautiful runtime error like the following if it doesn't:

```bash
panic: runtime error: invalid memory address or nil pointer dereference
[signal SIGSEGV: segmentation violation code=0x1 addr=0x0 pc=0x480414]
```

### Notes
- would be nicer to know for certain that a value is never nil, or might be nil so you can check, but currently in Go any value can be nil

Zero-value initialisation of structs - if you create a struct without specifying the value you want at a given field then it will take the zero value of the type, e.g. 0 for an int, "" for a string, nil for an object pointer, rather than throwing an error (e.g. with JSON parsing)

No sum types (like T.any in Sorbet, union types in TypeScript etc.)

No immutable variables (i.e. being able to say a variable is constant, even if you don't know it's value at compile time - Go's const only works for compile-time constants)

Missing some useful functions that should be built-in (in my opinion) e.g. getting the sum of a slice

# Ruby

### Notes
Wait for conditional (foo if bar - German language joke)

Implicit function calls (or, as Rubyists would declare, optional parentheses)
