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

While this was intended to describe the kind of languages we converse in, I think this applies equally to programming languages. A good programming language can help us develop and express (and make a machine execute) novel ideas elegantly and simply. A bad language does the opposite, constraining our thoughts, bounding our creativity and tiring our fingers as we perform yet another null pointer check.

To demonstrate this, I'm going to talk about a couple of languages I've worked with - in particular, Go and Ruby - and some issues I've encountered with them. This post makes no assertions about the objective merits of these languages: a language can be wonderfully productive in spite of its quirks. Indeed, while languages such as Haskell allow for the creation of beautiful abstrations to express complex transformations of data, there is no doubt that it cannot match Go and Ruby for expressing the idea of actually making money for a business.

# Go

Go is elegantly summed up by one of its creators, Rob Pike, as follows:

> *Our programmers are Googlers, they’re not researchers. They’re typically, fairly young, fresh out of school, probably learned Java, maybe learned C or C++, probably learned Python. They’re not capable of understanding a brilliant language but we want to use them to build good software. So, the language that we give them has to be easy for them to understand and easy to adopt.*

This seems to be particularly condescending to Google employees, so perhaps it was with this in mind that he strapped an explosive to the underside of many data types routinely used in Go, to ensure that programmers are still forced to use some small part of their brain. I am, of course, talking about nil pointers here. If a variable in Haskell might be nil, the type checker will force you to handle this possibility:

```haskell
case mightBeNothing of
  | Just something -> ... -- Use the variable
  | Nothing -> ... -- Handle this case gracefully
```

In Go, you are allowed to simply stick a fork inside the plug socket and work out whether the socket is live:

```go
var mightBeNil *Foo = something()

mightBeNil.Method()
```

Of course, this will work fine if `something` returns a pointer to a `Foo` that exists, and it will give us a beautiful runtime error like the following if it doesn't:

```bash
panic: runtime error: invalid memory address or nil pointer dereference
[signal SIGSEGV: segmentation violation code=0x1 addr=0x0 pc=0x480414]
```

This is the type of problem that a good type system can completely solve for us (see Haskell and Rust, amongst other languages).

Next up, suppose you want to want to perform a fairly simple task: make a network request and parse the returned JSON. We're going to use `https://jsonplaceholder.typicode.com/todos/:id` for this purpose, which returns a simple response like the following:

```bash
$ curl https://jsonplaceholder.typicode.com/todos/1
{
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

Now, let's parse that into a struct in Go:

```go
package main

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
)

type Todo struct {
	UserID   int    `json:"userId"`
	ID       int    `json:"id"`
	Title    string `json:"title"`
	Completed bool   `json:"complete"`
}

func main() {
	url := os.Args[1]
	response, err := http.Get(url)
	if err != nil {
		log.Fatal(err)
	}
	defer response.Body.Close()

	body, err := io.ReadAll(response.Body)
	if err != nil {
		log.Fatal(err)
	}

	var todo Todo
	err = json.Unmarshal(body, &todo)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Printf("Todo: %+v\n", todo)
}
```

We've got lots of error handling here, so we should be pretty confident in the results:

```bash
$ go run test.go https://jsonplaceholder.typicode.com/todos/1
Todo: {UserID:1 ID:1 Title:delectus aut autem Completed:false}
```

The result looks good, with each of the fields shown in the network request being parsed in to the struct.

Now let's try another example:

```bash
$ curl https://jsonplaceholder.typicode.com/todos/4
{
  "userId": 1,
  "id": 4,
  "title": "et porro tempora",
  "completed": true
}
```

We'll parse this again with our little Go script:

```bash
$ go run test.go https://jsonplaceholder.typicode.com/todos/4
Todo: {UserID:1 ID:4 Title:et porro tempora Completed:false}
```

Oh dear - the value of `completed` in the request was `true`, but our code is showing `false`. Why is that? Well, if you look closely, you'll notice a small typo: we wrote `json:"complete"`, when in fact we want `json:"completed"`. Rather than shouting at us about our mistake when the request is parsed, Go simply ignores this silently.

Of course, it isn't hard to add our own validation, but I think good defaults are important, and Go defaults to handing you the gun and aiming it at your foot, allowing you to pull the trigger at your leisure (or at 3 in the morning, if you're on call).

We run in to the same issue with zero-value defaults in many other places in Go, such as when retrieving values from hash maps:

```go
package main

import "fmt"

func main() {
	m := make(map[string]int)
	m["a"] = 1
	fmt.Println(m["b"])
}
```

The result:

```bash
$ go run test.go
0
```

We looked up a key that doesn't exist, and rather than seeing an error (or even a nil value) we get the zero value for the type, which in many cases may not raise any suspicion, allowing the bug to remain tucked away in the folds of our program for a long time. A seasoned programmer would object that good testing should catch this, but why rely on testing when a good type system or runtime error can catch this for us?

To solve this we can do the following:

```go
if val, ok := m["b"]; !ok {
  panic("This key doesn't exist")
} else {
  fmt.Println(val)
}
```

Adding this boilerplate isn't so bad once you learn about this particular gotcha, but Go provides such an expansive menu of similar gotchas that it takes some time before they are all seared into one's brain.

There are a number of other changes I would make to Go given the choice, such as adding union types or immutable variables (distinct from the compile-time constants that Go provides), but I won't dive in to these further to save this post from turning too far into a functional programming pitch.

### Notes

No sum types (like T.any in Sorbet, union types in TypeScript etc.)

No immutable variables (i.e. being able to say a variable is constant, even if you don't know it's value at compile time - Go's const only works for compile-time constants)


# Ruby

While the examples given for Go are largely related to the type system, Ruby is a dynamically typed language, which means that the types of variables no longer resides in the compiler and instead reside in the heads of the three developers who built the application and have since moved to your company's competitors for a substantially larger compensation package. For this reason I won't dwell on the benefits that a good type system would bring the language, and note that there is actually a fairly nice type system for Ruby called [Sorbet](https://sorbet.org/), which makes working with Ruby much more enjoyable.

### Notes
Wait for conditional (foo if bar - German language joke)

Implicit function calls (or, as Rubyists would declare, optional parentheses)
