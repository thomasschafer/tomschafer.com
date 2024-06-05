---
title: "Bad programming languages"
pubDate: 2024-05-05
image:
  url: images/coding-in-coffee-shop.png
  alt: A young woman coding in a coffee shop
description: Some argue that there is no objective truth - only subjective beliefs. I will ignore this entirely and discuss the shortcomings of some languages I've used.
---

Bertrand Russell said

> _Language serves not only to express thought, but to make possible thoughts which could not exist without it._

While this was intended to describe the kind of languages we converse in, I think this applies equally to programming languages. A good programming language can help us develop and express (and make a machine execute) novel ideas elegantly and simply, guiding us down paths we may otherwise not have traveled. A bad language does the opposite, constraining our thoughts, bounding our creativity and tiring our fingers as we perform yet another null pointer check.

To demonstrate this, I'm going to talk about a couple of languages I've worked with - in particular, Go and Ruby - and some issues I've encountered with them. This post makes no assertions about the objective merits of these languages: a language can be wonderfully productive in spite of its quirks. Indeed, while languages such as Haskell allow for the creation of beautiful abstractions to express complex transformations of data, there is no doubt that it cannot match Go and Ruby for expressing the idea of actually making money for a business.

# Go

Go is elegantly summed up by one of its creators, Rob Pike, as follows:

> _Our programmers are Googlers, they’re not researchers. They’re typically, fairly young, fresh out of school, probably learned Java, maybe learned C or C++, probably learned Python. They’re not capable of understanding a brilliant language but we want to use them to build good software. So, the language that we give them has to be easy for them to understand and easy to adopt._

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

Next up, suppose you want to perform a fairly simple task: make a network request and parse the returned JSON. We're going to use `https://jsonplaceholder.typicode.com/todos/:id` for this purpose, which returns a simple response like the following:

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

Oh dear - the value of `completed` in the request was `true`, but our code is showing `false`. Why is that? Well, if you look closely, you'll notice a small typo: we wrote `json:"complete"`, when in fact we want `json:"completed"`. Rather than berating us for our mistake when the request is parsed, Go silently ignores this.

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

Adding this boilerplate isn't so bad once you learn about this particular behaviour, but Go provides such an expansive menu of similar gotchas that it takes some time before they are all seared into one's brain.

There are a number of other changes I would make to Go given the choice, such as adding union types or immutable variables (distinct from the compile-time constants that Go provides), but I won't dive in to these further to save this post from turning too far into a functional programming pitch.

## Further reading

For some more examples of Go footguns, I'd highly recommend [this article](https://www.dolthub.com/blog/2023-10-20-golang-pitfalls-3/) which discusses various issues with slices - it's a great read.

# Ruby

While the examples given for Go are largely related to the type system, Ruby is a dynamically typed language, which means that the types of variables no longer reside in the compiler and instead reside in the heads of the three developers who built the application and have since moved to your company's competitors for a substantially larger compensation package. For this reason I won't dwell on the benefits that a good type system would bring the language, and in fact there is actually a rather nice type system for Ruby called [Sorbet](https://sorbet.org/), which is optional but makes working with Ruby much more enjoyable.

The primary issue I have with Ruby is that it can be surprisingly difficult to jump in to with no prior knowledge of the language. Most languages such as Go, Python and C are sufficiently similar that the average programmer can skim a piece of code and work out what is going on. With Ruby, this is less true. Consider the following (taken from the [Ruby on Rails](https://github.com/rails/rails/blob/f6fd15cb7563740ba3896207ed28e1308301d9dc/activerecord/lib/active_record/insert_all.rb#L282-L290) source code):

```ruby
def touch_model_timestamps_unless(&block)
  return "" unless update_duplicates? && record_timestamps?

  model.timestamp_attributes_for_update_in_model.filter_map do |column_name|
    if touch_timestamp_attribute?(column_name)
      "#{column_name}=<removed for readability>"
    end
  end.join
end
```

One of the first things you might notice is the `return` on the first line. That's strange, a programmer with no experience in Ruby might think - we're immediately returning without an `if` statement? What is the point in the rest of the function? Of course, this is still a conditional return, but the boolean is placed at the end of the line.

This conditional is also placed after the `unless` keyword, which is identical to _if not_ but with the benefit that an engineer whose Ruby is not fluent will have to expend a non-trivial number of brain cycles applying the negation in their head. With post-fix `if` or `unless`, you first see _what_ will happen, and are left guessing _why_ this will happen until the end of the line. There is a joke about the German language and waiting for the verb, and I'm sure there is a similar pun to be made about Ruby programmers enduring a nail-biting wait for the conditional.

My final gripe with the snippet above is the implicit method calls. From glancing at the code above, you may assume that `update_duplicates?` and `record_timestamps?` are variables defined somewhere. They might be, but there are a plethora of other possibilities, and the answer can only be determined by digging further through the code (or, thanks to the joys of [monkey patching](https://shopify.engineering/the-case-against-monkey-patching), by running it). Indeed, they may be class, module or instance variables, but in this case they are actually methods defined on the class itself. Where are the brackets? Here is the stroke of genius from the designers of Ruby: _you don't need brackets to call a function_. This is genius because it makes the Ruby programmer feel like they are writing cleaner code, while simultaneously making it almost incomprehensible to everyone else, ensuring strong career stability for those well-versed in the language.

In Python, the `return "" unless ...` line would be written more like the following:

```python
if not (update_duplicates() and record_timestamps()):
  return ""
```

While there may be no such thing as objectively readable code, I think that the Python example is more clear to the average programmer making a quick change to a foreign service, or a bleary-eyed on-call engineer trying to work out why the application pods running Ruby are crashing with `undefined local variable or method 'record_timestamps' for main:Object (NameError)` at 3 in the morning.

As an aside, implicit function calling means that you can't pass in a function to another function as you might expect to, as the function will be called when you try and pass it in. Take the following example;

```ruby
def call_with_1(func)
  func(1)
end

def bar(x)
  puts x
end
```

If you wrote the above in most other languages and then called `call_with_1(bar)`, you'd expect `bar` to be called with `1`, which would simply be printed to the console. Instead, you get the following in Ruby:

```bash
(irb):16:in `bar': wrong number of arguments (given 0, expected 1) (ArgumentError)
        from (irb):20:in `<main>'
        from /Users/tschafer/.rbenv/versions/3.2.2/lib/ruby/gems/3.2.0/gems/irb-1.12.0/exe/irb:9:in `<top (required)>'
        from /Users/tschafer/.rbenv/versions/3.2.2/bin/irb:25:in `load'
        from /Users/tschafer/.rbenv/versions/3.2.2/bin/irb:25:in `<main>'
```

Of course, this can be worked around with blocks, procs or lambdas, but to me this seems like unnecessary complexity for no real gain. This sums up my experience with Ruby: the idioms discussed are easy to understand given a modicum of Ruby knowledge, but I think there is value to a language being understandable to the average programmer, and Ruby's quirks often provide substantial downside with marginal upside.

To its credit, some of the most profitable technology companies in the world were built using (and still use) Ruby, so you can take comfort in strong career prospects while you bang your head on your desk trying to work out why your app is so slow.
