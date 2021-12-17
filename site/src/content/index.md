# Bark

Bark is a static site generator written in Haskell, targeting ease of use.

If you have any experience with Hugo, 11ty or Jekyll then using Bark should be a cakewalk.

## Installation

Since bark is in it's experimental stages, the only way to get it currently is to
compile it from source. The source tree can be found [here](https://github.com/srijan-paul/bark) on GitHub. Just clone the repo and run the `build.sh` script to compile.
When finished, you should see an executable in `dist/bark`.

Once it's more stable, bark will be released on hackage.

## Usage

Using bark is as simple as it gets.

### One time setup

Before we continue, make sure you have added `bark` to your `PATH` environment variable.
If you're on Linux and are using bash, then just copy paste the following line into your terminal:

```sh
$ export PATH="path/to/bark":$PATH >> ~/.zshrc
```

Replace `/path/to/bark` with the actual path to the `bark` executable.
Let's do a quick sanity check to ensure all's good.
If you get something similar to what's shown below when you enter `bark` into the command line, then we're all set!

```sh
$ bark
Bark v0.1.0. commands available:
build: build the current project
watch: watch the directory and build whenever changes are detected.
```

### Creating projects

To create a new project, create an empty directory and initialize it with `bark init`.

```sh
$ mkdir project
$ cd project
$ bark init
```

Now your directory should have this structure:

```sh
.
├── src
│   ├── assets
│   ├── content
│   └── public
└── template
```

### Adding new posts

To add a new post, you'll have to create two new files in the `content` directory.

```sh
$ touch src/content/post.md src/content/post.meta
```

The `post.md` file contains the markdown content of our page.
The `post.meta` file contains metadata about our post.
The metadata is stored in a JSON-like format.

Here's an example of what you could write into `post.md`:

```md
# Hello, world!

This is my first post
```

And the corresponding metadata in a `post.meta` file:

```js
{
    date: "17th December 2021",
    tags: ["post", "blog"],
    template: "post",
    title: "My first post!"
}
```

The `template` key **must** be present in a metadata file.
It represents which HTML template to use when rendering the post.

### Using templates

A bark template is written in the [mustache](https://mustache.github.io/) templating language.
If you've used HTML, then you already know most of what you need to know.
In the mustache template, there are two prexisting variables:

1. `content`: The markdown content of the page, now converted to HTML.
2. `meta`: The metadata object present in the `post.meta` file.

Here is an example template:

```html
<html>
  <head>
    <title>{{ meta.title }}</title>
  </head>
  <body>
    <div class="main">
      Date: {{meta.date}}
      <!-- The '&' is neccessary when plugging in HTML,
           otherwise the output would be sanitized -->
      {{&content}}
    </div>
  </body>
</html>
```

### Building your project

This is were everything comes together nicely.

```sh
$ bark build
```

Once you do this, you should see a `build` directory pop up in the project root.
This directory now contains the HTML output of all our files.

## Sample project.

Want a sample project as reference, how about this one?

The page you're reading right now has been made with bark.
You can find the source [here]().

## FAQ

**Q. How can I get syntax highlighting?**
  
Same way I did on this page.
You could use a syntax highlighting library like [highlight.js](https://highlightjs.org/) and initialize it in a `<script>` tag in your template to embed it.

**Q. How can do I get nested/grouped paths to my pages?**

Simple! Just nest your directory structure inside `content` the way you want your URLs to turn out.
The build directory structure mirrors the source.

**Q. Does bark support images and other media content?**

![cat-image](assets/cookedcat.jpg)

**Q. My question isn't listed here !? WTF !?**

That's okay, you can always file an [issue](https://github.com/srijan-paul/bark/issues) to submit bugs/suggestions,
or get in touch with me on my [twitter](https://twitter.com/_injuly) or discord - **injuly#6820**.
