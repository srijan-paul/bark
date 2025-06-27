# Bark

A Haskell static site generator built for myself,
yet released for any who may find it useful.
Bark is intended to be dead simple, 
yet extensible enough to allow plugins and templates.

## Built-in Features

- CommonMark Markdown with GitHub Flavored Markdown extensions
- Syntax highlighting for code blocks (Fully static, without any JS).
- Incremental rebuilds during watch mode.
- Static file serving during development.

## Installation

```bash
git clone https://github.com/srijan-paul/bark
cd bark
cabal install exe:bark --overwrite-policy=always
```

## Quick Start

```bash
# Create a new project
bark init my-site
cd my-site

# Build the site
bark build

# Watch for changes and serve on localhost:8080
bark watch
```

## Project Structure

```
my-site/
├── bark.yml          # Configuration
├── src/              # Markdown source files
├── template/         # Mustache templates
├── assets/           # CSS, JS, images
├── copy/             # Files to copy as-is
└── build/            # Generated site
```

## Configuration

Edit `bark.yml` to customize paths:

```yaml
source: src
out: build
assets: assets
template: template
copy: copy
```

## Frontmatter

Add YAML frontmatter to your Markdown files:

```markdown
---
title: My First Post
template: post
date: 2024-01-15
---

# Hello World

This is my first post.
```

## Templates

Create Mustache templates in the `template/` directory:

```html
<!-- template/post.mustache -->
<!DOCTYPE html>
<html>
<head>
    <title>{{meta.title}}</title>
</head>
<body>
    <h1>{{meta.title}}</h1>
    <time>{{meta.date}}</time>
    {{{content}}}
</body>
</html>
```

## Plugins

To add custom plugins, use Bark as a library in your own Haskell project rather than the standalone binary.

### Simple Pre-processor Plugin

Add custom data to posts before HTML generation:

```haskell
import Bark.Types

addAuthor :: Plugin
addAuthor = BeforeBuild $ do
  compilation <- get
  let posts = compilationPosts compilation
      modifiedPosts = map addAuthorField posts
  put $ compilation {compilationPosts = modifiedPosts}
  where
    addAuthorField post = 
      post { postData = HM.insert "author" (String "Jane Doe") (postData post) }
```

### Simple Post-processor Plugin

Modify HTML after generation:

```haskell
addFooter :: Plugin
addFooter = AfterBuild $ do
  compilation <- get
  let pages = map appendFooter (compilationPages compilation)
  put $ compilation {compilationPages = pages}
  where
    appendFooter page = 
      page { htmlPageContent = htmlPageContent page <> "\n<footer>© 2024</footer>" }
```

## Custom Binary with Plugins

Create your own Haskell project with Bark as a dependency to use custom plugins:

```haskell
-- site.hs
{-# LANGUAGE OverloadedStrings #-}

import Bark.CLI (BarkCLI(..), builtinPlugins, doCommand, parseCommand)
import System.Environment (getArgs)

-- Your custom plugins here
myPlugins :: [Plugin]
myPlugins = [addAuthor, addFooter]

main :: IO ()
main = do
  let allPlugins = builtinPlugins ++ myPlugins
      cli = BarkCLI allPlugins
  maybeCommand <- parseCommand <$> getArgs
  maybe (return ()) (doCommand cli) maybeCommand
```

Build and install your custom binary:

```bash
cabal install exe:site --installdir=$(pwd) --overwrite-policy=always
./site build
./site watch
```

## Template Data

Posts have access to these template variables:

- `{{content}}` - Rendered HTML from Markdown
- `{{meta.*}}` - Data from YAML frontmatter
- `{{posts}}` - Array of all posts (for indexes)
- Custom fields added by plugins


