---
template: page
title: Bark - Static Site Generator
description: A static site generator written in Haskell
---

# Bark

A static site generator written in Haskell. Based on Markdown + Mustache + YAML frontmatter.
Bark has all the features you expect out of a static site generator, which is to say:

- Markdown support with YAML frontmatter.
- Mustache templates for HTML generation.
- Static assets like CSS, JS, and images.
- Incremental builds with live reload.
- Custom plugins for extending functionality.
- Zero JS Syntax highlighting for code blocks.
- Clean URLs based on file structure.

## Installation

Clone the repository and build from source:

```bash
git clone https://github.com/srijan-paul/bark
cd bark
cabal install exe:bark --overwrite-policy=always
```

## Quick Start

Create and build your first site:

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

Bark organizes your site into clear, predictable directories. Each serves a specific purpose in the build process:

```
my-site/
├── bark.yml          # Configuration file
├── src/              # Markdown source files
├── template/         # Mustache templates
├── assets/           # CSS, JS, images (copied with structure preserved)
├── copy/             # Files to copy directly to site root
└── build/            # Generated site (created automatically)
```

**Directory purposes:**
- **src/**: Your Markdown content files with YAML frontmatter
- **template/**: Mustache templates that define your site's HTML structure  
- **assets/**: Static assets like stylesheets and images (maintains directory structure)
- **copy/**: Files copied directly to your site's root (robots.txt, favicon.ico, etc.)
- **build/**: Generated output directory (don't edit manually)

## Configuration

Edit `bark.yml` to customize directory paths:

```yaml
source: src
out: build
assets: assets
template: template
copy: copy
```

All paths are relative to your project root.

## Writing Content

Content in Bark is written in Markdown with YAML frontmatter for metadata.
This combination gives you readable source files with structured data for templating.

### Frontmatter

Every Markdown file should start with YAML frontmatter between `---` delimiters.
This metadata controls how your content is processed and what data is available to templates:

```markdown
---
title: My First Post
template: post
date: 2024-01-15
tags: [haskell, programming]
---

# Hello World

This is my first post written in Markdown.
```

The `template` field is required and tells Bark which template file to use for rendering. You can add any other fields you want and access them in templates via `{{meta.fieldname}}`.

### URL Structure

Bark converts your file structure to URLs automatically:

- `src/index.md` → `/index.html`
- `src/about.md` → `/about/index.html`
- `src/blog/post.md` → `/blog/post/index.html`

Files named `index.md` create clean URLs without the filename.

## Templates

Templates control how your Markdown content becomes HTML.
Bark uses Mustache templates, which are simple and powerful.

### Basic Template

Create Mustache templates in the `template/` directory.
Each template is a regular HTML file with special placeholders for dynamic content:

```html
<!-- template/post.mustache -->
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>{{meta.title}}</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
</head>
<body>
    <article>
        <h1>{{meta.title}}</h1>
        <time datetime="{{meta.date}}">{{meta.date}}</time>
        {{{content}}}
    </article>
</body>
</html>
```

### Template Variables

Every template has access to several built-in variables that you can use to display content:

- `{{content}}` - The rendered HTML from your Markdown content
- `{{meta.*}}` - Any field from your post's YAML frontmatter (e.g., `{{meta.title}}`, `{{meta.date}}`)
- `{{posts}}` - An array of all posts in your site (useful for creating index pages)
- Custom fields added by your plugins

You can access nested frontmatter data using dot notation. For example, if your frontmatter has `author: {name: "John", email: "john@example.com"}`, you can use `{{meta.author.name}}`.

### Post Lists

Create index pages that list other posts. The `{{#posts}}` syntax iterates over all posts in your site:

```html
<!-- template/index.mustache -->
<ul>
{{#posts}}
    <li>
        <a href="/{{url}}">{{meta.title}}</a>
        <time>{{meta.date}}</time>
    </li>
{{/posts}}
</ul>
```

## Assets and Static Files

### Assets Directory

Files in `assets/` are copied to the output directory maintaining their structure:

- `assets/css/style.css` → `build/css/style.css`
- `assets/images/logo.png` → `build/images/logo.png`

### Copy Directory

Files in `copy/` are copied directly to the build root:

- `copy/favicon.ico` → `build/favicon.ico`
- `copy/robots.txt` → `build/robots.txt`

Use this for files that should appear at your site's root.

## Plugins

Bark's plugin system lets you extend the build process with custom functionality. To use plugins, you'll need to create your own Haskell project that uses Bark as a library rather than using the standalone binary.

Plugins come in two types: preprocessors that modify posts before HTML generation, and postprocessors that modify the final HTML pages.

### Pre-processor Plugin

Preprocessors run before Markdown is converted to HTML, letting you add custom data fields, modify content, or filter posts. Here's a simple example that adds an author field to all posts:

```haskell
import Bark.Types
import qualified Data.HashMap.Strict as HM

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

Now all your templates can access `{{author}}` to display the author name.

### Post-processor Plugin

Postprocessors run after HTML generation, allowing you to modify the final output. This is useful for adding analytics code, modifying markup, or injecting additional content:

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

This plugin adds a copyright footer to every generated page.

### Complex Plugin Example

Here's a more sophisticated plugin that generates individual pages for each tag used in your blog posts. This demonstrates working with frontmatter data, creating new pages, and using templates:

```haskell
generateTagPages :: Plugin
generateTagPages = AfterBuild $ do
  compilation <- get
  let posts = compilationPosts compilation
      tags = extractAllTags posts
      project = compilationProject compilation

  tagPages <- mapM (createTagPage project posts) tags
  put $ compilation { compilationPages = compilationPages compilation ++ tagPages }
  where
    extractAllTags = nub . concatMap getPostTags
    
    getPostTags post = 
      case HM.lookup "tags" (fmMetaData $ postFrontMatter post) of
        Just (Array tags) -> mapMaybe extractTag (toList tags)
        _ -> []
    
    extractTag (String t) = Just t
    extractTag _ = Nothing
    
    createTagPage project posts tag = do
      let taggedPosts = filter (hasTag tag) posts
          templatePath = projectTemplateDir project </> "tag.mustache"
          tagData = HM.fromList [("tag", String tag), ("posts", Array $ Vector.fromList $ map postToValue taggedPosts)]
      html <- template2Html templatePath (Object tagData)
      let fileName = T.toLower (T.replace " " "-" tag) <> ".html"
          outPath = projectOutDir project </> "tags" </> T.unpack fileName
      return $ HTMLPage Nothing html outPath
```

## Custom Binary

Create your own Haskell project with Bark as a dependency:

```haskell
-- site.hs
{-# LANGUAGE OverloadedStrings #-}

import Bark.CLI (BarkCLI(..), builtinPlugins, doCommand, parseCommand)
import System.Environment (getArgs)

-- Import your custom plugins
myPlugins :: [Plugin]
myPlugins = [addAuthor, addFooter, generateTagPages]

main :: IO ()
main = do
  let allPlugins = builtinPlugins ++ myPlugins
      cli = BarkCLI allPlugins
  maybeCommand <- parseCommand <$> getArgs
  maybe (return ()) (doCommand cli) maybeCommand
```

### Building Your Site Binary

Add Bark as a dependency in your `cabal` project:

```cabal
executable site
  main-is: site.hs
  build-depends: 
    base >= 4.7 && < 5,
    bark,
    text,
    containers
```

Build and install:

```bash
cabal install exe:site --installdir=$(pwd) --overwrite-policy=always
./site build
./site watch
```

## Built-in Features

### Markdown Processing

- CommonMark specification compliance
- GitHub Flavored Markdown extensions
- Automatic syntax highlighting for code blocks
- Smart punctuation and typography

### Development Workflow

- Incremental rebuilds during watch mode
- Static file serving on localhost:8080
- Live reload when files change
- Comprehensive error reporting

### Performance

- Only rebuilds changed files during watch mode
- Efficient asset copying
- Parallel processing where possible

## Commands

### build

Generate the complete site:

```bash
bark build [path]
```

If no path is provided, uses the current directory.

### watch

Build the site and watch for changes:

```bash
bark watch [path]
```

Serves the site on `http://localhost:8080` and rebuilds automatically when files change.

### init

Create a new Bark project:

```bash
bark init [path]
```

Creates the directory structure and configuration file. Uses current directory if no path provided.

## Library Usage

Import Bark modules in your Haskell project:

```haskell
import Bark.Core
import Bark.Types
import Bark.CLI
import Bark.FrontMatter
```

Key types and functions:

- `Project` - Represents a Bark project with all its paths
- `Post` - A markdown file with frontmatter and metadata
- `Plugin` - Custom processors for posts or pages
- `buildProject` - Build a project programmatically
- `readBarkProject` - Load a project from a directory

## Examples

### Blog with Archive

Create an archive page listing all posts:

```markdown
---
template: archive
title: Archive
---

# All Posts

This page lists all posts in the site.
```

```html
<!-- template/archive.mustache -->
<h1>{{meta.title}}</h1>
<div class="archive">
{{#posts}}
  <article>
    <h2><a href="/{{url}}">{{meta.title}}</a></h2>
    <time>{{meta.date}}</time>
    {{#meta.description}}<p>{{meta.description}}</p>{{/meta.description}}
  </article>
{{/posts}}
</div>
```

### Navigation Menu

Add navigation to all pages:

```haskell
addNavigation :: Plugin
addNavigation = BeforeBuild $ do
  compilation <- get
  let posts = compilationPosts compilation
      navItems = filter isNavItem posts
      navData = Array $ Vector.fromList $ map toNavItem navItems
      addNav post = post { postData = HM.insert "navigation" navData (postData post) }
  put $ compilation { compilationPosts = map addNav posts }
  where
    isNavItem post = 
      case HM.lookup "nav" (fmMetaData $ postFrontMatter post) of
        Just (Bool True) -> True
        _ -> False
    
    toNavItem post = Object $ HM.fromList
      [ ("title", String $ fromMaybe "Untitled" $ getTitle post)
      , ("url", String $ T.pack $ postUrl post)
      ]
```

## Troubleshooting

### Build Errors

If builds fail, check:

1. YAML frontmatter syntax is valid
2. Required template files exist
3. Template field is specified in frontmatter
4. No circular template dependencies

### Plugin Issues

Common plugin problems:

- Import the correct Bark modules
- Use `BeforeBuild` for post processing, `AfterBuild` for HTML processing
- Handle `Maybe` values from frontmatter fields
- Update compilation state with `put`

### Performance

For large sites:

- Use incremental builds with `bark watch`
- Minimize plugin complexity
- Consider using `copy/` for large static assets
- Profile custom plugins for bottlenecks

## Contributing

Bark is open source. Submit issues and pull requests on [GitHub](https://github.com/srijan-paul/bark).

### Development Setup

```bash
git clone https://github.com/srijan-paul/bark
cd bark
cabal build
cabal test
```

### Architecture

- `Bark.Core` - Main build pipeline
- `Bark.Types` - Core data types
- `Bark.CLI` - Command line interface
- `Bark.FrontMatter` - YAML parsing
- `Bark.Processors.*` - Built-in plugins
