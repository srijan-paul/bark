# Bark Documentation Site

This directory contains the Bark documentation website.

## Building

From the repository root:

```bash
# Build the bark binary
cabal build

# Build the site
dist-newstyle/build/*/ghc-*/bark-*/x/bark/build/bark/bark build

# Or using a shell alias
bark build
```

## Development

Watch for changes and serve locally:

```bash
bark watch
```

Then visit http://localhost:8080

## Structure

- `src/index.md` - Main documentation content
- `template/page.mustache` - HTML template with navigation
- `assets/css/` - Stylesheets with typography and layout
- `copy/` - Files copied to site root
- `build/` - Generated site output
