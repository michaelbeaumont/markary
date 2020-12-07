# Markary

This is a collection of Pandoc filters and configurations, used to generate
https://beaumont.dev

NOTE: There may still be some CSS here which isn't completely independent of that use
case.

# Components

Each filter fulfills a specific purpose.

## include-git

It allows us to include code at specific commits as well as diffs:

````
```{uri="src/status.rs" ref=v1_status_on_short diff=v1_status_on_short~ a=12 b=16 .rust}

```
````

It must be run in the relevant `git` directory for this filter to work.

## self-link-headers

For all headers, add a link to themselves if they don't have a link already.

## posts

Read metadata files off disk to create a list of posts, ordered by date. See
https://beaumont.dev/posts.html.

## Example usage

Example use in a `Makefile`:

```
SHELL := /bin/bash
markary := ../markary

# The idea is to use the default YAMLs provided by markary
# while adding site specific CSS/HTML
# We have `envsubst` because of https://github.com/jgm/pandoc/issues/5871.
# Render all our pages and render a list of pages
.PHONY: posts-list
posts-list: posts
	tmpdef=$$(mktemp); \
	MARKARY=${markary} envsubst < ${markary}/posts.yaml > $${tmpdef}; \
	pandoc posts.md \
	  --include-before-body=header.html \
	  --css=header.css \
	  --css=posts.css \
	  --defaults=$${tmpdef} \
	  --metadata=posts-metadata:"$$(ls docs/*.json)" \
	  -o "docs/posts.html";

# The `.json` files are used by the next target below.
.PHONY: posts
posts:
	mkdir -p docs; \
	tmpdef=$$(mktemp); \
	MARKARY=../${markary} envsubst < ${markary}/markary.yaml > $${tmpdef}; \
	cd farsign; \
	for post in posts/*.md; do \
	  pagename="$$(basename $${post%.md})"; \
	  sed '/\.\.\./q' "$${post}" | yq "." > "../docs/$${pagename}.json" ;\
	  pandoc $${post} \
	    --include-before-body=../header.html \
	    --css=../header.css \
	    --resource-path=../${markary}:../:. \
	    --defaults=$${tmpdef} \
	    -o "../docs/$${pagename}.html"; \
	done
```
