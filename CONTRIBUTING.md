# Contributing to Mahogany

Want to make a change? All you need to do is submit a pull request!
This document provides some suggestions and recommendations that will
make your contribution more successful.

## Git Commit messages

Try to write a [good commit message](https://cbea.ms/git-commit/).
In short:
+ The first line of the message shouldn't be more than 50 characters
  wide
+ The second line should be blank.
+ The message should complete the sentence "Applying this commit will ..."
+ Explain why the change is needed, not how it works, although that
  can be helpful too.

A useful trick to writing more concise messages is to include a prefix
that indicates what part of the application you are working on. For
example:
+ `Backend: rig up wayland protocol`
+ `frames: fix frame alignment`

## Code formatting and indentation

There is an [.editorconfig](https://editorconfig.org/) file in the
root project directory that tells your editor basic formatting
instructions. Most editors can use this file, but you may have to turn
it on. There is a .dir-locals.el file for this project that enables
emacs support.

### C Code

[clang-format](https://clang.llvm.org/docs/ClangFormat.html) is used
to format C code. Run it on files before saving, and it will take care
of all formatting issues.

### Lisp Code

Try to keep line length to 80 characters or less; sometimes long line
lengths makes code easier to read, so it is okay to have longer lines
if needed.

## Running the Project Interactively

To replicate the build environment as executed by the Makefile,
execute the `init-build-env.lisp` file while `*default-directory*` is
set to wherever the project is cloned:

``` lisp
(let ((clone-dir #p"path/to/dir"))
  #+swank
  (swank:set-default-directory clone-dir)
  #-swank
  (setf *default-directory* clone-dir)
  (load "init-build-env.lisp")
  (asdf:load-system "mahogany"))
```

## Submitting Pull Requests

When submitting a pull request, try to do the following things:
+ Reference an issue if there is a relevant one.
+ For significant changes, create an issue first to discuss the
  implementation and how it fits in with the rest of the project.
+ Ensure your branch is rebased upon the latest commit.

## Additional Tools

If you are changing the C back end's user interface, you will need
[cl-bindgen](https://github.com/sdilts/cl-bindgen) to generate the
lisp interface.

## Improving git blame

There are a few commits that have white space only changes and make it
harder to use `git blame`. You can ignore these commits by adding the
`.git-blame-ignore-revs` file to your git config:

``` bash
git config blame.ignoreRevsFile .git-blame-ignore-revs
```

## AI Usage

AI usage is strongly discouraged. The process that surrounds open source
software is ultimately about human collaboration, and LLMs bypass this. This
[blog article](https://simonwillison.net/2026/Apr/30/zig-anti-ai/) on Zig's AI
ban puts it best:

> Zig values contributors over their contributions. Each contributor represents
  an investment by the Zig core team - the primary goal of reviewing and
  accepting PRs isn't to land new code, it's to help grow new contributors who
  can become trusted and prolific over time.

> LLM assistance breaks that completely. It doesn't matter if the LLM helps you
  submit a perfect PR to Zig - the time the Zig team spends reviewing your work
  does nothing to help them add new, confident, trustworthy contributors to
  their overall project.

The rules surrounding the usage of AI tools in this repository try to put this
sentiment into practice.

### Usage
+ *No autonomous AI agent use or vibe coding.*
+ *No AI-generated text in human-to-human communication*. This includes PR
  descriptions.
  - Machine translations or other similar uses are acceptable, as long as you
    wrote the original content.
+ *Do not use AI to generate user-facing media (e.g. documentation, images and
  audio)*.
  + Documentation intended for developers and source code comments is excluded;
    AI tools should explain why they wrote the code they did.
+ *Do not include AI tools as the co-author of a commit or PR*.
+ *Include what parts of the commit are authored by AI.*
  + See the [SBCL repository](https://github.com/sbcl/sbcl) for some good
    examples. Including something like "Implementation mostly done by $TOOL with
    prose completely rewritten by me." at the bottom of the commit works.
