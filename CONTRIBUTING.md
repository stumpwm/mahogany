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
+ Explain why the change is needed, not how it works, athough that
  can be helpful too.

A useful trick to writing more consise messages is to include a prefix
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
lenghts makes code easier to read, so it is okay to have longer lines
if needed.

## Submitting Pull Requests

When submitting a pull request, try to do the following things:
+ Reference an issue if there is a relevant one.
+ For significant changes, create an issue first to discuss the
  implementation and how it fits in with the rest of the project.
+ Ensure your branch is rebased upon the lastest commit.

## Additional Tools

If you are changing the C back end's user interface, you will need
[cl-bindgen](https://github.com/sdilts/cl-bindgen) to generate the
lisp interface.
