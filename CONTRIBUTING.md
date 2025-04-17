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
