# Agent Shell - Project Guidelines

## Communication norms

PR and issue conversations are human relationships. The maintainer prefers
talking directly to humans.

When contributing:

- Write your own PR descriptions and issue comments. Don't have AI generate them.
- If you used AI to research something, summarize the findings in your own words
  and give your level of endorsement rather than pasting AI output verbatim.
  Concise, human-written summaries save the maintainer from having to parse
  lengthy generated text.
- Review all code in your PR yourself and vouch for its quality.

## Contributing

This is an Emacs Lisp project. See [CONTRIBUTING.org](CONTRIBUTING.org) for style guidelines, code checks, and testing. Please adhere to these guidelines.

## Development workflow

When adding or changing features:

1. **Run `bin/test`.** Set `acp_root` and `shell_maker_root` if the
   deps aren't in sibling worktrees. This runs byte-compilation, ERT
   tests, and checks that `README.org` was updated when code changed.
2. **Keep the README features list current.** The "Features on top of
   agent-shell" section in `README.org` must be updated whenever code
   changes land. Both `bin/test` and CI enforce this — changes to `.el`
   or `tests/` files without a corresponding `README.org` update will
   fail.
