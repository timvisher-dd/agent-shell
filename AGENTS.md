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


## Testing (batch)

When running ERT from the CLI, use `-Q` so Emacs does not load user init files.
Load tests explicitly, for example:

```
emacs --batch -Q -L . --eval '(mapc #'load (file-expand-wildcards "tests/*.el"))' \
  -f ert-run-tests-batch-and-exit
```

Add any required `-L` entries for local dependencies.

