# review-camelcase

![elm package](https://img.shields.io/elm-package/v/sparksp/elm-review-camelcase)
![elm-review 2.0](https://img.shields.io/badge/elm--review-2.0-%231293D8)
![elm 0.19](https://img.shields.io/badge/elm-0.19-%231293D8)
![Tests](https://github.com/sparksp/elm-review-camelcase/workflows/Tests/badge.svg)

Provides an [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule to ensure your code uses only camelCase and PascalCase for variables, constants and other declarations.

## Provided rule

- [`UseCamelCase`](https://package.elm-lang.org/packages/sparksp/elm-review-camelcase/1.1.0/UseCamelCase) - Reports code written in the wrong case style.

## Example configuration

```elm
module ReviewConfig exposing (config)

import Review.Rule exposing (Rule)
import UseCamelCase


config : List Rule
config =
    [ UseCamelCase.rule UseCamelCase.default
    ]
```

Detailed [configuration documentation](https://package.elm-lang.org/packages/sparksp/elm-review-camelcase/1.1.0/UseCamelCase#configuration) is provided with the rule.
