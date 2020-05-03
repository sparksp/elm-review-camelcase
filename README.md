# review-case

Provides an [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule to ensure your code uses camelCase and PascalCase.

## Provided rule

- [`UseCamelCase`](https://package.elm-lang.org/packages/sparksp/elm-review-camelcase/latest/UseCamelCase)

## Example configuration

```elm
module ReviewConfig exposing (config)

import Review.Rule exposing (Rule)
import UseCamelCase


config : List Rule
config =
    [ UseCamelCase.rule []
    ]
```

Detailed [configuration documentation](https://package.elm-lang.org/packages/sparksp/elm-review-camelcase/latest/UseCamelCase#Option) is provided with the rule.
