# Tracing.Newrelic

[![Travis CI Build Status](https://travis-ci.org/NoRedInk/tracing-newrelic.svg)](https://travis-ci.org/NoRedInk/tracing-newrelic)

`Tracing.Newrelic` is a Haskell package to report to [New Relic](https://newrelic.com/).

It works by wrapping the [New Relic C SDK](https://github.com/newrelic/c-sdk).

To use this package you need to have the NewRelic daemon running. To do this, please
download the [New Relic C SDK](https://github.com/newrelic/c-sdk) and follow the steps
described [here](https://github.com/newrelic/c-sdk#building-the-c-sdk).

## Example usage

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Tracing.NewRelic

main :: IO ()
main = do
  appConfig <- createAppConfig (AppName "My app") (LicenseKey "*****")
  app <- createApp appConfig (TimeoutMs 10000)

  tx <- startWebTransaction app "Here comes a request"
  segment <- startSegment tx (Just "Some name") (Just "Some category")

  -- Some expensive computation...

  _ <- endSegment segment
  _ <- endTransaction tx
```
