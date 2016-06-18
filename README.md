# Trelfred

A CLI for caching [Trello] boards. Writes a JSON file structured for [Alfred].

[Trello]: https://trello.com
[Alfred]: https://www.alfredapp.com/

## Trelfred Workflow

Trelfred is used by the [Trelfred Workflow]. It looks something like this:

![Trelfred demo](https://cloud.githubusercontent.com/assets/466493/14092917/223ea38e-f518-11e5-9ced-0ef55bb2cad9.gif)

[Trelfred Workflow]: https://github.com/jsteiner/trelfred-cache/releases

## Setup

Trelfred expects the following ENV variables to be exported:

* `TRELLO_USERNAME` - your username
* `TRELLO_API_KEY` - Get [here](https://trello.com/1/appKey/generate).
* `TRELLO_API_TOKEN` - Visit the following URL **replacing the api key** in the URL:

        https://trello.com/1/authorize?key=REPLACE_WITH_YOUR_DEVELOPER_KEY&name=Trelfred&expiration=never&response_type=token

## License

Trelfred is Copyright © 2016 Josh Steiner. It is free software, and
may be redistributed under the terms specified in the LICENSE file.
