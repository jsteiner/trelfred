# Trelfred

A CLI for searching [Trello] boards, formatted for [Alfred].

[Trello]: https://trello.com
[Alfred]: https://www.alfredapp.com/

Supports the following commands:

1. Cache Trello boards: stores board data in a CSV file in the current directory

   ```
   $ trelfred cache
   ```

1. Search Trello boards: returns all boards matching the given query in
   Alfred's XML format. It will automatically cache Trello boards for you if no
   cache file exists.

   ```
   $ trelfred search foobar
   ```

1. Increment board visits: used to sort boards by frequency of visits

   ```
   trelfred increment https://trello.com/b/board-id/board-name
   ```

## Credentials

Trelfred expects credentials be exposed in a `.env` file in the current
directory. The file should have the following structure / keys:

```
TRELLO_USERNAME=your-username
TRELLO_API_KEY=your-developer-key
TRELLO_API_TOKEN=your-trello-token
```
