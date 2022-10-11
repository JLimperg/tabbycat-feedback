# tabbycat-feedback

After you've run a debating tournament with Tabbycat, you can use this program
to generate anonymised feedback sheets for your adjudicators. Each adjudicator
receives one sheet containing the scores and comments from each round. The
names of teams and adjudicators providing feedback (as well as the teams'
positions in the debate) are not included in the sheets, but note that people
can often guess who the feedback comes from anyway. In particular, when wings
receive feedback from their chair for a specific round, this is hardly
anonymous.

## Installation

I don't provide a ready-made executable version of this program, so you'll have
to build it yourself (sorry):

- Install [the `stack` program](https://haskellstack.org).
- In this directory, run `stack build`. This will download a Haskell compiler,
  download and build all the dependencies and finally build `tabbycat-feedback`
  itself, so it'll take some time.

## Usage

You can now run `tabbycat-feedback` with this command:

```text
stack run -- tabbycat-feedback --token <TOKEN> --url <URL> --basedir output
```

where

- `<TOKEN>` is your Tabbycat API token.
- `<URL>` is the base API URL of your tournament. The URL looks like this:
  `https://<domain>.herokuapp.com/api/v1/tournaments/<tournament>`, where
  `<domain>` is your Heroku domain and `<tournament>` is your tournament slug.

The command will (after some time) generate some files in the `output`
directory:

- One file `<key>.html` for each adjudicator, where `<key>` is that
  adjudicator's URL key.
- One file `style.css`, which is a stylesheet needed to display the HTML files
  correctly.

You can now upload these files to a server, then point each adjudicator to
the URL where they can find their HTML file.

## Cleanup

After you're done with `tabbycat-feedback`, you can:

- Remove `~/.stack`. `stack` stores all its information (especially the Haskell
  compiler, which is quite big) there.
- Remove the folder `.stack-work` in this directory, which was created by
  `stack` when building `tabbycat-feedback`.
- Remove this directory entirely.
