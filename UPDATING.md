# Updating the database

The published Docker image `lazamar/nix-package-versions` has everything
needed to update the database: the built program, Nix and `sqlite3`. CI
publishes it on every merge to `main`, so an update is one `docker run` with
the existing database mounted as a volume.

## What you need

- **A Linux machine with Docker.** Each evaluation of a nixpkgs revision uses
  up to about 5 GB of RAM (measured on nixos-26.05 in September 2026). The
  machine needs at least 8 GB for one evaluation at a time. With 16 GB you can
  run 2 at once, and with 32 GB, 4. More CPU cores only help if you have the
  RAM to run more evaluations in parallel. An hourly-billed VPS that you delete
  afterwards works well.
- **Disk space** for the database (13 GB in September 2026, growing about
  35 MB per revision), plus about 8 GB for the image. Nix also keeps each
  evaluated revision's source inside the container, about 1 GB per revision,
  which is freed when the container is removed.
- **A GitHub token.** A classic personal access token with no scopes is
  enough, because it's only used to list nixpkgs commits. Without a token,
  GitHub allows 60 requests per hour, which isn't enough for an update.
- **The current `DATABASE.db`.**

## Steps

1. Put the database in a directory of its own. The program expects it to be
   called `DATABASE.db`:

   ```
   mkdir database
   mv DATABASE.db database/
   ```

   Always mount this directory, not the file itself. The database is in
   SQLite's WAL mode, so SQLite needs to create `DATABASE.db-wal` and
   `DATABASE.db-shm` next to it.

2. Pull the image:

   ```
   docker pull lazamar/nix-package-versions:latest
   ```

3. Find where the data currently ends. This lists the newest commit indexed
   for each channel, with the active channels first:

   ```
   docker run --rm -v "$PWD/database":/home/app/database \
     --entrypoint sqlite3 lazamar/nix-package-versions:latest \
     -readonly /home/app/database/DATABASE.db \
     "SELECT CHANNEL, date(MAX(COMMIT_DATE), 'unixepoch') FROM coverage GROUP BY CHANNEL ORDER BY 2 DESC LIMIT 10"
   ```

4. Start the update. Use a `--from` date a month or so before the oldest date
   among the active channels in step 3. Periods that are already covered are skipped, so an earlier date is
   safe; it only costs some extra GitHub requests. `--until` defaults to
   today.

   ```
   export GITHUB_TOKEN=ghp_...

   docker run -d --name npv-update \
     -v "$PWD/database":/home/app/database \
     lazamar/nix-package-versions:latest \
     update \
       --from 2026-08-01 \
       --github-user <your GitHub username> \
       --github-token "$GITHUB_TOKEN" \
       --db-root /home/app/database \
       +RTS -N2 -RTS
   ```

   `-N2` is the number of revisions evaluated at the same time. Set it to
   about your RAM in GB divided by 6. Without it, the program runs one
   evaluation per CPU core, which can run out of memory.

   The container runs in the background, so you can disconnect from the
   machine while it works. To follow its progress:

   ```
   docker logs -f npv-update
   ```

5. When the container has exited, check the summary at the end of the log,
   and run the query from step 3 again. The unstable channels and the
   current releases should now show commits from the last few weeks.

   ```
   docker logs npv-update 2>&1 | tail -5
   docker rm npv-update
   ```

   `docker rm` can take a few minutes, because it deletes the nixpkgs sources
   the container accumulated.

6. Copy `database/DATABASE.db` to where it's served. If a non-empty
   `DATABASE.db-wal` is left next to it (for example after an interrupted
   run), merge it into the database first:

   ```
   docker run --rm -v "$PWD/database":/home/app/database \
     --entrypoint sqlite3 lazamar/nix-package-versions:latest \
     /home/app/database/DATABASE.db "PRAGMA wal_checkpoint(TRUNCATE);"
   ```

## Reading the log

- `progress: N/M`: the program is fetching the commit list for channel period
  N out of M.
- `<commit> Nix: loading packages` … `Writing finished`: a revision was
  evaluated and saved.
- `Nix: failed`: the revision couldn't be evaluated and is recorded as
  `Broken`. The program then tries another commit from the same period.
  `Broken` commits are tried again on later runs.
- `Success: <channel> <period> <commit>` or `Failure: <channel> <period>`: the
  outcome for each channel period. Old releases whose branches no longer exist
  on GitHub always end in `Failure`. That's expected.

## Stopping and resuming

`docker stop npv-update` is safe at any time. Running the same command again
picks up where the last run stopped: covered periods are skipped, and a
revision that was only partly written is evaluated again.

## How long it takes

In September 2026, on an 8 vCPU / 32 GB machine with `-N4`, the update wrote
1 to 2 revisions per minute. Each month of missing data needs roughly 10
revisions across the active channels, so a monthly update takes well under an
hour.

## Building the image yourself

If the published image is outdated, build it from the repository:

```
docker build -t lazamar/nix-package-versions:latest .
```

The build compiles all Haskell dependencies. On a machine with less than
about 8 GB of RAM, limit cabal to one job (`cabal v2-build -j1`) in a local
copy of the Dockerfile, or the build can run out of memory.
