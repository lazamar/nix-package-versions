# Migration scripts

This query was used to migrate from having all package versions for each revision to
having only the latest possible revision for each package version


Query to create the new `revision` table

    insert into revision
    select COMMIT_HASH, DAY, CHANNEL, REPRESENTS_DAY, STATE
    from REVISIONS
    inner join REVISION_COMMITS
    on REVISIONS.COMMIT_HASH = REVISION_COMMITS.HASH
    limit 10;

Query to create the new `package` table

    insert into package
    select
        j.PACKAGE_NAME,
        j.VERSION_NAME,
        j.CHANNEL,
        j.REVISION_HASH,
        j.DESCRIPTION,
        j.NIXPATH,
        j.REPRESENTS_DAY
    from
        (select *
        from PACKAGE_VERSIONS
        inner join  REVISIONS
        on PACKAGE_VERSIONS.REVISION_HASH = REVISIONS.COMMIT_HASH
        ) as j
    inner join
        (select PACKAGE_NAME, VERSION_NAME, CHANNEL, MAX(REPRESENTS_DAY) as DATE
        from PACKAGE_VERSIONS
        inner join  REVISIONS
        on PACKAGE_VERSIONS.REVISION_HASH = REVISIONS.COMMIT_HASH
        group by PACKAGE_NAME, VERSION_NAME, CHANNEL
        ) as g
    on      j.PACKAGE_NAME = g.PACKAGE_NAME
        and j.VERSION_NAME = g.VERSION_NAME
        and j.CHANNEL      = g.CHANNEL
        and j.REPRESENTS_DAY = g.DATE

# Useful SQL commands

### Create REVISION_COMMITS from REVISIONS
I created the table in Haskell
Then in the repl

    insert or ignore into REVISION_COMMITS SELECT COMMIT_HASH, COMMIT_DAY, STATE from REVISIONS;

# Duplicate table (Doesn't create the columns with the correct types)

    create table MY_TABLE as select * from OTHER_TABLE
