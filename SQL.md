# Useful SQL commands

### Create REVISION_COMMITS from REVISIONS
I created the table in Haskell
Then in the repl

    insert or ignore into REVISION_COMMITS SELECT COMMIT_HASH, COMMIT_DAY, STATE from REVISIONS;

# Duplicate table (Doesn't create the columns with the correct types)

    create table MY_TABLE as select * from OTHER_TABLE
