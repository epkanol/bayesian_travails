# Data Description

## Tools

Two tools have been used to generate the dataset.

* Git is used to calculcate gross number of added and removed lines per file in each commit. All types of lines are included here - source code, comments, whitespace.
* Sonar is used to calculate CLOC, complexity metrics and number of duplicates in the current and prior commit.

## Column descriptions

1. `commitid` - categorical, Commit id. Files changed in the same git commit share the same commit id.
2. `repo` - categorical, Repository where the changed file is located.
3. `file` - categorical, File identifier, unique within the repository
4. `istestfile` - logical, TRUE if the file belongs to a test subdirectory (either unit test or integration test)
5. `isnewfile` - logical, TRUE if the file is introduced in the current commit.
6. `author` - categorical, Obfuscated email of Git `Author:` (typically person initiating the change)
7. `authorteam` - categorical, Team where the author was allocated *at the time of the change*.
8. `committer` - categorical, Obfuscated email of Git `Committer:` (person that merges the change)
9. `committerteam` - categorical, Team where the committer was allocated *at the time of the change*.
10. `added` - integer >= 0, Total number of added lines according to Git
11. `removed` - integer >= 0, Total number of removed lines according to Git
12. `changereason` - categorical, The reason for the change, see below
13. `currCloc` - integer >= 0, Sonar calculated CLOC value, after the commit was applied
14. `currComments` - integer >= 0, Sonar calculated number of comment lines, after the commit was applied
15. `currComplex` - integer >= 0, Sonar calculated McCabe complexity, after the commit was applied
16. `currDupLines` - integer >= 0, Sonar calculated duplicated lines, after the commit was applied
17. `currDupBlocks` - integer >= 0, Sonar calculated duplicated blocks, after the commit was applied
18. `prevCloc` - integer >= 0, Sonar calculated CLOC value, before the commit was applied
19. `prevComments` - integer >= 0, Sonar calculated number of comment lines, before the commit was applied
20. `prevComplex` - integer >= 0, Sonar calculated McCabe complexity, before the commit was applied
21. `prevDupLines` - integer >= 0, Sonar calculated duplicated lines, before the commit was applied
22. `prevDupBlocks` - integer >= 0, Sonar calculated duplicated blocks, before the commit was applied
23. `delta` - integer, The difference between the number of duplications before and after the commit was applied. If positive, the number of duplications increased, if negative, the number of duplications decreased.

### Repositories

The repositories are mostly named after our neighboring planets, with the exception of the `IntTest` repository, which contains integration-level tests (component interaction tests). As can be expected, a large majority of the files in this repository is indicated with `istestfile` being `TRUE`.

Files in the other repositories that have `istestfile=TRUE` are either unit tests or files used to support unit testing activities.

End-to-end acceptance tests are not implemented in Java, and are not included in this data set.

### Teams

Two of the teams are special - the `Arch` team contains technical architects and leaders.
Some authors join this team after first having participated in a regular ("coloured") team.

One team, denoted `UI`, is specialized on GUI tasks, and are typically working in JavaScript frameworks - not studied in this data set. In some repos, they still made some contributions to this code base. They are indicated as the `UI` team.

There are also some authors whose team affiliation is unknown - they were not recorded in the relevant organization chart. These are indicated as the `Unknown` team.

The rest of the teams are named after standard colours, in no particular order.

### Change Reason

The reason for the change is derived from comments the user voluntarily makes in the commit message. While there was an expectation (and tool reminders) that users make this indication, some users and tools did not supply any change reason.

* `R` - The change to the file was initiated by a new requirement or feature.
* `I` - The change to the file was initiated by a spontaneous improvement, not directly tied to a new requirement or existing fault correction.
* `F` - The change to the file was initiated by a fault that was fixed.
* `?` - Unknown change reason - either an automated change (e.g. a revert of previous change), or the author/committer chose not to indicate the reason for the change.