# Natural-SQL Translator

The Natural-SQL Translator aims to provide translation between natural languages and SQL statements.
* It allows for translation in both directions (i.e. natural language to SQL or SQL to natural language).
* It seeks to be general with respect to both natural languages and databases.

It is made up of [Grammatical Framework (GF)](https://www.grammaticalframework.org/) syntaxes and an accessible application.

For natural languages the [Resource Grammar Library (RGL)](https://github.com/GrammaticalFramework/gf-rgl) is used.

Features:
* The grammar can be expanded to new databases from within the application.
* The grammar can be expanded to new languages using GF (with functors being a partially incorporated method).
* The system supports a subset of SQL, but not all of its functionality.
* In its current state it lets users send SQL statements to a [PostgreSQL](https://www.postgresql.org/) database, and see the results from such an operation.
