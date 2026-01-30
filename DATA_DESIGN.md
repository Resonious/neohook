# Data Design

This project uses a local Turso database. This means data is on the same machine as the code, so operations are quick and local setup is easy.

To achieve high availability, data may be replicated across different instances of the app running in different locations. This is achieved in the application layer, thus the need for this document, as we need to be careful when creating migrations and SQL queries.

## The Gist of It

* Every table is append-only.
    * No `UPDATE` queries (unless required by law/regulation).
    * No `DELETE` queries.
* Every table has `node TEXT NOT NULL` to track who created the record.
* Every table either has a ulid `id` field or an integer `updated_at` field.
* When querying, only consider rows with the latest `id` or `updated_at` value.
* "Deleting" means creating a new row with important fields set to `NULL`.

## Tradeoffs

This design comes with some sacrifices:

* Consistency is eventual for everything. Data from server1.snd.one will not be available in server2.snd.one instantly (especially considering server2.snd.one may be down).

* Queries are awkward; need to always pull `max(updated_at)`.

* There is some boilerplate code necessary for performing sync, and you have to remember to hard-code each field for each synced table (might be able to further de-boilerplate this).

On the plus side, this means high availability is easy to set up locally and even unit test. We don't need to defer to complex, usually hosted, database setups.
