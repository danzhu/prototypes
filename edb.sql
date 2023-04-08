create table if not exists target(
    id integer primary key,
    expr text not null unique,
    result text not null default "",
    outdated integer not null default 1
);

create table if not exists depend(
    dependent integer not null references target(id),
    dependency integer not null references target(id),
    unique(dependent, dependency)
);
create index if not exists depend_consumer on depend(dependency);

pragma journal_mode = wal;
