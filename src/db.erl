-module(db).
-compile(export_all).

-import(mnesia).
-import(users).

create_database_schema() ->
	mensia:create_schema([node()]),
	mnesia:start(),
	users:create_login_schema().

