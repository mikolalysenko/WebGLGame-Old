-module(users).
-compile(export_all).

-import(application).
-import(dict).
-import(mnesia).

%Client login info
-record(login, {name, password}).

%Initialize the login schema
create_login_schema() ->
	mnesia:create_table(login, [
		{attributes, record_info(fields, login_info)} ]).

%Verifies a user name/password combo
verify_password(Name, Password) ->
	case mnesia:read(login, Name) of
		[] ->
			false;
		[ Record ] ->
			case Record#login.password of
				Password ->
					true;
				_ ->
					false
			end
	end.

%Adds a user
add_user(Name, Password) ->
	%The update transaction
	T = fun() ->
		%Check if user exists
		U = mnesia:read(login, Name),
		case U of
			[] ->
				mnesia:write(#login{
					name = Name, 
					password = Password}),
				ok;
			_ ->
				{failure, "User already exists"}
		end
	end,
	
	case mnesia:transaction(T) of
		{atomic, Result} ->
			Result;
		_ ->
			{failure, "Database error" }
	end.

