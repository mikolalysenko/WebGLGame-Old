-module(login).

-import(application).
-import(dict).
-import(mnesia).

%Client login info
-record(login_info, {name, password}).

%Initialize the login schema
create_login_schema() ->
	application:
	ok.

%Logs into the server
login(Name, Password) ->
	ok.

%Logs a player out of the server
logout(Name, Password) ->
	ok.

%Verifies a user name/password combo
verify_password(Name, Password) ->
	ok.
	
%Adds a user
add_user(Name, Password) ->
	ok.
	
%Deletes a user
delete_user(Name) ->
	ok.

