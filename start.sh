# This will not work if an application with a dependency e.g. cowboy is
# declared in 'applications' tuple in the application file.
APP_NAME=sling 
erl -pa ./ebin/ ./deps/*/ebin -eval "application:start($APP_NAME)" -config $APP_NAME $1
