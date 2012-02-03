-module(mbtiles).
-behaviour(gen_server).
-export([start/1, stop/1, get/2, get/5, get/6, put/7, put/6, put/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% @spec start(Arg :: atom())
%start it up with the Arg being the name of a MBTILE set
start(Arg) -> gen_server:start_link({local, Arg}, ?MODULE, Arg, []).
stop(Which) -> gen_server:call(Which, stop).


%%@spec get(What::{tile|grid},{tms|xyz|[]}, Zoom, X, Y)
%get a tile or grid
get(Which, What, tms, Z, X, Y) -> gen_server:call(Which, {get, What, Z, X, Y});
get(Which, What, xyz, Z, X, Y) -> gen_server:call(Which, {get, What, Z, X, flipY(Y,Z)}).

get(Which, What, Z, X, Y) -> get(Which, What, xyz, Z, X, Y).

get(Which, I) -> gen_server:call(Which, {get, I}).
put(Which, What, tms, Z, X, Y, Data) -> gen_server:call(Which, {put, What, Z, X, Y, Data});
put(Which, What, xyz, Z, X, Y, Data) -> gen_server:call(Which, {put, What, Z, X, flipY(Y,Z), Data}).

put(Which, What, Z, X, Y, Data) -> put(Which, What, xyz, Z, X, Y, Data).

put(Which, Name, Value) -> gen_server:call(Which, {put,Name, Value}).
flipY(Y,Z) ->
try round(math:pow(2,Z) - Y - 1)
catch
throw:E -> io:format("ow shit ~p did ~p which I don't understand~n", [?MODULE, E])
end.

handle_call({get,tile,Z,X,Y}, _From, D)->
Reply = case sqlite3:sql_exec(D, lists:concat(["SELECT tile_data FROM tiles WHERE zoom_level = ", Z, " AND tile_column = ", X, " AND tile_row = ", Y])) of
[{columns,["tile_data"]},{rows,[{{blob,Tile}}]}] -> Tile;
[{columns,["tile_data"]},{rows,[]}] -> {noSuchTile, Z, X, Y};
[_] -> throw(noSuchTile)
end,
{reply, Reply, D};
handle_call({get,grid,Z,X,Y}, _From, D)->
Reply = lists:append([fetchGrid(D,Z,X,Y),fetchKey(D,Z,X,Y),[125,125,41,59]]),
{reply, Reply, D};
handle_call({get,_,_,_,_}, _From, D)->
Reply = {noIdea},
{reply, Reply, D};
handle_call({get,info.json}, _From, D)->
Reply = cleanInfo(extractInfo(sqlite3:read_all(D, metadata))),
{reply, Reply, D};
handle_call({get,info.jsonp}, _From, D)->
Reply = pi(cleanInfo(extractInfo(sqlite3:read_all(D, metadata)))),
{reply, Reply, D};
handle_call({get,info}, _From, D)->
Reply = extractInfo(sqlite3:read_all(D, metadata)),
{reply, Reply, D};
handle_call({get,_}, _From, D)->
Reply = {noIdea},
{reply, Reply, D};
handle_call({put,tile, Z, X, Y, Data}, _From, D)->
Reply = sqlite3:write(D, tiles, [{zoom_level, Z},{tile_column,X},{tile_row,Y},{tile_data,{blob,Data}}]),
{reply, Reply, D};
handle_call({put, Name, Value}, _From, D)->
Reply = sqlite3:write(D, metadata,[{name, Name},{value,Value}]),
{reply, Reply, D};
handle_call(stop, _From, D) ->
{stop, normal, stopped, D}.

init(M) ->
case checkMBTILES(M) of
true -> {ok, element(2,sqlite3:start_link(m,[{file, getTilePath(M)}]))};
false -> createMBTILES(M)
end.


getTilePath(M) ->
filename:join([filename:absname(""),"tiles",lists:concat([M, ".mbtiles"])]).

checkMBTILES(M) ->
filelib:is_file(getTilePath(M)).

inflateGrid(G) ->
A = zlib:open(),
zlib:inflateInit(A),
try zlib:inflate(A, G)
after zlib:inflateEnd(A),
zlib:close(A)
end.

fetchGrid(D,Z,X,Y) ->
case sqlite3:sql_exec(D, lists:concat(["SELECT grid FROM grids WHERE zoom_level = ", Z, " AND tile_column = ", X, " AND tile_row = ", Y])) of
[{columns,["grid"]},{rows,[{{blob,Grid}}]}] -> pg(inflateGrid(Grid));
[{columns,["grid"]},{rows,[]}] -> throw(noSuchGrid);
true -> throw(noSuchGrid)
end.

fetchKey(D,Z,X,Y) ->
case sqlite3:sql_exec(D, lists:concat(["select key_name, key_json FROM grid_data WHERE zoom_level = ", Z, " AND tile_column = ", X, " AND tile_row = ", Y])) of
[{columns,["key_name","key_json"]},{rows,Key}] -> pk(Key);
[{columns,["key_name","key_json"]},{rows,[]}] -> throw(noSuchKey);
true -> throw(noSuchKey)
end.

extractInfo(D)->
lists:map(fun({B,C})->{binary_to_list(B),binary_to_list(C)} end,element(2,hd(tl(D)))).

cleanInfo(D)->
%with much thanks to http://stackoverflow.com/questions/3923400/erlang-tuple-list-into-json for the following
StingConverted = [ {X,list_to_binary(Y)} || {X,Y} <- D ],
mochijson2:encode(StingConverted).

pg(G) ->
L = hd(lists:reverse(G)),
R = tl(lists:reverse(G)),
Len = size(L),
C = binary_to_list(L,1,Len-1),
W = lists:append([C,[44,34,100,97,116,97,34,58,123]]),
I = list_to_binary(W),
lists:reverse(lists:append([[I],R,[40,100,105,114,103]])).

pk(K) ->
lists:reverse(tl(lists:reverse(lists:flatten(lists:map(fun({A,B}) -> [34,A,34,58,B,44] end, K))))).

pi(I) ->
lists:append([[103,114,105,100,40],I,[41,59]]).

createMBTILES(M) ->
D = {ok, element(2,sqlite3:start_link(m,[{file, getTilePath(M)}]))},
Db = element(2,D),
sqlite3:create_table(Db, tiles, [{zoom_level, integer},{tile_column, integer},{tile_row, integer},{tile_data, blob}]),
sqlite3:create_table(Db, grids, [{zoom_level, integer},{tile_column, integer},{tile_row, integer},{grid, blob}]),
sqlite3:create_table(Db, grid_data, [{zoom_level, integer},{tile_column, integer},{tile_row, integer},{key_name, text}, {key_json, text}]),
sqlite3:create_table(Db, metadata, [{name, text},{value, text}]),
D.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, State) -> 
sqlite3:close(State),
ok.
code_change(_OldVsn, State, _Extra)->{ok, State}.
