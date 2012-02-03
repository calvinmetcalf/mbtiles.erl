a interface for serving [mbtiles](https://github.com/mapbox/mbtiles-spec) in erlang

you should be able to install with make, i.e.

	make
	
you may have to first do

	sudo apt-get install erlang sqlite3
	
at the moment it assumes that the tiles are in the folder called tiles, there is one in there for testing called gc (it's massachusetts governor's council districts). To run from this directory in the erl prompt.

	mbtiles:start(gc).

to start it then to get a tile

	mbtiles:get(gc,tile,8,77,94).
	
should get <<numbers>> xyz is assumed but to get tms you can do 

	mbtiles:get(gc,tile,tms,8,77,94).

to see it try 

	file:write_file("94.png",mbtiles:get(gc,tile,8,77,94)). 
	
should see a tile. Try

	mbtiles:get(gc,grid,8,77,94).
	
and

	file:write_file("94.grid.json",mbtiles:get(gc,grid,8,77,94)).
	
for grid json 

you can also do 

	mbtiles:get(gc,info). 

change info to info.json or info.jsonp to if you would like one of those format as opposed to an erlang list of tuples

if mbtiles:start(db). is used on a non exsisting set of mbtiles then it will be created, but I'm using a significantly simplified schema with 4 tables tiles, grids, grid_data, and metadata which functionally should be identical. 

also added 
	
	mbtiles:put(db,"name","value").

which will insert name and value into the metadata table, notice the quotations.

	mbtiles:put(db,tile,z,x,y,tiledata).

will incert the binary tiledata into the db flipping the y you can also use

	mbtiles:put(db,tile,{tms|xyz},z,x,y,tiledata).

if you want to specify the scheme. 

putting grids in will come......at some point.
